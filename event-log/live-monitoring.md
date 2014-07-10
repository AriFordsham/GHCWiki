# Live profiling via RTS and ghc-events project


This wiki page is for the project by Karolis Velicka \<karolis.velicka at google's email service\> that he is working on in Microsoft Research in the summer of 2014.


This wiki page documents my current understanding of the system, plans for improving it, progress reports and assorted notes from meting that are relevant to the topic. 


In case you find some flaws in my understanding, something that you disagree with or statements that are outright wrong, please do contact me. Any other contributions/advice/edits are very welcome!

## Goals

### Initial Goals


As set out by [ Duncan Coutts](http://www.well-typed.com/people/duncan):


In the RTS:

- Allow turning event logging on/off as a whole at runtime.
- Allow turning on/off individual classes of events at runtime.
- Allow redirecting the eventlog output from a file to an arbitrary FD. (Interesting policy Q: what to do when sending out the eventlog blocks...)
- Allow emitting "synchronisation" events, mainly to be used when switching eventlog on/off or redirecting the output to a new sink, or just from time to time to let monitoring apps get a full tracking state. The synchronisation events would be basically the same ones as are emitted at startup, including process info, declaration of capsets, threads etc. (May require new events to distinguish the new "I'm reminding you that this thread exists" from the existing events like "I'm now creating this new thread". Or we may be able to have a semantics where a second "I'm creating thread 42" event is treated as a sync event and can be ignored.)
- Allow active flushing of the eventlog buffers. Normally the eventlog data is only flushed when a per-cap buffer is full. This can be a long time if one cap is very active while another is not. Software being monitored may wish to ensure that the latest data is sent out every few sec, even if the buffers are not full.
- Alternatively to the above, or as well, allow setting the per-cap eventlog buffer size.
- Haskell APIs for all the RTS features above.


In ghc-events library:

- redo the binary parser to be incremental both on input and output: that is allow supplying input chunk by chunk, and getting parsed events out for the data already supplied. This should be doable using the current version of the binary library.


Basic demo of live monitoring using above new features:

- demo CLI 'monitored' prog that uses the RTS APIs to direct its eventlog to a local FIFO, emit the various sync events. Perhaps interactive to exercise turning the eventlog on/off, enabling/disabling various event classes.
- demo CLI 'monitoring' prog that uses the new ghc-events lib to start reading and decoding the event stream.
- Proof-of-concept EKG-like functionality would be to emit mem/GC summary stats lines every second.

### Goals that have been added later

- Move the link between RTS and ghc-events into a separate library where both a client-server (see below) and file based implementations of the "link" could be stored.

## Architecture

```wiki
 +---------+                                +--------------------+
 |         |                                |                    |
 | Program |                                |      Consumer      |
 |         |                                | (e.g. Threadscope) |
 +---------+                                |                    |
 |         |                                +-------+-+----------+
 |   RTS   |                                |    ghc-events      |
 |         |                                |                    |
 +----+----+                                |                    |
      |                                     +--------------------+
      |         +----------------------+                          
      |         |                      |              ^           
      +-------> |  Middleware library  |              |           
                |  (working title Foo) +--------------+           
                |                      |                          
                +----------------------+                          

```


Foo is an abstraction / library (or libraries) that will use the new API of RTS to connect with ghc-events. It could happen via a file, socket or any other means, depending on the implementation.

## Notes from meetings

### Initial

- In current state, RTS writes to the eventlog asynchronously.
- Need a new module in RTS for start/stop event logging
- RTS and GHC.RTS talk together via foreign ccalls
- Start/stop of logging would need to stop the HECs for sync. Need to measure the performance of this
- During this we need to traverse threads in struct generation_, they are in global variable called generations. Also look into StgTSO (thread structures)
- Flushing only needs to flush inactive buffers (that are also not empty)
- Possibly add some GC events (some are currently in tracegc at the moment):

  - heap size
  - heap live
- RTS broadcasts on demand once a client connects
- Sockets seem to be appropriate for live monitoring 

### 09/07/2014

- Start/stop of streaming is different from the "sync events" mentioned in goals
- The details for the middleware library (Foo) are not that relevant now, focus should be on RTS level
- Available event classes are in Trace.h
- cassava CSV library is a good example that uses an incremental parser
- ErrorT may no longer be necessary (at least in the case of ErrorT String)
- Need info on events w.r.t. blocks and capabilities for the purpose of sorting  
- Eventlogs are comprised of event blocks that belong to a certain capability plus some global events
- Look at Trace.{h, c}
- Trace has two levels:

  - "Tracing" - more general, prints to stderr, used for debugging
  - "Event" - lives in RTS/Eventlog, used for writing \*.eventlog files
- Need to read/understand:

  - includes/rts/EventLogFormat.h
  - [EventLog](event-log).{h, c}

## Discussed APIs


Here we have possible API implementations. Keep in mind that all this is a work in progress 

### ghc-events

```wiki
-- high level lazy API (based on chunked input internally)
streamEvents :: ByteString.Lazy -> [CapEvent]

-----------------------------------------------------------------------------

-- | An incremental decoder for a single item. It accepts input incrementally
-- and produces a single result at the end.
data Decoder a =
      -- | The input data was malformed. The first field contains any
      -- unconsumed input and third field contains information about
      -- the parse error.
      Fail !B.ByteString !ByteOffset String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. If fed an 'B.empty string', the
      -- continuation is guaranteed to return either 'FailH' or
      -- 'DoneH'.
    | Partial (B.ByteString -> Decoder a)

      -- | The parse succeeded and produced the given 'Header'.
    | Done !B.ByteString !ByteOffset a


-- | An incremental decoder for a sequence. It accepts input incrementally
-- and additionally it can produce sequence results incrementally.
data SequenceDecoder a =
      -- | The input data was malformed. The first field contains any
      -- unconsumed input and third field contains information about
      -- the parse error.
      FailS !B.ByteString !ByteOffset String

      -- | The decoder read zero or more records. Feed a 'B.ByteString' to
      -- the continuation to continue parsing. Use an 'B.empty' string to
      -- indicate that no more input data is available. If fed an 'B.empty'
      -- string, the continuation is guaranteed to return either 'Fail'
      -- or 'Done'.
    | Many [a] (B.ByteString -> SequenceDecoder a)

      -- | The decoder read zero or more records. This is the end of
      -- the sequence.
    | DoneS !B.ByteString !ByteOffset [a]
    deriving Functor

eventlogDecoder :: Decoder (SequenceDecoder Event)

```

### RTS


C side:

```wiki
// Initialise the event logging system
initEventLogging();


/* Following only work when events are not being transmitted */
setDestination(fd);
setBufferSize(int sz);
sendHeader();
startStreaming();
enableEvents(EventClasses);
disableEvents(EventClasses);

stopEventLogging();

// flush the buffer for a Capability after ms milliseconds of inactivity
flushEventLog(int ms);


```


ghc/base side:

```wiki
initEventLogging:: IO()
setDestination:: Fd -> IO()
-- <...> 
-- equivalent to functions on C side
```

## Misc/Old


example log servers


C:

```wiki
logserver.c
  s = socket()
  listen(s)
  t = accept(s)
  initEventLogging(t)
```


Haskell:

```wiki
logserver.hs
  s <- socket
  listen
  t <- accept s
  startEventLogging t
```