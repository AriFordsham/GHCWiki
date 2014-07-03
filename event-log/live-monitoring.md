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

## Notes from meetings


Some are merged with other points and therefore are removed from this list

- In current state, RTS writes to the eventlog asynchronously.
- Need a new module in RTS for start/stop event logging
- RTS and GHC.RTS talk together via foreign ccalls
- Start/stop of logging would need to stop the HECs for sync. Need to measure the performance of this
- During this we need to traverse threads in struct generation_, they are in global variable called generations. Also look into StgTSO (thread structures)
- Flushing only needs to flush inactive buffers (that are also not empty)
- Possibly add some GC events (some are currently in tracegc at the moment):

  - heap size
  - heap live
- High level API for time sorted lazy list of events


Q&A from the meeting

- Does RTS broadcast all the time or on demand?

  - On demand, after a client connects
- What would the connection be for live eventlog

  - Sockets seem to be appropriate for the job
- Which tasks are priorities?

  - Sliped my mind. TODO

## Discussed APIs

### ghc-events

```wiki
-- high level lazy API (based on chunked input underneath)
streamEvents :: ByteString.Lazy -> [CapEvent]

-- returns a list of fully parsed events ant the remaining ByteString
parse :: ByteString -> ([Event], ByteString)
-- alternatively parse one event at a time and return the rest
parse :: ByteString -> (Event, ByteString)

-- a parser instance that may be used
Parser (Event, Bytestring)
-- could be created by
p = do
  e <- parseEvent
  b <- takeRemainingInput
  return (e,b)


```

### RTS


C side:

```wiki
// idea is to use a generic fd so that both file-based and 
// socket-based connection would be available
initEventLogging(fd);

// flush the buffer for a Capability after ms milliseconds of inactivity
flushEventLog(int ms);

stopEventLogging();

// Current idea is to use bit fields for this
enableEvents(EventClasses);
disableEvents(EventClasses);
```


ghc/base side:

```wiki
startEventLogging :: Fd -> IO()
stopEventLogging  :: IO()
-- <...> 
-- equivalent to functions on C side
```

## Misc


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