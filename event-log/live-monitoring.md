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
 |         |                                +--------------------+
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

### 09/07/2014 (Duncan)

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

### 25/07/14 (Peter)

- Stop HECs for sync: Could request a global GC for this? That gives you RTS-wide synchronization for free, and doing a GC is probably (?) cheap enough.
- Random thought: The "tracing" / "eventlog" split feels like a bit of a relic. Maybe we could now replace the tracing back-end that reads the event-log via socket and produces the old tracing messages? Probably out of scope for you, but this is where we might be going at some point.
- enableEvents needs room for arguments as well as "flags". Context (edited for brevity):

  ```wiki
  kvelicka:
  - enableEvents / disableEvents: Don't want to make it more complicated than it needs to be, but for profiling I would have a good couple of different configurations, not even counting configurable sampling delays. Is there a strong reason that we need to make the interface a bit map? Also - wouldn't it be make more sense if the consumer could decide what messages it wants to receive?
  are you suggesting that there's no reason for this currently? or is it something about bit vectors specifically that you don't like?

  petermw:
  Hm, maybe the other way around - what was the intended use for those functions? We are talking essentially about "+RTS -ls", right?
  So the idea here is that this RTS option calls these functions, and then does what?
  Suppresses the messages? Wouldn't it be easier to make sure the messages aren't generated in the first place?
  Bottom line is maybe that I'm a bit confused why this is in the interface to begin with. Only reason I could think of is that you intended to have a central place to change these settings at run-time :)

  kvelicka:
  having everything in one place makes it easier to run the thing, especially if you want to change the event logger settings on the same running program

  petermw:
  https://github.com/scpmw/ghc/blob/profiling-ncg/rts/RtsFlags.c#L1657
  This is what reads these flags currently
  So the idea here is that instead of, say, "RtsFlags.TraceFlags.scheduler = rtsTrue" we compose some sort of bitmap, then pass it to the eventlog, which presumably unpacks it and sets the very same flag? :)
  Well, for my profiling I have options like "-Ey100000"
  Which would activate generation of cycle sampling messages with a period of 100000 cycles
  That's not something that would fit into a simple "on/off" scheme
  It's a flag with arguments essentially. I would like something along the lines of enableEvents(type, param, ...) and disableEvents(type) more
  The "full" parameterisation here would be enableEvents(ET_SAMPLES, SAMPLE_INSTRUCTION_POINTER, SAMPLE_BY_SAMPLES, 100000);
  Because theoretically we might also sample by cost-centres - and I probably wouldn't want another "type" for every single performance metric that perf_events offers
  ```
- Event should hold its parent Capability in itself, having CapEvents is redundant (may be out of scope for my project)
- Headers are not used by users of ghc-events 


Possible solutions to remove EventBlock: 

- Yeah, the easiest implementation would be IORef (Seq CapEvent), or something along those lines
- Maybe IORef \[CapEvent\] is enough as well

## Proposed APIs

### ghc-events


Client API relevant to real-time event monitoring.  The full API is larger; you can find it in the [ ghc-events library documentation](https://hackage.haskell.org/package/ghc-events).

```wiki
-- Datatype that holds a link to the eventlog
data EventHandle -- Abstract

-- Equivalent to the current API:
data EventInfo = {...} 
data ThreadStopStatus = {...} 
data CapsetType = {...} 
newtype KernelThreadId = {...}

type Timestamp = Word64
type ThreadId = Word32
type TaskId = Word64

data Event = Event { ev_time :: Timestamp,
             , ev_cap  :: Maybe Int
             , ev_info :: EventInfo
             } deriving Show

-- Opens the event stream from the specified handle,
-- reads the header info, and initialises the EventHandle
openEventHandle :: Handle -> IO EventHandle

-- Reads one event from the handle (incrementally). Returns Nothing if no events
-- are readable from the log
readEvent :: EventHandle -> IO (Maybe CapEvent)


-- Read/write from/to files
readEventLogFromFile :: FilePath -> IO (Either String [Event])
writeEventLogToFile :: FilePath -> [Event] -> IO ()

-- Pretty printing support
showEventInfo :: EventInfo -> String
showThreadStopStatus :: ThreadStopStatus -> String
ppEventLog :: EventLog -> String
ppEventType :: EventType -> String
ppEvent :: IntMap EventType -> CapEvent -> String

-- Perf events
nEVENT_PERF_NAME :: EventTypeNum
nEVENT_PERF_COUNTER :: EventTypeNum
nEVENT_PERF_TRACEPOINT :: EventTypeNum
sz_perf_num :: EventTypeSize
sz_kernel_tid :: EventTypeSize

```

### RTS


C side:

```
///////////
/* USAGE *////////////
/*
Functions in the API expect to be called in a certain sequence:
1. initEventLogging(); - initialises the system using the default values for all settings
   (see below)
2. Settings functions can be called as per needs of an user
3. sendHeader(); - sends header to the fd. This is necessary for parsing the log
   as ghc-events expects all logs to begin with a header that defines the events
   held in the log
4. startEventLogging(); - starts sending the events to the fd. No settings functions should be
   called when event logging is active
5. stopEventLogging(); - stops sending the events to the fd. This closes all tags,
   making the log "complete". May not be a good idea to restart streaming to the same fd
*///////////////
/* RUNNING *///////////////
// Initialise the event logging system with the default values
// The API uses state to control the default values. More information
// is available in the SETTINGS section.
initEventLogging();// Sends the header file of the eventlog via the fd
sendHeader();//Starts sending the events to the fd
startEventLogging();// Stops sending the events to the fd
stopEventLogging();//////////////
/* SETTINGS *///////////////
// Sets the destination file descriptor that the eventlog will be written to.
// Should only be called when logging is not active.
// Default: a file called <progname>.eventlog
setDestination(fd);// Sets the size of the per-capability eventlog buffers to sz words.
// Should only be called when logging is not active.
// Default: 2,097,152 (2MB)
setBufferSize(int sz);// Enable or disable particular classes of events. Argument is a bit array
// Should only be called when logging is not active
// More info on the classes: 
// http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html
//    Section 4.17.6, the -lclass flag
// The list above may be out of date, refer to $ghc_source/rts/Trace.h for all 
// currently available event classes
// TODO: map bits to event classes
enableEvents(long EventClasses);
disableEvents(long EventClasses);// Set the flush timer for a Capability's buffer. I.e. the buffer will get 
// flushed after ms milliseconds of inactivity
// Should only be called when logging is not active.
// Default: 0, i.e. do not flush buffers automatically
// TODO: no equivalent in current RTS, may need a default value
flushEventLog(int ms);
```


ghc/base side:

```wiki
initEventLogging:: IO()
setDestination:: Fd -> IO()
-- <...> 
-- equivalent to functions on C side
```