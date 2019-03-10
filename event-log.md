## Summary


EventLog is a fast, extensible event logging framework in the GHC run-time system (RTS) to support profiling of GHC run-time events. The [GHC User's Guide](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/runtime-control.html#rts-eventlog) describes how to enable event logging, after the program is linked with `eventlog`

```wiki
+RTS -l
```


Log events in binary format to the file program.eventlog, where flags is a sequence of zero or more characters indicating which kinds of events to log. Currently there is only one type supported: -ls, for scheduler events.


The format of the log file is described by the header EventLogFormat.h that comes with GHC, and it can be parsed in Haskell using the [ ghc-events](http://hackage.haskell.org/package/ghc-events) library. To dump the contents of a .eventlog file as text, use the tool show-ghc-events that comes with the ghc-events package.

## Design

- EventLog framework is located in ghc_root/rts/eventlog/
- A [ ghc-events](http://hackage.haskell.org/package/ghc-events) library to parse EventLog files for any visualizer, which relies upon the Data.Binary library.
- Visualizer: [ ThreadScope](http://raintown.org/?page_id=132)

## Code repository

- Available in GHC 6.10.x in ghc_root/rts/eventlog
- [ ghc-events: EventLog binary file parser for profilers](http://code.haskell.org/ghc-events)
- [ ThreadScope: thread-level profiler for GHC EventLog events](http://code.haskell.org/ThreadScope/)

## Publications

- [ Parallel Performance Tuning for Haskell (Don Jones Jr., Simon Marlow, Satnam Singh) Haskell '09: Proceedings of the second ACM SIGPLAN symposium on Haskell, Edinburgh, Scotland, ACM, 2009](http://www.haskell.org/~simonmar/bib/threadscope-09_abstract.html)

## Contributors

- Satnam Singh
- Simon Marlow
- Donnie Jones \<donnie@…\>
