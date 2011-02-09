## Summary

>
> EventLog is a fast, extensible event logging framework in the GHC run-time system (RTS) to support profiling of GHC run-time events. 

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
