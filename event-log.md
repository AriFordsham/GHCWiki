## Summary

> [EventLog](event-log) framework exists in the GHC RTS to support profiling of GHC run-time events, such as capability states.  A visualizer is currently under development for viewing the profiling information.

## Design

- [EventLog](event-log) framework is located in ghc/rts/eventlog/
- Library to parse [EventLog](event-log) files for any visualizer, which relies upon the Data.Binary library: GHC/RTS/Events
- Visualizer: under development

## Feature List

- [EventLog](event-log) framework

  - Support for more events, such as cpu and memory usage TODO
- [EventLog](event-log) parser library

  - Parsing all events supported by [EventLog](event-log) framework TODO
- Visualizer 

  - Capability to Thread view TODO
  - Filtering of capabilities and/or threads TODO
  - Querying: find sequences of events, use regular expressions to find events TODO
  - Overlay multiple graphs to display multiple event types (e.g., a graph with thread states, cpu usage, and memory usage)
  - Gracefully handle when a non-[EventLog](event-log) file is loaded by the visualizer (TODO robustness needed)
  - Export graphs in multiple formats: pdf, jpg, grayscale, etc. TODO
  - Link code to events (TODO very difficult, may not happen)

## Code repository

- Available in GHC 6.10.x in ghc_root/rts/eventlog
- [ ghc-events: EventLog binary file parser for profilers](http://code.haskell.org/ghc-events)
- [ ThreadScope: thread-level profiler for GHC EventLog events](http://code.haskell.org/ThreadScope/)

## Contributors

- Satnam Singh
- Simon Marlow
- Donnie Jones \<donnie@…\>
