There are a few different projects of different skills levels which I (@mpickering) would be willing to help with at Zurihac. The first is continuing the [reimplementation of `hp2pretty`](https://github.com/mpickering/hp2pretty) to work with eventlogs and HTML rendering. The second are improvements on the profiling code of GHC itself. 

`hp2pretty` - Visualising Heap Profiles 
=======================================

[Example](http://mpickering.github.io/vega.html) 

[This repo](https://github.com/mpickering/hp2pretty) is a reimplementation of `hp2pretty`, there is a new frontend which reads eventlogs and a new backend which renders the chart using a [`vega-lite`](https://vega.github.io/vega-lite/)) specification. The resulting chart is interactive.

1. (Beginner) Improve the rendering of the heap profile.
   * Display which Constructor we currently mouse over.
   * Display heap usage of the constructor/total on that position.
   * Maybe we can change the colour of the event log line when emphasising it.
   * Allow selecting a specific part of the profiled to view ([example](https://vega.github.io/vega/examples/overview-plus-detail/))
   * Make it easy to switch between a "zero", ["normalize"](https://vega.github.io/vega-lite/examples/stacked_area_normalize.html) and "[center"](https://vega.github.io/vega-lite/docs/stack.html#streamgraph) stack chart. This is easy to achieve statically but not dynamically using `vega-lite`. ([vega-lite#3338](https://github.com/vega/vega-lite/issues/3338))
   * Add a title and other meta information.

2. (Intermediate) Continue refactoring the internals of hp2pretty. 
   * Honour the `--sort` flag 
   * Honour the `--uniform` flag
   * Honour the `--reverse` flag

3. (Intermediate) Get `hp2pretty` ready for a release.
   * Vendor necessary javascript libraries so there are no runtime dependencies (For example [`js-flot`](https://hackage.haskell.org/packages/search?terms=js-flot)
   * Embed generated JSON into html document.
   * Think of another name for the library

Improvements to GHC profiling (Intermediate/Advanced - knowledge of C)
=====================================================

* Add a cost-centre profiling mode which inserts cost-centres AFTER optimisations have run. (This is mostly already implemented but needs to be tested)([#16765](https://gitlab.haskell.org/ghc/ghc/issues/16765))
* Dynamically adjust sampling interval in proportion to time taken to GC ([#1420](https://gitlab.haskell.org/ghc/ghc/issues/1420))
* Add eventlog events for biographical and retainer profilers ([#16766](https://gitlab.haskell.org/ghc/ghc/issues/16766))
* Add profiling support to [`ghc-heap`](https://www.stackage.org/haddock/lts-13.24/ghc-heap-8.6.5/GHC-Exts-Heap.html)([#15375](https://gitlab.haskell.org/ghc/ghc/issues/15735))
* LDV Profiling: Make heapCensus agree with LdcCensusForDead (#16807)
* (Advanced) Give more detailed information about PINNED data in a heap profile
 ([#7275](https://gitlab.haskell.org/ghc/ghc/issues/7275))