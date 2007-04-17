# Data Parallel Haskell


This page documents the integration of nested data parallelism into GHC at the developer level, including notes about where we are and what needs doing.  See also the [ user-level wiki page](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell), which includes examples and tutorial-style instructions.


Most of the material describing our approach is partitioned into a set of subpages:

- [Nested data parallelism by example](data-parallel/example)
- [Data parallelism on shared-memory machines](data-parallel/smp)
- [High-level design of adding NDP to GHC](data-parallel/design)
- [Details of the implementation of closure conversion (as a smaller-scale trial of what expects us with implementing vectorisation)](data-parallel/closure-conversion)
- [Desugaring of array comprehensions](data-parallel/desugaring)
- [Other nested data parallel work](data-parallel/related)

## Status and work plan


Detailed information on how to use the current implementation is at the [ user-level wiki page](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell).  Here is information on the implementation status and outstanding work items:

- DPH [repositories](data-parallel/repositories)
- Our [work plan](data-parallel/work-plan)
- Some [benchmarks](data-parallel/benchmarks)

## Old material


Pages that have fallen out of use (and contain out dated information):

- [Optimisation, and problems therewith](data-parallel/optimisation)