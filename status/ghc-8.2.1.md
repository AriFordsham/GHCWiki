# GHC plans for 8.2.1


This page is our road-map for what will be in 8.2.  


We expect GHC 8.2 to be principally a consolidation release, in which we settle down and flesh out existing features, rather than introduce major new features.  In particular, we would like to work on performance, especially of the compiler itself.

- We hope to incorporate all the "Landed" and "In-flight" stuff under "Release highlights" below.

- We'll include (or at least review) all patches in tickets in "Status: patch" below.

- We will address all the tickets under "Status: new" below with "highest" or "high" priority.  We love help to do more, but there are far too many "normal" tickets to make any promises.


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Very tentative: Release candidate by **mid-February 2017**. Release in **mid-April 2017**.

## Libraries Status


See [Libraries](status/ghc-8.2.1/libraries)

## Release highlights


Below are the major highlights of 8.2.

- **Indexed `Typeable` representations**[Typeable/BenGamari](typeable/ben-gamari) (Ben Gamari, Simon Peyton Jones, et al). While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

>
> GHC 8.2 will address this by introducing indexed type representations, leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [ paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/) for an description of the proposal and the [ Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari) for the current status of the implementation.

- Backpack is targeting to be merged in GHC 8.2. More to come here. (Edward Z Yang)
- Merge `Bifoldable` and `Bitraversable` into `base`, addressing [\#10448](https://gitlab.haskell.org//ghc/ghc/issues/10448) (Edward Kmett, Ryan Scott)
- Generalize the `deriving` algorithms for `Eq`, `Functor`, etc. to be able to derive the data types in `Data.Functor.Classes` (`Eq1`, `Eq2`, etc.), `Bifunctor`, `Bifoldable`, and `Bitraversable` (Ryan Scott)
- Deriving strategies (Ryan Scott): grant users the ability to choose explicitly how a class should be `derived` (using a built-in algorithm, `GeneralizedNewtypeDeriving`, `DeriveAnyClass`, or otherwise), addressing [\#10598](https://gitlab.haskell.org//ghc/ghc/issues/10598).
- Exhaustiveness checking for `EmptyCase`s ([ Phab:D2105](https://phabricator.haskell.org/D2105)), addressing [\#10746](https://gitlab.haskell.org//ghc/ghc/issues/10746). 

### Back-end and runtime system

- Compact regions (Giovanni Campagna, Edward Yang, [ Phab:D1264](https://phabricator.haskell.org/D1264), [ paper](http://ezyang.com/papers/ezyang15-cnf.pdf)). This runtime system feature allows a referentially "closed" set of heap objects to be collected into a "compact region", allowing cheaper garbage collection, heap-object sharing between processes, and the possibility of inexpensive serialization.

- Refactoring and improvements to the cost-center profiler (Ben Gamari, [ Phab:D1722](https://phabricator.haskell.org/D1722)): Allow heap profiler samples to be directed to the GHC eventlog, allowing correlation with other program events, enabling easier analysis by tooling and eventual removal of the old, rather crufty `.hp` profile format. 

- Further improvements to debugging information (Ben Gamari): There are still a number of outstanding issues with GHC's DWARF implementation, some of which even carry the potential to crash the runtime system during stacktrace collection. GHC 8.2 will hopefully have these issues resolved, allowing debugging information to be used by end-user code in production.

>
> With stable stack unwinding support comes a number of opportunities for new serial and parallel performance analysis tools (e.g. statistical profiling) and debugging. As GHC's debugging information improves, we expect to see tooling developed to support these applications. See the [ DWARF status page](https://ghc.haskell.org/trac/ghc/wiki/DWARF/80Status) for futher information.

- Support for NUMA systems (Simon Marlow, [ in-progress](https://github.com/simonmar/ghc/tree/numa)).  The aim is to reduce the number of remote memory accesses for multi-socket systems that have a mixture of local and remote memory.

- Experimental changes to the scheduler (Simon Marlow, [ in progress](https://github.com/simonmar/ghc/commit/7e05ec18b4eda8d97e37015d415e627353de6b50)) that enable the number of threads used for GC to be lower than the `-N` setting.

### Frontend, build system and miscellaneous changes

- New Shake-based build system, `hadrian`, will be merged.  (Andrey Mokhov)
- The [improved LLVM backend plan](improved-llvm-backend) plan didn't make the cut for 8.0, but will for 8.2 (Austin Seipp)
- Deterministic builds [DeterministicBuilds](deterministic-builds). Given the same environment, file and flags produce ABI compatible binaries. (Bartosz Nitka, in-progress)

### Landed in `master` branch

- TODO

## Tickets marked merge with no milestone

<table><tr><th>Ticket (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

## Tickets slated for 8.2.1

### merge/patch/upstream

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.2.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

### new

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

### infoneeded

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.2, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>