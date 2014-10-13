# GHC plans for 7.10.1


We've written up some tentative [ plans for the 7.10.1 release](https://haskell.org/pipermail/ghc-devs/2014-October/006518.html). Here's a short recap:

- We're planning to freeze sometime in mid November, most likely.
- We'd like a 3-4 month stable freeze (from Nov-Feb)
- We'd like a release in **Feb 2015**, which is our goal.
- We haven't planned an RC yet, but hope one **might be out by Christmas**.

- [ Phab:D168](https://phabricator.haskell.org/D168): Partial type signatures
- [ Phab:D72](https://phabricator.haskell.org/D72): New rebindable syntax for arrows.
- [ Phab:D155](https://phabricator.haskell.org/D155): LLVM 3.5 compatibility
- [ Phab:D169](https://phabricator.haskell.org/D169): Source code note infrastructure
- [ Phab:D202](https://phabricator.haskell.org/D202): Injective type families
- [ Phab:D270](https://phabricator.haskell.org/D270) through [ Phab:D293](https://phabricator.haskell.org/D293): Edward Yang's HEAP_ALLOCED saga
- [ Phab:D130](https://phabricator.haskell.org/D130): Implementation of hsig (module signatures)


See [milestone:7.10.1](/trac/ghc/milestone/7.10.1) and [ Active tickets](https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&group=status&milestone=7.10.1) for more.

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

## Tickets slated for 7.8.4

## Status: closed (341 matches)

<table><tr><th>Ticket (Ticket query: milestone: 7.10.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: milestone: 7.10.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: milestone: 7.10.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: milestone: 7.10.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: milestone: 7.10.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#703](https://gitlab.haskell.org//ghc/ghc/issues/703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[all binaries built by ghc have executable stacks](https://gitlab.haskell.org//ghc/ghc/issues/703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#1473](https://gitlab.haskell.org//ghc/ghc/issues/1473)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[isSpace is too slow](https://gitlab.haskell.org//ghc/ghc/issues/1473)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#1476](https://gitlab.haskell.org//ghc/ghc/issues/1476)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Template Haskell: splicing types and patterns](https://gitlab.haskell.org//ghc/ghc/issues/1476)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#2104](https://gitlab.haskell.org//ghc/ghc/issues/2104)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add Labels](https://gitlab.haskell.org//ghc/ghc/issues/2104)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Rules to eliminate casted id's](https://gitlab.haskell.org//ghc/ghc/issues/2110)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#2182](https://gitlab.haskell.org//ghc/ghc/issues/2182)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc sessions (--make, --interactive, ghc api) erroneously retain instances](https://gitlab.haskell.org//ghc/ghc/issues/2182)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#2526](https://gitlab.haskell.org//ghc/ghc/issues/2526)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Warn about missing signatures only for exported functions](https://gitlab.haskell.org//ghc/ghc/issues/2526)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>gridaphobe</th></tr>
<tr><th>[\#2528](https://gitlab.haskell.org//ghc/ghc/issues/2528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[documentation for nub and  nubBy should be corrected, extended or removed.](https://gitlab.haskell.org//ghc/ghc/issues/2528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#2836](https://gitlab.haskell.org//ghc/ghc/issues/2836)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Data.Typeable does not use qualified names](https://gitlab.haskell.org//ghc/ghc/issues/2836)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#3003](https://gitlab.haskell.org//ghc/ghc/issues/3003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Happy does not reject pragmas](https://gitlab.haskell.org//ghc/ghc/issues/3003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Reorder tests in quot to improve code](https://gitlab.haskell.org//ghc/ghc/issues/3065)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#3191](https://gitlab.haskell.org//ghc/ghc/issues/3191)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hpc reports spurious results with .lhs files even after processing with ghc -E](https://gitlab.haskell.org//ghc/ghc/issues/3191)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>andy@…</th></tr>
<tr><th>[\#3376](https://gitlab.haskell.org//ghc/ghc/issues/3376)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hpc and CPP don't mix on Windows](https://gitlab.haskell.org//ghc/ghc/issues/3376)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>andy@…</th></tr>
<tr><th>[\#3470](https://gitlab.haskell.org//ghc/ghc/issues/3470)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[OSX installer should give an informative error message when XCode is missing](https://gitlab.haskell.org//ghc/ghc/issues/3470)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3509](https://gitlab.haskell.org//ghc/ghc/issues/3509)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[libffi.so not found on Mac OS X (10.5.8)](https://gitlab.haskell.org//ghc/ghc/issues/3509)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3543](https://gitlab.haskell.org//ghc/ghc/issues/3543)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-fext-core doesn't force recompilation when .hcr file doesn't exist](https://gitlab.haskell.org//ghc/ghc/issues/3543)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3625](https://gitlab.haskell.org//ghc/ghc/issues/3625)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCI doesn't work with dtrace on OS X](https://gitlab.haskell.org//ghc/ghc/issues/3625)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Dynamically link GHCi (and use system linker) on platforms that support it](https://gitlab.haskell.org//ghc/ghc/issues/3658)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#3739](https://gitlab.haskell.org//ghc/ghc/issues/3739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-cabal mishandles relative paths in arguments](https://gitlab.haskell.org//ghc/ghc/issues/3739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#3771](https://gitlab.haskell.org//ghc/ghc/issues/3771)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[haddock: internal error: evacuate: strange closure type 19269](https://gitlab.haskell.org//ghc/ghc/issues/3771)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3859](https://gitlab.haskell.org//ghc/ghc/issues/3859)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Problems with toClockTime on NetBSD](https://gitlab.haskell.org//ghc/ghc/issues/3859)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#4218](https://gitlab.haskell.org//ghc/ghc/issues/4218)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[System.Random is way too lazy](https://gitlab.haskell.org//ghc/ghc/issues/4218)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#4347](https://gitlab.haskell.org//ghc/ghc/issues/4347)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Bug in unification of polymorphic and not-yet-polymorphic type](https://gitlab.haskell.org//ghc/ghc/issues/4347)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#4366](https://gitlab.haskell.org//ghc/ghc/issues/4366)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[in-tree GMP build problem with shared libraries](https://gitlab.haskell.org//ghc/ghc/issues/4366)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#4834](https://gitlab.haskell.org//ghc/ghc/issues/4834)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement Functor =\> Applicative =\> Monad Hierarchy (aka AMP phase 3)](https://gitlab.haskell.org//ghc/ghc/issues/4834)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#4880](https://gitlab.haskell.org//ghc/ghc/issues/4880)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Functor, Monad, Applicative instances for Data.Monoid.First, Data.Monoid.Last](https://gitlab.haskell.org//ghc/ghc/issues/4880)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#4896](https://gitlab.haskell.org//ghc/ghc/issues/4896)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Deriving Data does not work for attached code](https://gitlab.haskell.org//ghc/ghc/issues/4896)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#4921](https://gitlab.haskell.org//ghc/ghc/issues/4921)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[report ambiguous type variables more consistently](https://gitlab.haskell.org//ghc/ghc/issues/4921)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#4934](https://gitlab.haskell.org//ghc/ghc/issues/4934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[threadWaitRead works incorrectly on nonthreaded RTS](https://gitlab.haskell.org//ghc/ghc/issues/4934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5190](https://gitlab.haskell.org//ghc/ghc/issues/5190)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TinyCoreLinux extension](https://gitlab.haskell.org//ghc/ghc/issues/5190)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#5248](https://gitlab.haskell.org//ghc/ghc/issues/5248)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Infer type context in a type signature](https://gitlab.haskell.org//ghc/ghc/issues/5248)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#5280](https://gitlab.haskell.org//ghc/ghc/issues/5280)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[System.Random commits (rand \`mod\` base) error.](https://gitlab.haskell.org//ghc/ghc/issues/5280)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rrnewton</th></tr>
<tr><th>[\#5364](https://gitlab.haskell.org//ghc/ghc/issues/5364)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Access RTS flag values from inside Haskell programs](https://gitlab.haskell.org//ghc/ghc/issues/5364)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#5442](https://gitlab.haskell.org//ghc/ghc/issues/5442)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-pkg unregister --user/--global/--package-conf not normative](https://gitlab.haskell.org//ghc/ghc/issues/5442)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#5443](https://gitlab.haskell.org//ghc/ghc/issues/5443)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Errors when shutting down the event manager loop](https://gitlab.haskell.org//ghc/ghc/issues/5443)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>tibbe</th></tr>
<tr><th>[\#5462](https://gitlab.haskell.org//ghc/ghc/issues/5462)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Deriving clause for arbitrary classes](https://gitlab.haskell.org//ghc/ghc/issues/5462)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dreixel</th></tr>
<tr><th>[\#5608](https://gitlab.haskell.org//ghc/ghc/issues/5608)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Fix T3807 for Mac OS X 10.5](https://gitlab.haskell.org//ghc/ghc/issues/5608)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#5647](https://gitlab.haskell.org//ghc/ghc/issues/5647)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[CLI option to silence "Loading package foo ... linking ... done" output](https://gitlab.haskell.org//ghc/ghc/issues/5647)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5763](https://gitlab.haskell.org//ghc/ghc/issues/5763)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Confusing error message](https://gitlab.haskell.org//ghc/ghc/issues/5763)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#5821](https://gitlab.haskell.org//ghc/ghc/issues/5821)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[SPECIALISE fails with a cryptic warning](https://gitlab.haskell.org//ghc/ghc/issues/5821)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#5844](https://gitlab.haskell.org//ghc/ghc/issues/5844)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic on generating Core code](https://gitlab.haskell.org//ghc/ghc/issues/5844)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5898](https://gitlab.haskell.org//ghc/ghc/issues/5898)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc: internal error: Invalid Mach-O file](https://gitlab.haskell.org//ghc/ghc/issues/5898)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5907](https://gitlab.haskell.org//ghc/ghc/issues/5907)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Crashed loading package Safe](https://gitlab.haskell.org//ghc/ghc/issues/5907)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5924](https://gitlab.haskell.org//ghc/ghc/issues/5924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Bad Cmm generated for updating one element in Array\#](https://gitlab.haskell.org//ghc/ghc/issues/5924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5942](https://gitlab.haskell.org//ghc/ghc/issues/5942)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[implement POSIX confstr() in System/Posix/Unistd.hsc](https://gitlab.haskell.org//ghc/ghc/issues/5942)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#6016](https://gitlab.haskell.org//ghc/ghc/issues/6016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[On Windows, runhaskell hits an error on UTF-8 files with a BOM](https://gitlab.haskell.org//ghc/ghc/issues/6016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#6022](https://gitlab.haskell.org//ghc/ghc/issues/6022)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC infers over-general types](https://gitlab.haskell.org//ghc/ghc/issues/6022)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#6056](https://gitlab.haskell.org//ghc/ghc/issues/6056)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[INLINABLE pragma prevents worker-wrapper to happen.](https://gitlab.haskell.org//ghc/ghc/issues/6056)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#6149](https://gitlab.haskell.org//ghc/ghc/issues/6149)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.4.2 tests for profasm seg-fault under solaris](https://gitlab.haskell.org//ghc/ghc/issues/6149)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7011](https://gitlab.haskell.org//ghc/ghc/issues/7011)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[32bit GHC 7.4.2 cannot compile integer-gmp on OS X 10.8](https://gitlab.haskell.org//ghc/ghc/issues/7011)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7028](https://gitlab.haskell.org//ghc/ghc/issues/7028)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[incorrect link paths for in mac os x after install](https://gitlab.haskell.org//ghc/ghc/issues/7028)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>leroux</th></tr>
<tr><th>[\#7056](https://gitlab.haskell.org//ghc/ghc/issues/7056)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Relax static-PE linker object checks](https://gitlab.haskell.org//ghc/ghc/issues/7056)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#7103](https://gitlab.haskell.org//ghc/ghc/issues/7103)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Compiler panic, when loading wxc in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/7103)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7189](https://gitlab.haskell.org//ghc/ghc/issues/7189)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[RTS Assertion Crash](https://gitlab.haskell.org//ghc/ghc/issues/7189)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#7200](https://gitlab.haskell.org//ghc/ghc/issues/7200)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[template-haskell-2.7.0.0 fails to build with GHC 7.0.4 due to missing pragma](https://gitlab.haskell.org//ghc/ghc/issues/7200)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>duncan</th></tr>
<tr><th>[\#7243](https://gitlab.haskell.org//ghc/ghc/issues/7243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[regression: acceptable foreign result types](https://gitlab.haskell.org//ghc/ghc/issues/7243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7379](https://gitlab.haskell.org//ghc/ghc/issues/7379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[rangeTest test fails on Windows](https://gitlab.haskell.org//ghc/ghc/issues/7379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7452](https://gitlab.haskell.org//ghc/ghc/issues/7452)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\[GNU gold\] ld: error: cannot find \[...\]/Types__1.o](https://gitlab.haskell.org//ghc/ghc/issues/7452)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7460](https://gitlab.haskell.org//ghc/ghc/issues/7460)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Double literals generated bad core](https://gitlab.haskell.org//ghc/ghc/issues/7460)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>tibbe</th></tr>
<tr><th>[\#7473](https://gitlab.haskell.org//ghc/ghc/issues/7473)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[getModificationTime gives only second-level resolution](https://gitlab.haskell.org//ghc/ghc/issues/7473)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#7484](https://gitlab.haskell.org//ghc/ghc/issues/7484)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Template Haskell allows building invalid record fields/names](https://gitlab.haskell.org//ghc/ghc/issues/7484)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7490](https://gitlab.haskell.org//ghc/ghc/issues/7490)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-stage1 panic when building a cross-compiler or cross-building a compiler](https://gitlab.haskell.org//ghc/ghc/issues/7490)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7505](https://gitlab.haskell.org//ghc/ghc/issues/7505)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Commentary shipped with GHC sources is outdated](https://gitlab.haskell.org//ghc/ghc/issues/7505)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7544](https://gitlab.haskell.org//ghc/ghc/issues/7544)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[GHC downloads are unsigned](https://gitlab.haskell.org//ghc/ghc/issues/7544)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7604](https://gitlab.haskell.org//ghc/ghc/issues/7604)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[System.Directory.canonicalizePath "" behaviour differs between platforms](https://gitlab.haskell.org//ghc/ghc/issues/7604)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#7623](https://gitlab.haskell.org//ghc/ghc/issues/7623)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[GHC Arm support](https://gitlab.haskell.org//ghc/ghc/issues/7623)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7643](https://gitlab.haskell.org//ghc/ghc/issues/7643)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Kind application error](https://gitlab.haskell.org//ghc/ghc/issues/7643)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#7673](https://gitlab.haskell.org//ghc/ghc/issues/7673)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Windows: run "git config --global core.autocrlf false" before cloning the repo](https://gitlab.haskell.org//ghc/ghc/issues/7673)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#7727](https://gitlab.haskell.org//ghc/ghc/issues/7727)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Nonsense evaluation: sequence \[\] = \[\]](https://gitlab.haskell.org//ghc/ghc/issues/7727)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7730](https://gitlab.haskell.org//ghc/ghc/issues/7730)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[:info and polykinds](https://gitlab.haskell.org//ghc/ghc/issues/7730)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>archblob</th></tr>
<tr><th>[\#7735](https://gitlab.haskell.org//ghc/ghc/issues/7735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-fext-core doesn't generate .hcr when .o and .hi files are present](https://gitlab.haskell.org//ghc/ghc/issues/7735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7736](https://gitlab.haskell.org//ghc/ghc/issues/7736)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Parallel array enumeration causes compiler panic (enumFromToP)](https://gitlab.haskell.org//ghc/ghc/issues/7736)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>chak</th></tr>
<tr><th>[\#7767](https://gitlab.haskell.org//ghc/ghc/issues/7767)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>["internal error: evacuate: strange closure type 154886248" crash](https://gitlab.haskell.org//ghc/ghc/issues/7767)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Fix definitions of abs/signum for Floats/Doubles](https://gitlab.haskell.org//ghc/ghc/issues/7858)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>bernalex</th></tr>
<tr><th>[\#7863](https://gitlab.haskell.org//ghc/ghc/issues/7863)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Verbosity level for quieter Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/7863)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>hvr</th></tr>
<tr><th>[\#7913](https://gitlab.haskell.org//ghc/ghc/issues/7913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Argument order not preserved by nubBy](https://gitlab.haskell.org//ghc/ghc/issues/7913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7936](https://gitlab.haskell.org//ghc/ghc/issues/7936)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[newStdGen leaks memory when result is not used](https://gitlab.haskell.org//ghc/ghc/issues/7936)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#7942](https://gitlab.haskell.org//ghc/ghc/issues/7942)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[aarch64 support in ghc](https://gitlab.haskell.org//ghc/ghc/issues/7942)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#7955](https://gitlab.haskell.org//ghc/ghc/issues/7955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[CApiFFI doesn't produce wrappers for \#defined values in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/7955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7984](https://gitlab.haskell.org//ghc/ghc/issues/7984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hsc2hs --cross-compile does not handle negative \#enum](https://gitlab.haskell.org//ghc/ghc/issues/7984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7987](https://gitlab.haskell.org//ghc/ghc/issues/7987)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC Build Error](https://gitlab.haskell.org//ghc/ghc/issues/7987)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8013](https://gitlab.haskell.org//ghc/ghc/issues/8013)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Strange closure type error building hs-kqueue on FreeBSD](https://gitlab.haskell.org//ghc/ghc/issues/8013)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8044](https://gitlab.haskell.org//ghc/ghc/issues/8044)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["Inaccessible code" error reported in wrong place](https://gitlab.haskell.org//ghc/ghc/issues/8044)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8089](https://gitlab.haskell.org//ghc/ghc/issues/8089)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Implementation of GHC.Event.Poll.poll is broken due to bad coercion](https://gitlab.haskell.org//ghc/ghc/issues/8089)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8093](https://gitlab.haskell.org//ghc/ghc/issues/8093)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Runtime Internal Error with setNumCapabilities 1](https://gitlab.haskell.org//ghc/ghc/issues/8093)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8097](https://gitlab.haskell.org//ghc/ghc/issues/8097)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[internal error: getMBlock: mmap: Operation not permitted](https://gitlab.haskell.org//ghc/ghc/issues/8097)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8174](https://gitlab.haskell.org//ghc/ghc/issues/8174)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC should not load packages for TH if they are not used](https://gitlab.haskell.org//ghc/ghc/issues/8174)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8192](https://gitlab.haskell.org//ghc/ghc/issues/8192)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Finally remove -fwarn-lazy-unlifted-bindings flag](https://gitlab.haskell.org//ghc/ghc/issues/8192)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8331](https://gitlab.haskell.org//ghc/ghc/issues/8331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC fails to apply {-\# SPECIALIZE \#-} for dubious reasons](https://gitlab.haskell.org//ghc/ghc/issues/8331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8345](https://gitlab.haskell.org//ghc/ghc/issues/8345)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[A more efficient atomicModifyIORef'](https://gitlab.haskell.org//ghc/ghc/issues/8345)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8374](https://gitlab.haskell.org//ghc/ghc/issues/8374)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\`tcIfaceGlobal (local): not found\` while compiling](https://gitlab.haskell.org//ghc/ghc/issues/8374)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8407](https://gitlab.haskell.org//ghc/ghc/issues/8407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Module re-exports at the package level](https://gitlab.haskell.org//ghc/ghc/issues/8407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#8427](https://gitlab.haskell.org//ghc/ghc/issues/8427)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC accepts invalid program because of EPS poisoning](https://gitlab.haskell.org//ghc/ghc/issues/8427)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8528](https://gitlab.haskell.org//ghc/ghc/issues/8528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Preprocessor issues building GHC HEAD on Mavericks](https://gitlab.haskell.org//ghc/ghc/issues/8528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8545](https://gitlab.haskell.org//ghc/ghc/issues/8545)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Reorganize Git repositories](https://gitlab.haskell.org//ghc/ghc/issues/8545)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#8546](https://gitlab.haskell.org//ghc/ghc/issues/8546)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic during cabal build with profiling enabled.](https://gitlab.haskell.org//ghc/ghc/issues/8546)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8584](https://gitlab.haskell.org//ghc/ghc/issues/8584)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Pattern synonym type signatures](https://gitlab.haskell.org//ghc/ghc/issues/8584)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#8620](https://gitlab.haskell.org//ghc/ghc/issues/8620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[make -j3 build of head on Mac 10.9 with xcode 5 fails](https://gitlab.haskell.org//ghc/ghc/issues/8620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8621](https://gitlab.haskell.org//ghc/ghc/issues/8621)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Pull request for inclusion in \`unix' module of fsync(2), fdatasync(2), posix_fadvise(2) and posix_fallocate(2)](https://gitlab.haskell.org//ghc/ghc/issues/8621)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8624](https://gitlab.haskell.org//ghc/ghc/issues/8624)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[-ddump-splices-file](https://gitlab.haskell.org//ghc/ghc/issues/8624)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8701](https://gitlab.haskell.org//ghc/ghc/issues/8701)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Update libffi-tarballs to latest libffi](https://gitlab.haskell.org//ghc/ghc/issues/8701)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8704](https://gitlab.haskell.org//ghc/ghc/issues/8704)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Use GHC.Exts.build in randoms, randomRs to achieve fusion](https://gitlab.haskell.org//ghc/ghc/issues/8704)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rrnewton</th></tr>
<tr><th>[\#8712](https://gitlab.haskell.org//ghc/ghc/issues/8712)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.Ix missing info on row/column major indexing](https://gitlab.haskell.org//ghc/ghc/issues/8712)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8742](https://gitlab.haskell.org//ghc/ghc/issues/8742)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Reuse scavenge_small_bitmap](https://gitlab.haskell.org//ghc/ghc/issues/8742)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>Tarrasch</th></tr>
<tr><th>[\#8778](https://gitlab.haskell.org//ghc/ghc/issues/8778)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Typeable TypeNats](https://gitlab.haskell.org//ghc/ghc/issues/8778)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>diatchki</th></tr>
<tr><th>[\#8787](https://gitlab.haskell.org//ghc/ghc/issues/8787)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[compiler/ghc.mk: restore GhcHcOpts variable handling](https://gitlab.haskell.org//ghc/ghc/issues/8787)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8789](https://gitlab.haskell.org//ghc/ghc/issues/8789)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[includes/stg/SMP.h: use 'NOSMP' instead of never defined 'WITHSMP'](https://gitlab.haskell.org//ghc/ghc/issues/8789)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8790](https://gitlab.haskell.org//ghc/ghc/issues/8790)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[rts: unrust 'libbfd' debug symbols parser](https://gitlab.haskell.org//ghc/ghc/issues/8790)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8798](https://gitlab.haskell.org//ghc/ghc/issues/8798)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Missing symbols with -fprof-auto-calls](https://gitlab.haskell.org//ghc/ghc/issues/8798)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8807](https://gitlab.haskell.org//ghc/ghc/issues/8807)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Variable constraints not handled properly in TH](https://gitlab.haskell.org//ghc/ghc/issues/8807)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#8838](https://gitlab.haskell.org//ghc/ghc/issues/8838)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Allow running bash shell commands](https://gitlab.haskell.org//ghc/ghc/issues/8838)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8848](https://gitlab.haskell.org//ghc/ghc/issues/8848)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Warning: Rule too complicated to desugar](https://gitlab.haskell.org//ghc/ghc/issues/8848)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8861](https://gitlab.haskell.org//ghc/ghc/issues/8861)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Use commas to separate thousands when printing memory stats](https://gitlab.haskell.org//ghc/ghc/issues/8861)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#8873](https://gitlab.haskell.org//ghc/ghc/issues/8873)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[/cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/bin/ghcii-7.6.3.sh/../ghc: No such file or directory](https://gitlab.haskell.org//ghc/ghc/issues/8873)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#8877](https://gitlab.haskell.org//ghc/ghc/issues/8877)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["if this code is reached, the program will abort" in unregisterised build](https://gitlab.haskell.org//ghc/ghc/issues/8877)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8886](https://gitlab.haskell.org//ghc/ghc/issues/8886)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[sync-all: END actions result in confusing error message](https://gitlab.haskell.org//ghc/ghc/issues/8886)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#8899](https://gitlab.haskell.org//ghc/ghc/issues/8899)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[StdGen does not generate 0](https://gitlab.haskell.org//ghc/ghc/issues/8899)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#8923](https://gitlab.haskell.org//ghc/ghc/issues/8923)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add SmallArray\# type](https://gitlab.haskell.org//ghc/ghc/issues/8923)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>tibbe</th></tr>
<tr><th>[\#8926](https://gitlab.haskell.org//ghc/ghc/issues/8926)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC makes unsound references in object code](https://gitlab.haskell.org//ghc/ghc/issues/8926)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#8935](https://gitlab.haskell.org//ghc/ghc/issues/8935)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Obscure linker bug leads to crash in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/8935)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8939](https://gitlab.haskell.org//ghc/ghc/issues/8939)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Should check the return value of clock_gettime](https://gitlab.haskell.org//ghc/ghc/issues/8939)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#8940](https://gitlab.haskell.org//ghc/ghc/issues/8940)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["make help" fails](https://gitlab.haskell.org//ghc/ghc/issues/8940)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8943](https://gitlab.haskell.org//ghc/ghc/issues/8943)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add System.Process.createPipe](https://gitlab.haskell.org//ghc/ghc/issues/8943)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8968](https://gitlab.haskell.org//ghc/ghc/issues/8968)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms and GADTs](https://gitlab.haskell.org//ghc/ghc/issues/8968)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#8972](https://gitlab.haskell.org//ghc/ghc/issues/8972)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Investigate adding fast compare-and-swap Int type/primops](https://gitlab.haskell.org//ghc/ghc/issues/8972)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>tibbe</th></tr>
<tr><th>[\#8975](https://gitlab.haskell.org//ghc/ghc/issues/8975)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Troubling build warning with GHC 7.8.1 on Mavericks](https://gitlab.haskell.org//ghc/ghc/issues/8975)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8976](https://gitlab.haskell.org//ghc/ghc/issues/8976)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[dll-split: internal error: evacuate(static): strange closure type 0](https://gitlab.haskell.org//ghc/ghc/issues/8976)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8993](https://gitlab.haskell.org//ghc/ghc/issues/8993)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Fold NullaryTypeClasses into MPTC](https://gitlab.haskell.org//ghc/ghc/issues/8993)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9004](https://gitlab.haskell.org//ghc/ghc/issues/9004)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add sortOn function to Data.List](https://gitlab.haskell.org//ghc/ghc/issues/9004)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9008](https://gitlab.haskell.org//ghc/ghc/issues/9008)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Data.Function: Add reverse application operator](https://gitlab.haskell.org//ghc/ghc/issues/9008)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9016](https://gitlab.haskell.org//ghc/ghc/issues/9016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add System.Exit.die](https://gitlab.haskell.org//ghc/ghc/issues/9016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9032](https://gitlab.haskell.org//ghc/ghc/issues/9032)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic with self-import](https://gitlab.haskell.org//ghc/ghc/issues/9032)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#9055](https://gitlab.haskell.org//ghc/ghc/issues/9055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[unregisterised build fails with: globalRegMaybe not defined for this platform](https://gitlab.haskell.org//ghc/ghc/issues/9055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9057](https://gitlab.haskell.org//ghc/ghc/issues/9057)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Remove /usr/bin/… references](https://gitlab.haskell.org//ghc/ghc/issues/9057)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9067](https://gitlab.haskell.org//ghc/ghc/issues/9067)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Optimize clearNursery by short-circuiting when we get to currentNursery](https://gitlab.haskell.org//ghc/ghc/issues/9067)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9069](https://gitlab.haskell.org//ghc/ghc/issues/9069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[-XDeriveTraversable should imply -XDeriveFunctor and -XDeriveFoldable](https://gitlab.haskell.org//ghc/ghc/issues/9069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9086](https://gitlab.haskell.org//ghc/ghc/issues/9086)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[main :: IO Int does different things with runghc and when compiled](https://gitlab.haskell.org//ghc/ghc/issues/9086)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>gintas</th></tr>
<tr><th>[\#9092](https://gitlab.haskell.org//ghc/ghc/issues/9092)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc: panic! (the 'impossible' happened)](https://gitlab.haskell.org//ghc/ghc/issues/9092)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9097](https://gitlab.haskell.org//ghc/ghc/issues/9097)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Change GHC.Exts.Any to a type family](https://gitlab.haskell.org//ghc/ghc/issues/9097)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9099](https://gitlab.haskell.org//ghc/ghc/issues/9099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add strict fmap](https://gitlab.haskell.org//ghc/ghc/issues/9099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9100](https://gitlab.haskell.org//ghc/ghc/issues/9100)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cannot build crypto-pubkey with GHC 7.8.2 and profiling](https://gitlab.haskell.org//ghc/ghc/issues/9100)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9109](https://gitlab.haskell.org//ghc/ghc/issues/9109)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Improve error messages around "untouchable" type variables](https://gitlab.haskell.org//ghc/ghc/issues/9109)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9111](https://gitlab.haskell.org//ghc/ghc/issues/9111)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[base should export Typeable instances of its promoted data constructors](https://gitlab.haskell.org//ghc/ghc/issues/9111)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9126](https://gitlab.haskell.org//ghc/ghc/issues/9126)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-ddump-to-file in TcRnMonad.lhs](https://gitlab.haskell.org//ghc/ghc/issues/9126)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>GregWeber</th></tr>
<tr><th>[\#9132](https://gitlab.haskell.org//ghc/ghc/issues/9132)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[takeWhile&C. still not fusible](https://gitlab.haskell.org//ghc/ghc/issues/9132)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>skeuchel</th></tr>
<tr><th>[\#9140](https://gitlab.haskell.org//ghc/ghc/issues/9140)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Unboxed tuples fails in GHCi (7.8.2)](https://gitlab.haskell.org//ghc/ghc/issues/9140)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>archblob</th></tr>
<tr><th>[\#9142](https://gitlab.haskell.org//ghc/ghc/issues/9142)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[LLVM 3.5.0 rejects aliases  used by LLVM codegen](https://gitlab.haskell.org//ghc/ghc/issues/9142)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9154](https://gitlab.haskell.org//ghc/ghc/issues/9154)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Links to Cabal documentation from the user guide are broken](https://gitlab.haskell.org//ghc/ghc/issues/9154)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9163](https://gitlab.haskell.org//ghc/ghc/issues/9163)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Ptr should have a phantom role](https://gitlab.haskell.org//ghc/ghc/issues/9163)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9165](https://gitlab.haskell.org//ghc/ghc/issues/9165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Export TimerManager from GHC.Event](https://gitlab.haskell.org//ghc/ghc/issues/9165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9169](https://gitlab.haskell.org//ghc/ghc/issues/9169)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add mkWeakTMVar to Control.Concurrent.STM.TMVar](https://gitlab.haskell.org//ghc/ghc/issues/9169)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#9177](https://gitlab.haskell.org//ghc/ghc/issues/9177)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Suggest Int when user uses int](https://gitlab.haskell.org//ghc/ghc/issues/9177)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>nomeata</th></tr>
<tr><th>[\#9185](https://gitlab.haskell.org//ghc/ghc/issues/9185)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[glibc 2.20 outputs warnings for _BSD_SOURCE (Stg.h) on unknown archs](https://gitlab.haskell.org//ghc/ghc/issues/9185)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>juhpetersen</th></tr>
<tr><th>[\#9186](https://gitlab.haskell.org//ghc/ghc/issues/9186)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC panic when compiling compdata](https://gitlab.haskell.org//ghc/ghc/issues/9186)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9189](https://gitlab.haskell.org//ghc/ghc/issues/9189)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Linking fails on OSX when -staticlib and -threaded are enabled](https://gitlab.haskell.org//ghc/ghc/issues/9189)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9196](https://gitlab.haskell.org//ghc/ghc/issues/9196)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Higher-rank constraint treated as type instead](https://gitlab.haskell.org//ghc/ghc/issues/9196)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9200](https://gitlab.haskell.org//ghc/ghc/issues/9200)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Milner-Mycroft failure at the kind level](https://gitlab.haskell.org//ghc/ghc/issues/9200)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9201](https://gitlab.haskell.org//ghc/ghc/issues/9201)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC unexpectedly refines explicit kind signatures](https://gitlab.haskell.org//ghc/ghc/issues/9201)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9220](https://gitlab.haskell.org//ghc/ghc/issues/9220)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[type roles for unboxed arrays](https://gitlab.haskell.org//ghc/ghc/issues/9220)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9224](https://gitlab.haskell.org//ghc/ghc/issues/9224)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add support for binary integer literals](https://gitlab.haskell.org//ghc/ghc/issues/9224)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9230](https://gitlab.haskell.org//ghc/ghc/issues/9230)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Make -fwarn-tabs the default](https://gitlab.haskell.org//ghc/ghc/issues/9230)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>mlen</th></tr>
<tr><th>[\#9240](https://gitlab.haskell.org//ghc/ghc/issues/9240)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["read . show ≡ id" not satisfied by Data.Fixed](https://gitlab.haskell.org//ghc/ghc/issues/9240)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9243](https://gitlab.haskell.org//ghc/ghc/issues/9243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Recompilation avoidance doesn't work for -fno-code/-fwrite-interface](https://gitlab.haskell.org//ghc/ghc/issues/9243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9265](https://gitlab.haskell.org//ghc/ghc/issues/9265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Create PackageKey to replace PackageId, including version dependency information](https://gitlab.haskell.org//ghc/ghc/issues/9265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9268](https://gitlab.haskell.org//ghc/ghc/issues/9268)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[internal error: evacuate(static): strange closure type -385875968](https://gitlab.haskell.org//ghc/ghc/issues/9268)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9281](https://gitlab.haskell.org//ghc/ghc/issues/9281)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Rewrite \`integer-gmp\` to use only non-allocating GMP functions](https://gitlab.haskell.org//ghc/ghc/issues/9281)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9284](https://gitlab.haskell.org//ghc/ghc/issues/9284)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[shutdownCapability sometimes loops indefinitely on OSX after forkProcess](https://gitlab.haskell.org//ghc/ghc/issues/9284)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9293](https://gitlab.haskell.org//ghc/ghc/issues/9293)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCI :unset -XLanguageExtension returns "don't know how to reverse -XLanguageExtension"](https://gitlab.haskell.org//ghc/ghc/issues/9293)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>leroux</th></tr>
<tr><th>[\#9294](https://gitlab.haskell.org//ghc/ghc/issues/9294)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[More exports and documentation for GHC API Parser Module](https://gitlab.haskell.org//ghc/ghc/issues/9294)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>agibiansky</th></tr>
<tr><th>[\#9309](https://gitlab.haskell.org//ghc/ghc/issues/9309)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Regression when building the 'sizes' application](https://gitlab.haskell.org//ghc/ghc/issues/9309)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9317](https://gitlab.haskell.org//ghc/ghc/issues/9317)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Add PolyKinds extension to Data.Monoid](https://gitlab.haskell.org//ghc/ghc/issues/9317)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9318](https://gitlab.haskell.org//ghc/ghc/issues/9318)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type error reported in wrong place with repeated type family expressions](https://gitlab.haskell.org//ghc/ghc/issues/9318)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#9323](https://gitlab.haskell.org//ghc/ghc/issues/9323)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Confusing type error behaviour](https://gitlab.haskell.org//ghc/ghc/issues/9323)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9324](https://gitlab.haskell.org//ghc/ghc/issues/9324)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi permission checks should ignore root user](https://gitlab.haskell.org//ghc/ghc/issues/9324)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9331](https://gitlab.haskell.org//ghc/ghc/issues/9331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Release Cabal 1.22 before GHC 7.10 release](https://gitlab.haskell.org//ghc/ghc/issues/9331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9332](https://gitlab.haskell.org//ghc/ghc/issues/9332)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Memory blowing up for strict sum/strict foldl in ghci](https://gitlab.haskell.org//ghc/ghc/issues/9332)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9333](https://gitlab.haskell.org//ghc/ghc/issues/9333)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Thread status decoded wrong in base library](https://gitlab.haskell.org//ghc/ghc/issues/9333)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>jberthold</th></tr>
<tr><th>[\#9337](https://gitlab.haskell.org//ghc/ghc/issues/9337)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add \`sortOn\` function to Data.List](https://gitlab.haskell.org//ghc/ghc/issues/9337)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9340](https://gitlab.haskell.org//ghc/ghc/issues/9340)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Implement new \`clz\` inline primop](https://gitlab.haskell.org//ghc/ghc/issues/9340)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9341](https://gitlab.haskell.org//ghc/ghc/issues/9341)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Evaluate default CPUs setting for validate](https://gitlab.haskell.org//ghc/ghc/issues/9341)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9355](https://gitlab.haskell.org//ghc/ghc/issues/9355)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[scanr does not participate in stream fusion](https://gitlab.haskell.org//ghc/ghc/issues/9355)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9359](https://gitlab.haskell.org//ghc/ghc/issues/9359)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Deriving clause failure with polymorphic kinds](https://gitlab.haskell.org//ghc/ghc/issues/9359)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9362](https://gitlab.haskell.org//ghc/ghc/issues/9362)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[make clean deletes inplace mingw](https://gitlab.haskell.org//ghc/ghc/issues/9362)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9363](https://gitlab.haskell.org//ghc/ghc/issues/9363)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[windows configure 'find invalid mode'](https://gitlab.haskell.org//ghc/ghc/issues/9363)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9368](https://gitlab.haskell.org//ghc/ghc/issues/9368)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add strictly accumulating scanl' to Data.List](https://gitlab.haskell.org//ghc/ghc/issues/9368)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9369](https://gitlab.haskell.org//ghc/ghc/issues/9369)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.List.unfoldr does not fuse and is not inlined.](https://gitlab.haskell.org//ghc/ghc/issues/9369)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9372](https://gitlab.haskell.org//ghc/ghc/issues/9372)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[dll-split during stage 2 compiling ghc v7.8.3 for arm_linux](https://gitlab.haskell.org//ghc/ghc/issues/9372)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9382](https://gitlab.haskell.org//ghc/ghc/issues/9382)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Have rts/Linker.c lookupSymbol find symbols in the process executable.](https://gitlab.haskell.org//ghc/ghc/issues/9382)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>facundo.dominguez</th></tr>
<tr><th>[\#9384](https://gitlab.haskell.org//ghc/ghc/issues/9384)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[setNumCapabilities call breaks eventlog events](https://gitlab.haskell.org//ghc/ghc/issues/9384)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9391](https://gitlab.haskell.org//ghc/ghc/issues/9391)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[LLVM 3.2 crash (AVX messes up GHC calling convention)](https://gitlab.haskell.org//ghc/ghc/issues/9391)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9393](https://gitlab.haskell.org//ghc/ghc/issues/9393)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[execvpe should handle ENOTDIR](https://gitlab.haskell.org//ghc/ghc/issues/9393)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9396](https://gitlab.haskell.org//ghc/ghc/issues/9396)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Code cleanup: warning: use of GNU old-style field designator extension](https://gitlab.haskell.org//ghc/ghc/issues/9396)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9400](https://gitlab.haskell.org//ghc/ghc/issues/9400)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[poor performance when compiling modules with many Text literals at -O1](https://gitlab.haskell.org//ghc/ghc/issues/9400)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>xnyhps</th></tr>
<tr><th>[\#9423](https://gitlab.haskell.org//ghc/ghc/issues/9423)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[shutdownCapability sometimes loops indefinitely on OSX after hs_exit()](https://gitlab.haskell.org//ghc/ghc/issues/9423)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9440](https://gitlab.haskell.org//ghc/ghc/issues/9440)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Buggy binder swap in occurrence analysis](https://gitlab.haskell.org//ghc/ghc/issues/9440)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9449](https://gitlab.haskell.org//ghc/ghc/issues/9449)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC.Prim documentation says "Safe Inferred"](https://gitlab.haskell.org//ghc/ghc/issues/9449)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#9480](https://gitlab.haskell.org//ghc/ghc/issues/9480)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Segfault in GHC API code using Shelly](https://gitlab.haskell.org//ghc/ghc/issues/9480)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9495](https://gitlab.haskell.org//ghc/ghc/issues/9495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Do What I Mean RULES for foldr2 look shady](https://gitlab.haskell.org//ghc/ghc/issues/9495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9497](https://gitlab.haskell.org//ghc/ghc/issues/9497)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Silent typed holes](https://gitlab.haskell.org//ghc/ghc/issues/9497)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>merijn</th></tr>
<tr><th>[\#9508](https://gitlab.haskell.org//ghc/ghc/issues/9508)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Rename package key](https://gitlab.haskell.org//ghc/ghc/issues/9508)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9510](https://gitlab.haskell.org//ghc/ghc/issues/9510)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Prelude.!! is not a good consumer](https://gitlab.haskell.org//ghc/ghc/issues/9510)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9527](https://gitlab.haskell.org//ghc/ghc/issues/9527)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add Generic instances for Language.Haskell.TH](https://gitlab.haskell.org//ghc/ghc/issues/9527)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9529](https://gitlab.haskell.org//ghc/ghc/issues/9529)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Clean up cseProgram](https://gitlab.haskell.org//ghc/ghc/issues/9529)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9531](https://gitlab.haskell.org//ghc/ghc/issues/9531)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement Prelude.Word Proposal](https://gitlab.haskell.org//ghc/ghc/issues/9531)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9532](https://gitlab.haskell.org//ghc/ghc/issues/9532)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Expose new CLZ/CTZ primops via \`Data.Bits\` interface](https://gitlab.haskell.org//ghc/ghc/issues/9532)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9535](https://gitlab.haskell.org//ghc/ghc/issues/9535)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Remove max_bytes_used from haddock tests](https://gitlab.haskell.org//ghc/ghc/issues/9535)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9536](https://gitlab.haskell.org//ghc/ghc/issues/9536)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement foldr/cons/build](https://gitlab.haskell.org//ghc/ghc/issues/9536)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9537](https://gitlab.haskell.org//ghc/ghc/issues/9537)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[concatMap is not a good producer for list fusion](https://gitlab.haskell.org//ghc/ghc/issues/9537)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9540](https://gitlab.haskell.org//ghc/ghc/issues/9540)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[words is not a good producer; unwords is not a good consumer](https://gitlab.haskell.org//ghc/ghc/issues/9540)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9550](https://gitlab.haskell.org//ghc/ghc/issues/9550)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add uncons to Data.List](https://gitlab.haskell.org//ghc/ghc/issues/9550)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9561](https://gitlab.haskell.org//ghc/ghc/issues/9561)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Clean up mergeSATInfo](https://gitlab.haskell.org//ghc/ghc/issues/9561)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9578](https://gitlab.haskell.org//ghc/ghc/issues/9578)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Quoting issue in configure.ac](https://gitlab.haskell.org//ghc/ghc/issues/9578)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9582](https://gitlab.haskell.org//ghc/ghc/issues/9582)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Associated Type Synonyms do not unfold in InstanceSigs](https://gitlab.haskell.org//ghc/ghc/issues/9582)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9586](https://gitlab.haskell.org//ghc/ghc/issues/9586)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement Traversable/Foldable-Burning-Bridges Proposal](https://gitlab.haskell.org//ghc/ghc/issues/9586)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9590](https://gitlab.haskell.org//ghc/ghc/issues/9590)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[AMP breaks \`haskell2010\` package](https://gitlab.haskell.org//ghc/ghc/issues/9590)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9593](https://gitlab.haskell.org//ghc/ghc/issues/9593)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Edit New issue Building current ghc HEAD fails with globalRegMaybe not defined for this platform](https://gitlab.haskell.org//ghc/ghc/issues/9593)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9598](https://gitlab.haskell.org//ghc/ghc/issues/9598)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC fails to build on Windows because of AMP breakage in haskeline](https://gitlab.haskell.org//ghc/ghc/issues/9598)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#9604](https://gitlab.haskell.org//ghc/ghc/issues/9604)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Python test runner assumes native Python on Windows](https://gitlab.haskell.org//ghc/ghc/issues/9604)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9605](https://gitlab.haskell.org//ghc/ghc/issues/9605)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Misleading error message with forgotten "do"](https://gitlab.haskell.org//ghc/ghc/issues/9605)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Yuras</th></tr>
<tr><th>[\#9609](https://gitlab.haskell.org//ghc/ghc/issues/9609)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi gives overly specific error message for unknown constructor](https://gitlab.haskell.org//ghc/ghc/issues/9609)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9610](https://gitlab.haskell.org//ghc/ghc/issues/9610)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Missing fixity declaration for \`Data.Foldable.{elem,notElem}\`](https://gitlab.haskell.org//ghc/ghc/issues/9610)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9619](https://gitlab.haskell.org//ghc/ghc/issues/9619)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[HPC Code Coverage complains when two exactly the same mix files are on the path](https://gitlab.haskell.org//ghc/ghc/issues/9619)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#9621](https://gitlab.haskell.org//ghc/ghc/issues/9621)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[New Data.Foldable methods](https://gitlab.haskell.org//ghc/ghc/issues/9621)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9623](https://gitlab.haskell.org//ghc/ghc/issues/9623)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Use Data.List.dropWhileEnd](https://gitlab.haskell.org//ghc/ghc/issues/9623)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9626](https://gitlab.haskell.org//ghc/ghc/issues/9626)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Test command lines munged on Windows when running on msys Python](https://gitlab.haskell.org//ghc/ghc/issues/9626)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9633](https://gitlab.haskell.org//ghc/ghc/issues/9633)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[PolyKinds in 7.8.2 vs 7.8.3](https://gitlab.haskell.org//ghc/ghc/issues/9633)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9639](https://gitlab.haskell.org//ghc/ghc/issues/9639)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Remove OldTypeable](https://gitlab.haskell.org//ghc/ghc/issues/9639)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9644](https://gitlab.haskell.org//ghc/ghc/issues/9644)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[mapMaybes documentation contains some gibberish](https://gitlab.haskell.org//ghc/ghc/issues/9644)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9652](https://gitlab.haskell.org//ghc/ghc/issues/9652)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Bootstrapping GHC HEAD against itself doesn't work](https://gitlab.haskell.org//ghc/ghc/issues/9652)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9654](https://gitlab.haskell.org//ghc/ghc/issues/9654)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Clean up stringify in util/hsc2hs/CrossCodegen](https://gitlab.haskell.org//ghc/ghc/issues/9654)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9662](https://gitlab.haskell.org//ghc/ghc/issues/9662)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[stack overflow in type checker](https://gitlab.haskell.org//ghc/ghc/issues/9662)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9664](https://gitlab.haskell.org//ghc/ghc/issues/9664)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Add identity functor to base](https://gitlab.haskell.org//ghc/ghc/issues/9664)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9668](https://gitlab.haskell.org//ghc/ghc/issues/9668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Unicode info is out of date](https://gitlab.haskell.org//ghc/ghc/issues/9668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9670](https://gitlab.haskell.org//ghc/ghc/issues/9670)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make Data.List.tails a good producer for list fusion](https://gitlab.haskell.org//ghc/ghc/issues/9670)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9676](https://gitlab.haskell.org//ghc/ghc/issues/9676)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.List.isSuffixOf can be very inefficient](https://gitlab.haskell.org//ghc/ghc/issues/9676)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9681](https://gitlab.haskell.org//ghc/ghc/issues/9681)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Printing of "\\n" in error messages is broken](https://gitlab.haskell.org//ghc/ghc/issues/9681)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9682](https://gitlab.haskell.org//ghc/ghc/issues/9682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement "Add bifunctor related classes to base"-Proposal (1/3)](https://gitlab.haskell.org//ghc/ghc/issues/9682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9697](https://gitlab.haskell.org//ghc/ghc/issues/9697)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[./configure fails due to obselete usage of 'find -perm' in aclocal.m4](https://gitlab.haskell.org//ghc/ghc/issues/9697)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9698](https://gitlab.haskell.org//ghc/ghc/issues/9698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[GHC_PACKAGE_PATH should be more lenient for empty paths](https://gitlab.haskell.org//ghc/ghc/issues/9698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9703](https://gitlab.haskell.org//ghc/ghc/issues/9703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add missing calling conventions to Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/9703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cmsaperstein</th></tr>
<tr><th>[\#9713](https://gitlab.haskell.org//ghc/ghc/issues/9713)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Update comment about C helper for dynamic exports.](https://gitlab.haskell.org//ghc/ghc/issues/9713)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9714](https://gitlab.haskell.org//ghc/ghc/issues/9714)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Un-\`wire\` Integer type](https://gitlab.haskell.org//ghc/ghc/issues/9714)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9722](https://gitlab.haskell.org//ghc/ghc/issues/9722)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghcirun004 intermittently fails with ghc: ioManagerWakeup: write: Bad file descriptor](https://gitlab.haskell.org//ghc/ghc/issues/9722)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9732](https://gitlab.haskell.org//ghc/ghc/issues/9732)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms and unboxed values](https://gitlab.haskell.org//ghc/ghc/issues/9732)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9734](https://gitlab.haskell.org//ghc/ghc/issues/9734)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Provide macro  __GLASGOW_HASKELL_TH__](https://gitlab.haskell.org//ghc/ghc/issues/9734)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9736](https://gitlab.haskell.org//ghc/ghc/issues/9736)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Constant folding rules are wrong for GHCJS](https://gitlab.haskell.org//ghc/ghc/issues/9736)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>luite</th></tr>
<tr><th>[\#9738](https://gitlab.haskell.org//ghc/ghc/issues/9738)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Handle ANN pragmas in declaration splices](https://gitlab.haskell.org//ghc/ghc/issues/9738)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9739](https://gitlab.haskell.org//ghc/ghc/issues/9739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.8 chokes on recursive classes](https://gitlab.haskell.org//ghc/ghc/issues/9739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9740](https://gitlab.haskell.org//ghc/ghc/issues/9740)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[D380 caused fft2 regressions](https://gitlab.haskell.org//ghc/ghc/issues/9740)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9741](https://gitlab.haskell.org//ghc/ghc/issues/9741)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Interpreter stack checks are not quite right](https://gitlab.haskell.org//ghc/ghc/issues/9741)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9742](https://gitlab.haskell.org//ghc/ghc/issues/9742)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[The default definitions of foldl1 and foldr1 are too strict](https://gitlab.haskell.org//ghc/ghc/issues/9742)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9754](https://gitlab.haskell.org//ghc/ghc/issues/9754)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Fix Applicative instances in the wake of AMP](https://gitlab.haskell.org//ghc/ghc/issues/9754)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9759](https://gitlab.haskell.org//ghc/ghc/issues/9759)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Add Alternative wrapper to Data.Monoid](https://gitlab.haskell.org//ghc/ghc/issues/9759)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9762](https://gitlab.haskell.org//ghc/ghc/issues/9762)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\#8696 + \#9012](https://gitlab.haskell.org//ghc/ghc/issues/9762)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rwbarton</th></tr>
<tr><th>[\#9763](https://gitlab.haskell.org//ghc/ghc/issues/9763)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement Foldable methods for Array directly](https://gitlab.haskell.org//ghc/ghc/issues/9763)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9767](https://gitlab.haskell.org//ghc/ghc/issues/9767)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add isSubsequenceOf to Data.List](https://gitlab.haskell.org//ghc/ghc/issues/9767)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9771](https://gitlab.haskell.org//ghc/ghc/issues/9771)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Excessive memory usage compiling T3064](https://gitlab.haskell.org//ghc/ghc/issues/9771)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#9776](https://gitlab.haskell.org//ghc/ghc/issues/9776)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-frule-check flag not recognized without parameter](https://gitlab.haskell.org//ghc/ghc/issues/9776)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>carlostome</th></tr>
<tr><th>[\#9778](https://gitlab.haskell.org//ghc/ghc/issues/9778)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Namespace collision detection for promoted types](https://gitlab.haskell.org//ghc/ghc/issues/9778)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>carlostome</th></tr>
<tr><th>[\#9781](https://gitlab.haskell.org//ghc/ghc/issues/9781)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make list monad operations fuse](https://gitlab.haskell.org//ghc/ghc/issues/9781)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9783](https://gitlab.haskell.org//ghc/ghc/issues/9783)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonym matcher is unnecessarily strict on unboxed continuations](https://gitlab.haskell.org//ghc/ghc/issues/9783)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9787](https://gitlab.haskell.org//ghc/ghc/issues/9787)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[typo in the ghci help : 'simplifed'](https://gitlab.haskell.org//ghc/ghc/issues/9787)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9788](https://gitlab.haskell.org//ghc/ghc/issues/9788)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\`coerce\` has an impossible type](https://gitlab.haskell.org//ghc/ghc/issues/9788)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9796](https://gitlab.haskell.org//ghc/ghc/issues/9796)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Implement amap/coerce rule for \`Array\`](https://gitlab.haskell.org//ghc/ghc/issues/9796)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9800](https://gitlab.haskell.org//ghc/ghc/issues/9800)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic when building HEAD with BuildFlavour=quickest: "Can't use Integer in integer-\*"](https://gitlab.haskell.org//ghc/ghc/issues/9800)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9801](https://gitlab.haskell.org//ghc/ghc/issues/9801)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make listArray fuse](https://gitlab.haskell.org//ghc/ghc/issues/9801)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9807](https://gitlab.haskell.org//ghc/ghc/issues/9807)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Only test for bug \#9439 when llvm is installed](https://gitlab.haskell.org//ghc/ghc/issues/9807)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9808](https://gitlab.haskell.org//ghc/ghc/issues/9808)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Add data-dir to InstalledPackageInfo](https://gitlab.haskell.org//ghc/ghc/issues/9808)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>luite</th></tr>
<tr><th>[\#9810](https://gitlab.haskell.org//ghc/ghc/issues/9810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[encodeFloat 1 2047 = -1024.0](https://gitlab.haskell.org//ghc/ghc/issues/9810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9814](https://gitlab.haskell.org//ghc/ghc/issues/9814)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add Data.Void to base](https://gitlab.haskell.org//ghc/ghc/issues/9814)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9816](https://gitlab.haskell.org//ghc/ghc/issues/9816)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add function for size-checked conversion of Integral types](https://gitlab.haskell.org//ghc/ghc/issues/9816)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9818](https://gitlab.haskell.org//ghc/ghc/issues/9818)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add \`Natural\` number type to \`base\`](https://gitlab.haskell.org//ghc/ghc/issues/9818)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#9822](https://gitlab.haskell.org//ghc/ghc/issues/9822)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add displayException to Exception typeclass](https://gitlab.haskell.org//ghc/ghc/issues/9822)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>snoyberg</th></tr>
<tr><th>[\#9826](https://gitlab.haskell.org//ghc/ghc/issues/9826)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[add Storable Complex and Ratio instance to base library](https://gitlab.haskell.org//ghc/ghc/issues/9826)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>carter</th></tr>
<tr><th>[\#9827](https://gitlab.haskell.org//ghc/ghc/issues/9827)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[void does not use \<$](https://gitlab.haskell.org//ghc/ghc/issues/9827)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th></tr>
<tr><th>[\#9828](https://gitlab.haskell.org//ghc/ghc/issues/9828)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[genprimopcode: parse error](https://gitlab.haskell.org//ghc/ghc/issues/9828)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thomie</th></tr>
<tr><th>[\#9836](https://gitlab.haskell.org//ghc/ghc/issues/9836)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Facility to mitigate double-breakage due to Data.Version.versionTag Deprecation](https://gitlab.haskell.org//ghc/ghc/issues/9836)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9845](https://gitlab.haskell.org//ghc/ghc/issues/9845)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC API evaluating some Cabal code fails with SegFault if executable is compiled statically](https://gitlab.haskell.org//ghc/ghc/issues/9845)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9856](https://gitlab.haskell.org//ghc/ghc/issues/9856)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Test suite regressions due to integer-gmp2](https://gitlab.haskell.org//ghc/ghc/issues/9856)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9857](https://gitlab.haskell.org//ghc/ghc/issues/9857)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.9 panics (simplifier ticks exhausted) on \`half-0.2\`](https://gitlab.haskell.org//ghc/ghc/issues/9857)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#9859](https://gitlab.haskell.org//ghc/ghc/issues/9859)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Implement \`calloc{,Bytes,Array,Array0}\` allocators](https://gitlab.haskell.org//ghc/ghc/issues/9859)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9870](https://gitlab.haskell.org//ghc/ghc/issues/9870)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Document deviation of Data.List.splitAt from Report semantics](https://gitlab.haskell.org//ghc/ghc/issues/9870)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9873](https://gitlab.haskell.org//ghc/ghc/issues/9873)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Use CONF_GCC_LINKER_OPTS_STAGE2 only in stage2](https://gitlab.haskell.org//ghc/ghc/issues/9873)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9875](https://gitlab.haskell.org//ghc/ghc/issues/9875)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ld -l:filename.dylib does not appear to be portable](https://gitlab.haskell.org//ghc/ghc/issues/9875)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9876](https://gitlab.haskell.org//ghc/ghc/issues/9876)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[mkdir errors when running \`make sdist\`](https://gitlab.haskell.org//ghc/ghc/issues/9876)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>joehillen</th></tr>
<tr><th>[\#9879](https://gitlab.haskell.org//ghc/ghc/issues/9879)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic with partial type signatures](https://gitlab.haskell.org//ghc/ghc/issues/9879)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thomasw</th></tr>
<tr><th>[\#9886](https://gitlab.haskell.org//ghc/ghc/issues/9886)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[PowerPC : Undefined reference to \`__sync_fetch_and_xor_8'](https://gitlab.haskell.org//ghc/ghc/issues/9886)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>erikd</th></tr>
<tr><th>[\#9889](https://gitlab.haskell.org//ghc/ghc/issues/9889)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonym does not work in top-level pattern bind](https://gitlab.haskell.org//ghc/ghc/issues/9889)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9900](https://gitlab.haskell.org//ghc/ghc/issues/9900)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Support pattern synonyms in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/9900)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9905](https://gitlab.haskell.org//ghc/ghc/issues/9905)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.8.x command line error](https://gitlab.haskell.org//ghc/ghc/issues/9905)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rwbarton</th></tr>
<tr><th>[\#9914](https://gitlab.haskell.org//ghc/ghc/issues/9914)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Inconsistent handling of leading whitespace in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/9914)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9915](https://gitlab.haskell.org//ghc/ghc/issues/9915)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi has trouble with 'foreign' when it is not a keyword](https://gitlab.haskell.org//ghc/ghc/issues/9915)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9922](https://gitlab.haskell.org//ghc/ghc/issues/9922)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Partial type signatures + extensions panic](https://gitlab.haskell.org//ghc/ghc/issues/9922)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thomasw</th></tr>
<tr><th>[\#9924](https://gitlab.haskell.org//ghc/ghc/issues/9924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[libffi configure script does not detect MSYS2 Windows 10 x86_64](https://gitlab.haskell.org//ghc/ghc/issues/9924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9926](https://gitlab.haskell.org//ghc/ghc/issues/9926)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Restore INSTALL file in src dist](https://gitlab.haskell.org//ghc/ghc/issues/9926)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9928](https://gitlab.haskell.org//ghc/ghc/issues/9928)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Wrong information about the previous version of the time library in GHC 7.10.1 RC2 release notes](https://gitlab.haskell.org//ghc/ghc/issues/9928)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9930](https://gitlab.haskell.org//ghc/ghc/issues/9930)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[By default, a source code without extension would get overwritten by executable on \*nix](https://gitlab.haskell.org//ghc/ghc/issues/9930)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Rufflewind</th></tr>
<tr><th>[\#9934](https://gitlab.haskell.org//ghc/ghc/issues/9934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Typo in GHC.RTS.Flags](https://gitlab.haskell.org//ghc/ghc/issues/9934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9935](https://gitlab.haskell.org//ghc/ghc/issues/9935)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Can't compile rts/StgCRun.c for aarch64-linux](https://gitlab.haskell.org//ghc/ghc/issues/9935)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>erikd</th></tr>
<tr><th>[\#9937](https://gitlab.haskell.org//ghc/ghc/issues/9937)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[add updated prefetch API  mention to the 7.10 release notes](https://gitlab.haskell.org//ghc/ghc/issues/9937)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9949](https://gitlab.haskell.org//ghc/ghc/issues/9949)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[zipWith too strict in second argument in GHC-7.10.0](https://gitlab.haskell.org//ghc/ghc/issues/9949)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9953](https://gitlab.haskell.org//ghc/ghc/issues/9953)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms don't work with GADTs](https://gitlab.haskell.org//ghc/ghc/issues/9953)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9955](https://gitlab.haskell.org//ghc/ghc/issues/9955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-stage1 compiles with bootstrapping ghc package, not the built one](https://gitlab.haskell.org//ghc/ghc/issues/9955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#9956](https://gitlab.haskell.org//ghc/ghc/issues/9956)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Command line flag deprecated warning could be annoying for -Werror users](https://gitlab.haskell.org//ghc/ghc/issues/9956)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9957](https://gitlab.haskell.org//ghc/ghc/issues/9957)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Document -fwarn-unticked-promoted-constructors in release notes](https://gitlab.haskell.org//ghc/ghc/issues/9957)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9961](https://gitlab.haskell.org//ghc/ghc/issues/9961)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[compile-time performance regression compiling genprimcode](https://gitlab.haskell.org//ghc/ghc/issues/9961)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#9963](https://gitlab.haskell.org//ghc/ghc/issues/9963)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi panic with --print-libdir flag](https://gitlab.haskell.org//ghc/ghc/issues/9963)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>thomie</th></tr>
<tr><th>[\#9967](https://gitlab.haskell.org//ghc/ghc/issues/9967)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonym type signature documentation out of date](https://gitlab.haskell.org//ghc/ghc/issues/9967)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9971](https://gitlab.haskell.org//ghc/ghc/issues/9971)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.10 dies with "out of memory"](https://gitlab.haskell.org//ghc/ghc/issues/9971)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#9975](https://gitlab.haskell.org//ghc/ghc/issues/9975)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[RecordWildcards and PatternSynonyms cause impossible bug](https://gitlab.haskell.org//ghc/ghc/issues/9975)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9984](https://gitlab.haskell.org//ghc/ghc/issues/9984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Show, Read, Eq, Ord instances for Control.Applicative.Const](https://gitlab.haskell.org//ghc/ghc/issues/9984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9988](https://gitlab.haskell.org//ghc/ghc/issues/9988)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Remove fun_id, is_infix from FunBind, as they are now in Match](https://gitlab.haskell.org//ghc/ghc/issues/9988)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>alanz</th></tr>
<tr><th>[\#9997](https://gitlab.haskell.org//ghc/ghc/issues/9997)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["A module cannot import itself"-regression](https://gitlab.haskell.org//ghc/ghc/issues/9997)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#9999](https://gitlab.haskell.org//ghc/ghc/issues/9999)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Typeable deriving behavior different between data families and associated data types](https://gitlab.haskell.org//ghc/ghc/issues/9999)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dreixel</th></tr>
<tr><th>[\#10002](https://gitlab.haskell.org//ghc/ghc/issues/10002)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Invalid ghcversion.h file in GHC 7.10.1 RC1](https://gitlab.haskell.org//ghc/ghc/issues/10002)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>hvr</th></tr>
<tr><th>[\#10003](https://gitlab.haskell.org//ghc/ghc/issues/10003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[integer-gmp2 tries to be GMP 4.x compatible but uses functions from GMP 5.x](https://gitlab.haskell.org//ghc/ghc/issues/10003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>hvr</th></tr>
<tr><th>[\#10004](https://gitlab.haskell.org//ghc/ghc/issues/10004)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[rec {} causes "head:empty list" exception](https://gitlab.haskell.org//ghc/ghc/issues/10004)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10011](https://gitlab.haskell.org//ghc/ghc/issues/10011)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[The Data instance for Ratio violates internal invariants.](https://gitlab.haskell.org//ghc/ghc/issues/10011)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10017](https://gitlab.haskell.org//ghc/ghc/issues/10017)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[signal handlers are invoked multiple times when the threaded rts is used](https://gitlab.haskell.org//ghc/ghc/issues/10017)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>AndreasVoellmy</th></tr>
<tr><th>[\#10019](https://gitlab.haskell.org//ghc/ghc/issues/10019)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi cannot reify a custom data constructor with Template Haskell on the first attempt](https://gitlab.haskell.org//ghc/ghc/issues/10019)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10020](https://gitlab.haskell.org//ghc/ghc/issues/10020)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.10 rejects nullary type class with associated data](https://gitlab.haskell.org//ghc/ghc/issues/10020)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10031](https://gitlab.haskell.org//ghc/ghc/issues/10031)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[coerce can cause compiler to loop](https://gitlab.haskell.org//ghc/ghc/issues/10031)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10036](https://gitlab.haskell.org//ghc/ghc/issues/10036)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Update Cabal before final 7.10 release](https://gitlab.haskell.org//ghc/ghc/issues/10036)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10038](https://gitlab.haskell.org//ghc/ghc/issues/10038)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Missing information about some libraries in the GHC 7.10.1 RC2 release notes](https://gitlab.haskell.org//ghc/ghc/issues/10038)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10043](https://gitlab.haskell.org//ghc/ghc/issues/10043)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[runtime fails in threaded way on SPARC (bus error -\> unaligned access to data)](https://gitlab.haskell.org//ghc/ghc/issues/10043)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>kgardas</th></tr>
<tr><th>[\#10050](https://gitlab.haskell.org//ghc/ghc/issues/10050)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[template haskell Ppr missing parentheses for SigT](https://gitlab.haskell.org//ghc/ghc/issues/10050)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10051](https://gitlab.haskell.org//ghc/ghc/issues/10051)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[panic - the 'impossible' happened](https://gitlab.haskell.org//ghc/ghc/issues/10051)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10058](https://gitlab.haskell.org//ghc/ghc/issues/10058)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic: Loading temp shared object failed](https://gitlab.haskell.org//ghc/ghc/issues/10058)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10072](https://gitlab.haskell.org//ghc/ghc/issues/10072)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic: generalised wildcards in RULES](https://gitlab.haskell.org//ghc/ghc/issues/10072)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thomasw</th></tr>
<tr><th>[\#10078](https://gitlab.haskell.org//ghc/ghc/issues/10078)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[tcPluginStop of a type checker plugin is not called if an error occurs](https://gitlab.haskell.org//ghc/ghc/issues/10078)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>jbracker</th></tr>
<tr><th>[\#10088](https://gitlab.haskell.org//ghc/ghc/issues/10088)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Broken link in Data.Ix documentation](https://gitlab.haskell.org//ghc/ghc/issues/10088)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#10091](https://gitlab.haskell.org//ghc/ghc/issues/10091)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add configurable verbosity level to hpc executable](https://gitlab.haskell.org//ghc/ghc/issues/10091)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Yuras</th></tr>
<tr><th>[\#10096](https://gitlab.haskell.org//ghc/ghc/issues/10096)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Top-level ./configure should accept and propagate --with-curses-{includes,libraries} to libraries](https://gitlab.haskell.org//ghc/ghc/issues/10096)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10099](https://gitlab.haskell.org//ghc/ghc/issues/10099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[cabal install broken with ghc 7.10.1-rc2](https://gitlab.haskell.org//ghc/ghc/issues/10099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10113](https://gitlab.haskell.org//ghc/ghc/issues/10113)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Re-export (\<$\>) and (\<$) from Prelude](https://gitlab.haskell.org//ghc/ghc/issues/10113)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10115](https://gitlab.haskell.org//ghc/ghc/issues/10115)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Unable to run cabal haddock --hoogle on GHC 7.10](https://gitlab.haskell.org//ghc/ghc/issues/10115)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10128](https://gitlab.haskell.org//ghc/ghc/issues/10128)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.List.transpose needs more docs](https://gitlab.haskell.org//ghc/ghc/issues/10128)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#10142](https://gitlab.haskell.org//ghc/ghc/issues/10142)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Documentation for Ix is contradictory around minimal definition](https://gitlab.haskell.org//ghc/ghc/issues/10142)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>hvr</th></tr>
<tr><th>[\#10164](https://gitlab.haskell.org//ghc/ghc/issues/10164)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Cleanup test framework string formatting](https://gitlab.haskell.org//ghc/ghc/issues/10164)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10165](https://gitlab.haskell.org//ghc/ghc/issues/10165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Win32 broken with GHC 7.10 RC3](https://gitlab.haskell.org//ghc/ghc/issues/10165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10176](https://gitlab.haskell.org//ghc/ghc/issues/10176)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Invalid core generated with GHC 7.10 RC3](https://gitlab.haskell.org//ghc/ghc/issues/10176)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>nomeata</th></tr></table>