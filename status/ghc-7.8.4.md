## Tickets scheduled for 7.8.4


See [milestone:7.8.4](/trac/ghc/milestone/7.8.4)


Note however, that not all tickets with a 7.8.4 milestone will be addressed for the final 7.8.4 release (if it happens at all).

### [\#9439](https://gitlab.haskell.org//ghc/ghc/issues/9439): LLVM mangler mangles too vigorously


After 7.8.3 it was realized that the LLVM code generator's mangler mangled occurrences of tokens occurring within strings of user code. This very non-obvious miscompilation. While the tokens involved aren't likely to appear in user code, they do appear in the code generator itself.  This will result in GHC builds bootstrapped with an affected compiler to produce incorrect binaries.


This bug poses a potentially significant inconvenience to users of architectures supported only by the LLVM code generator (e.g. ARM) as they will be unable to bootstrap 7.10 with a 7.8 release. The fix is implemented in  [5895f2b8ffba72a8393e9f712461e6e5ed7ceced](/trac/ghc/changeset/5895f2b8ffba72a8393e9f712461e6e5ed7ceced/ghc). A configure-time check to ensure an affected compiler isn't used as stage0 is implemented in [bbd031134a571c1020945b2548e3fc4795b5047a](/trac/ghc/changeset/bbd031134a571c1020945b2548e3fc4795b5047a/ghc). Both of these should be easily backported to the 7.8 branch.

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

## Status: closed (36 matches)

<table><tr><th>Ticket (Ticket query: milestone: 7.8.4, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: milestone: 7.8.4, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: milestone: 7.8.4, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: milestone: 7.8.4, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: milestone: 7.8.4, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#7068](https://gitlab.haskell.org//ghc/ghc/issues/7068)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Extensive Memory usage (regression)](https://gitlab.haskell.org//ghc/ghc/issues/7068)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#7143](https://gitlab.haskell.org//ghc/ghc/issues/7143)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.6.0.20120810-x86_64-windows.exe -\> ghc can't figure out LLVM version](https://gitlab.haskell.org//ghc/ghc/issues/7143)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Fanael</th></tr>
<tr><th>[\#7898](https://gitlab.haskell.org//ghc/ghc/issues/7898)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[SpecConstr explodes when compiling module BSP of frag-1.1.2](https://gitlab.haskell.org//ghc/ghc/issues/7898)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8819](https://gitlab.haskell.org//ghc/ghc/issues/8819)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[64bit Testsuite failures in unregisterised 7.8 RCs](https://gitlab.haskell.org//ghc/ghc/issues/8819)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8825](https://gitlab.haskell.org//ghc/ghc/issues/8825)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc can't determine gcc version on ru_RU locale](https://gitlab.haskell.org//ghc/ghc/issues/8825)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8849](https://gitlab.haskell.org//ghc/ghc/issues/8849)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Unregisterised compiler: arithmetic failure](https://gitlab.haskell.org//ghc/ghc/issues/8849)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8852](https://gitlab.haskell.org//ghc/ghc/issues/8852)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[7.8.1 uses a lot of memory when compiling attoparsec programs using \<\|\>](https://gitlab.haskell.org//ghc/ghc/issues/8852)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8951](https://gitlab.haskell.org//ghc/ghc/issues/8951)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[genSym uses atomic_inc but doesn't link arm_atomic_spin_lock](https://gitlab.haskell.org//ghc/ghc/issues/8951)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8960](https://gitlab.haskell.org//ghc/ghc/issues/8960)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[SpecConstr usage explodes beyond 4GB with GHC 7.8.1 rc 2](https://gitlab.haskell.org//ghc/ghc/issues/8960)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8980](https://gitlab.haskell.org//ghc/ghc/issues/8980)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.8.1 -O2 eats excessive amounts of RAM, highlighting-kate and pandoc-citeproc](https://gitlab.haskell.org//ghc/ghc/issues/8980)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8988](https://gitlab.haskell.org//ghc/ghc/issues/8988)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Documentation build fails if GHCi is unavailable](https://gitlab.haskell.org//ghc/ghc/issues/8988)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9006](https://gitlab.haskell.org//ghc/ghc/issues/9006)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC accepts import of private data constructor if it has the same name as the type](https://gitlab.haskell.org//ghc/ghc/issues/9006)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9295](https://gitlab.haskell.org//ghc/ghc/issues/9295)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Deadlock in forkProcess](https://gitlab.haskell.org//ghc/ghc/issues/9295)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9296](https://gitlab.haskell.org//ghc/ghc/issues/9296)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Acquire all_tasks_mutex in forkProcess](https://gitlab.haskell.org//ghc/ghc/issues/9296)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9303](https://gitlab.haskell.org//ghc/ghc/issues/9303)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[O2: (GHC version 7.8.3 for x86_64-unknown-linux): allocateRegsAndSpill: Cannot read from uninitialized register %vI_s1Mp](https://gitlab.haskell.org//ghc/ghc/issues/9303)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9316](https://gitlab.haskell.org//ghc/ghc/issues/9316)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.8.3 no longer infers correct type in presence of type families and constraints](https://gitlab.haskell.org//ghc/ghc/issues/9316)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9329](https://gitlab.haskell.org//ghc/ghc/issues/9329)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC panics when Cmm-compiling \`STK_CHK_GEN_N (8);\`](https://gitlab.haskell.org//ghc/ghc/issues/9329)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9336](https://gitlab.haskell.org//ghc/ghc/issues/9336)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[binutils gold linker detection does not work when called via gcc and selected by commandline parameters](https://gitlab.haskell.org//ghc/ghc/issues/9336)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9345](https://gitlab.haskell.org//ghc/ghc/issues/9345)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.List.inits is extremely slow](https://gitlab.haskell.org//ghc/ghc/issues/9345)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ekmett</th></tr>
<tr><th>[\#9371](https://gitlab.haskell.org//ghc/ghc/issues/9371)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Overlapping type families, segafult](https://gitlab.haskell.org//ghc/ghc/issues/9371)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9379](https://gitlab.haskell.org//ghc/ghc/issues/9379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Blocked STM transaction is not interruptible](https://gitlab.haskell.org//ghc/ghc/issues/9379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9390](https://gitlab.haskell.org//ghc/ghc/issues/9390)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Inlining prevents evaluation of ignored parts of unboxed tuples](https://gitlab.haskell.org//ghc/ghc/issues/9390)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9415](https://gitlab.haskell.org//ghc/ghc/issues/9415)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Superclass cycle with ambiguous type causes loop](https://gitlab.haskell.org//ghc/ghc/issues/9415)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#9417](https://gitlab.haskell.org//ghc/ghc/issues/9417)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms across modules broken in Haddock](https://gitlab.haskell.org//ghc/ghc/issues/9417)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9433](https://gitlab.haskell.org//ghc/ghc/issues/9433)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Partially applied type family allowed but unusable](https://gitlab.haskell.org//ghc/ghc/issues/9433)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9435](https://gitlab.haskell.org//ghc/ghc/issues/9435)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[x86 sse4.2 popCnt16\# needs to zero-extend its result](https://gitlab.haskell.org//ghc/ghc/issues/9435)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9439](https://gitlab.haskell.org//ghc/ghc/issues/9439)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[LlvmCodegen: Overzealous mangler incorrectly transforms user code](https://gitlab.haskell.org//ghc/ghc/issues/9439)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#9523](https://gitlab.haskell.org//ghc/ghc/issues/9523)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Typo in GHC Generics documentation](https://gitlab.haskell.org//ghc/ghc/issues/9523)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9552](https://gitlab.haskell.org//ghc/ghc/issues/9552)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[powerpc64 little endian: dll-split: Reachable modules from DynFlags out of date](https://gitlab.haskell.org//ghc/ghc/issues/9552)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9563](https://gitlab.haskell.org//ghc/ghc/issues/9563)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Support for deriving Generic1 for data families](https://gitlab.haskell.org//ghc/ghc/issues/9563)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>dreixel</th></tr>
<tr><th>[\#9575](https://gitlab.haskell.org//ghc/ghc/issues/9575)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-XAutoDeriveTypeable fails to generate instances](https://gitlab.haskell.org//ghc/ghc/issues/9575)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>dreixel</th></tr>
<tr><th>[\#9620](https://gitlab.haskell.org//ghc/ghc/issues/9620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[libffi.a is put in the wrong folder](https://gitlab.haskell.org//ghc/ghc/issues/9620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9658](https://gitlab.haskell.org//ghc/ghc/issues/9658)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Prettyprint constraints in type signatures can omit necessary parentheses](https://gitlab.haskell.org//ghc/ghc/issues/9658)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9705](https://gitlab.haskell.org//ghc/ghc/issues/9705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic on a pattern synonym in a class](https://gitlab.haskell.org//ghc/ghc/issues/9705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#9817](https://gitlab.haskell.org//ghc/ghc/issues/9817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[signal handlers in unix are passed garbage when using the signle threaded rts](https://gitlab.haskell.org//ghc/ghc/issues/9817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#9860](https://gitlab.haskell.org//ghc/ghc/issues/9860)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Package flags not command line completable in 7.8](https://gitlab.haskell.org//ghc/ghc/issues/9860)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>kolmodin</th></tr></table>