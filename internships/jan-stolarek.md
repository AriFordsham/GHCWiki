# Jan Stolarek's internship notes

# Back-end notes

## Some interesting tickets

- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605) - Optimisation: strict enumerations
- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498) - Optimisation: eliminate unnecessary heap check in recursive function. 
- [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600) - Optimisation: CPR the results of IO
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289) - Needless reboxing of values when returning from a tight loop
- [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387) - Optimizer misses unboxing opportunity
- [\#2450](https://gitlab.haskell.org//ghc/ghc/issues/2450) - Data.Complex.magnitude squares using <sup>(2 :: Int), which is slow
  </sup>
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731) - Avoid unnecessary evaluation when unpacking constructors
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823) - Another arity expansion bug
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470) - Loop optimization: identical counters
- [\#4937](https://gitlab.haskell.org//ghc/ghc/issues/4937) - Remove indirections caused by sum types, such as Maybe
- [\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567) - LLVM: Improve alias analysis / performance [BackEndNotes](back-end-notes#heap/stack-checks) page has some discussion of this.
- [\#7198](https://gitlab.haskell.org//ghc/ghc/issues/7198) - New codegen more than doubles compile time of T3294
- [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) - Register allocator chokes on certain branches with literals (bug can be triggered with `./inplace/bin/ghc-stage2 -c -no-hs-main -fasm -O2 ./testsuite/tests/llvm/should_compile/T7571.cmm`)
- [\#8048](https://gitlab.haskell.org//ghc/ghc/issues/8048) - Register spilling produces ineffecient/highly contending code

## Notes on the wiki

- [Commentary/Compiler/NewCodeGen/Cleanup](commentary/compiler/new-code-gen/cleanup)
- [Commentary/Compiler/NewCodeGenStupidity](commentary/compiler/new-code-gen-stupidity)
- [ cmm-notes](http://darcs.haskell.org/ghc/compiler/cmm/cmm-notes)

# Various stuff


Tickets that I could potentially look into:

- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070) - floor(0/0) should not be defined
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676) - realToFrac doesn't sanely convert between floating types
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744) - Comparisons against minBound/maxBound not optimised
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101) - Primitive constant unfolding
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615) - ghc produces poor code for `div` with constant powers of 2.
- [\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858) - Fix definitions of abs/signum for Floats/Doubles.


Some LLVM notes that may be useful:

- [LLVM Alias Notes](commentary/compiler/backends/llvm/alias)
- [ David Terei's LLVM blog post](http://blog.davidterei.com/2011/09/ghc-project-for-all.html)
- [ Max Bolingbroke's LLVM blog entry](http://blog.omega-prime.co.uk/?p=135)
- [ Implementation of various LLVM optimisations using Hoopl](https://github.com/mlite/HsLlvm) - **this seems very relevant**