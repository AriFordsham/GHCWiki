
This list overviews the active branches in the main GHC repository.

# Active branches

- `dicts-strict`: **Strict Dictionaries**.  Nick Frisby. Make most dictionary arguments strict. It's stowed in a branch until `cardinality` is merged. It should be easier to implement after `cardinality`'s refactoring.

- `late-dmd`: **Late Demand Analysis**. Nick Frisby. See [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782). It's stowed in a branch become of some nofib slowdowns I'm struggling to understand.

- `late-lam-lift`: **Late Lambda Lift**.  Nick Frisby. We lift some lambdas before CorePrep. I'm still determining when to 'not' lift a lambda. My terse notes [here](late-lam-lift).

- `ticky-for-all-lets`: **Ticky for all `let`s**.  Nick Frisby. I think this patch is in mostly good shape, but I had to revert it because of a silly mistake. Need to double-check it. It adds ticky counters for lets, even those that don't end up as proper closures.

- `type-nats`: **Solver for Type-Level Naturals**. Iavor S. Diatchki.  An experimental solver for discharging constraints involving natural numbers.

- `atomics`: **Atomic memory operations**. Ryan Newton. This expands on some work Simon Marlow started in 7.2.

- `simd`: **SIMD support**. Geoffrey Mainland. This extends the simple SIMD support in GHC HEAD, and is slated for 7.8.

- `th-new`: **New Template Haskell**. Geoffrey Mainland. This revamps Template Haskell, based on Simon PJ's proposal. See [ http://ghc.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal](http://ghc.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)

- `ghc-lwc2`: **Lightweight concurrency substrate support**. KC Sivaramakrishnan. This implements an evolution of Peng Li's original concept of implementing a lot of the concurrency substrate in the RTS *in Haskell*. The design is described in a paper, "Composable Scheduler Activations for Haskell", found here: [ http://research.microsoft.com/en-us/um/people/simonpj/papers/lw-conc/lwc-hs13.pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/lw-conc/lwc-hs13.pdf).

# Limbo branches


These branches are not merged, but they are also not totally dead ended, and their status is currently uncertain.

- `coloured-core`: **Support for terminal color codes in `-ddump-simpl` output**. Thomas Schilling.

- `supercompiler`: **Max's Supercompiler**. Max Bolingbroke. This implements the ideas present in Max's PhD thesis, seen here: [ http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-835.html](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-835.html)

- `local-gc`: **Capability-local garbage collection**. Simon Marlow & Simon PJ. As described in "Multicore Garbage Collection with Local Heaps": [ http://community.haskell.org/\~simonmar/papers/local-gc.pdf](http://community.haskell.org/~simonmar/papers/local-gc.pdf)

# Uncertain


The actual status of these branches, including whether they have been merged and/or superseded, is not clear.

- `cpr-sum-types`: **CPR for sum types**. Simon PJ.

- `tc-arrows`: **Rearrange the typechecking of arrows, especially arrow "forms"**. Simon PJ & Ross Patterson.

- `cross-compiler-alienless`: **Better support for cross compilation**? Gabor Grief.

- `srcloc` and \`real-src-loc-span: **Amendments to spannings and SrcLocs**. Ian Lynagh.

- `ghc-spj`: **???**. Simon PJ.

# Dead/merged branches


This is a list of inactive branches which have been merged into the tree.

- `ghc-axioms`: **First attempt at branched type family instances**. Richard Eisenberg / José Pedro Magalhães / Simon PJ. Last commit was Jan 3, 2012. This was an early attempt at implementing branched type family instances. Superseded by `overlapping-tyfams`.

- `overlapping-tyfams`: **Proper implementation of branched type family instances**.  Richard Eisenberg. Last commit was Dec 21, 2012. Merged into master on Dec 21, 2012, as commit [8366792eede3c8eb486ff15d8c8e62e9363f1959](/trac/ghc/changeset/8366792eede3c8eb486ff15d8c8e62e9363f1959/ghc). See [NewAxioms](new-axioms).

- `type-reasoning` on *base*: ** Experiments with type-level reasoning definitions**.  Richard Eisenberg / Gabor Greif. Changes to TypeLits and related files to support type-level reasoning. Merged into master on July 24th by Richard.

- `at-defaults`: **Associated-type defaults**. Max Bolingbroke. Merged into the 7.4 release.

- `cardinality`: **Cardinality Analysis**. Ilya Sergey. This will be part of the 7.8 release.

- `encoding`: **Better UTF8 encoding support**. Max Bolingbroke. This was merged into 7.4

- `type-holes-branch`: **Support for Type Holes**. Simon PJ, Sean Leather and Thijs Alkemade. This will be part of the 7.8 release.

- `imp-param-class`: **Turn Implicit Parameters into functional dependencies using [TypeNats](type-nats)**. Iavor Diatchki. Merged and will be part of 7.8(?)

- `unboxed-tuple-arguments`: **Extending -XUnboxedTuples**. Max Bolingbroke. Merged into 7.6.

- `ghc-new-co`: **New GHC Coercions**. Simon PJ. Merged into 7.4(?)

- `ghc-constraint-solver`: **New constraint solver**. Simon & Dimitrios. Merged into 7.4(?)

- `patch-5084`: **Fix for [\#5084](https://gitlab.haskell.org//ghc/ghc/issues/5084)**. Max Bolingbroke. SimonPJ fixed it differently for 7.4(?)

- `known-key-serialization`: **Iface serialization changes**. Max Bolingbroke. Merged in 7.4(?)

- `profiling`: **Profiling infrastructure overhaul**. Simon Marlow. Merged in 7.4(?)

- `ghc-defer`: **Deferred type errors**. Simon PJ. Merged in 7.6.

- `silent-sc-args`: **Silent superclass parameters**. Simon PJ. Merged in 7.6.

- `new-demand-to-merge`: **New demand analyzer**. Ilya Sergey. Will be part of 7.8 release.

- `patch-7704`: **Fix for [\#7704](https://gitlab.haskell.org//ghc/ghc/issues/7704)**. José Pedro Magalhães. Will be part of 7.8.
