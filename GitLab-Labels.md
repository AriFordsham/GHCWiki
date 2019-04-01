GitLab uses labels to organize both issues and merge requests. GHC uses this
mechanism to track a variety of information. Here we document the conventions
we use.

If you create a new label please do add it to this list, keeping the list in
alphabetical order.

# Types of issues

 * ~bug: This denotes an issue which describes a bug or infelicity
 * ~"feature request": This denotes an issue which describes a desired new feature.
 * ~task: This denotes an issue which describes a task of some sort (e.g. a refactoring that should be undertaken, a test which should be introduced, etc.)

# Types of bugs

These labels describe how a ~bug manifests. Usually every bug should be bear exactly one of the following.

 * ~"compiler perf": A case where compilation time isn't what we would like it to be
 * ~"runtime perf": A case where the code produced by GHC isn't as performant as we would like
 * ~"compiler crash": A case where GHC itself crashes
 * ~"ghc build failure": A case where GHC itself fails to build
 * ~"incorrect runtime result": A case where a program compiled by GHC produces the wrong result
 * ~"runtime crash": A case where a program compiled by GHC crashes at runtime
 * ~"error messages": Issues with the error messages produced by GHC

# Miscellaneous

 * ~"CI breakage": Denotes a bug that broke CI (this could likely be dropped; nothing should break CI now)
 * ~"user-facing": Denotes a merge request which changes the interface exposed by GHC or its core libraries in a user-noticeable way. Merge requests so-marked trigger the head.hackage build jobs.
 * ~"broken test": An issue due to which one or more tests in the testsuite are marked as `expect_broken`
 * ~"Trac import": Issues imported from GHC's old Trac instance
 * ~"report impact": Issues affect adherance to the [Haskell Report](/haskell-report)

# Issue lifecycle

These indicate various stages in the lifecycle of an issue.

 * ~"needs triage": An issue that hasn't yet been triaged. This involves a maintainer applying the appropriate labels and weight.
 * ~"info needed": Information is needed from the issue reporter
 * ~"upstream": Things that require coordination with an upstream package

## Backport status

These labels are to do with backporting of fixes to stable release branches.

 * ~"backport needed": This denotes a merge request which should be backported to a stable branch before the next release.
 * ~"backport": This denotes a merge request which backports a merge request to a stable branch. The merge request description should link to the MR or issue from which the backported patch came

# Bug environmental scope

These labels identify the environments in which a ~bug will manifest.

## Operating systems

 * ~macOS: Issues affecting Apple's macOS, iOS, of watchOS
 * ~Windows: Issues affecting Microsoft Windows

## Architectures

 * ~ARM: Issues affecting the ARM architecture
 * ~PowerPC: Issues affecting the PowerPC architecture
 * ~SPARC: Issues affecting the SPARC architecture

## Other

 * ~clang: Bugs triggered by GHC's interaction with the Clang C compiler (e.g. on Darwin)
 * ~cpp: Bugs related to use of the C preprocessor
 * ~cross-compilation: Bugs which only show up during cross-compilation
 * ~unregisterised: Issues with GHC's unregisterised C backend

# Bug subsystems

These labels identify compiler subsystems which are in some way implicated in a bug.

 * ~"ambiguity check": A bug in type ambiguity checking
 * ~"API annotations": TODO
 * ~backpack: The backpack module system
 * ~"code generation": Things pertaining to STG-to-Cmm code generation
 * ~"Data Parallel Haskell":  The (stagnated) Data Parallel Haskell project 
 * ~debugger: The GHCi debugger
 * ~"debug information": Native debugging information and backtrace support (e.g. DWARF) 
 * ~driver: The compiler driver
 * ~"GHC API": The API exposed by the `ghc` library
 * ~GHCi: The GHCi shell
 * ~hadrian: The Shake-based Hadrian build system
 * ~"I/O manager": GHC's I/O manager (also known as event manager).
 * ~"linking": Linker issues
 * ~"LLVM backend": LLVM backend (`-fllvm`)
 * ~"NCG backend": The native code generator backend (`-fasm`)
 * ~"nonmoving-gc": The low-latency non-moving garbage collector
 * ~"NUMA":  Runtime support for Non-Uniform Memory Architecture machines
 * ~numerics: Issues with pertaining to GHC's handling of numerics, in particular floating point arithmetic
 * ~"package system": Issues pertaining to handling of packages (also see ~backpack)
 * ~"parser": GHC's Haskell parser
 * ~"pattern match warnings":  GHC's pattern match checker for incomplete and overlapping patterns
 * ~"profiling": The cost-center heap and CPU profiler
 * ~"recompilation checking": GHC's recompilation avoidance mechanism
 * ~"remote GHCi": The GHCi external interpreter feature (`-fexternal-interpreter`)
 * ~"renamer": GHC's renamer
 * ~"RTS": The GHC runtime system
 * ~"simplifier": GHC's Core simplifier (see Optimisations section below for more specific labels)
 * ~"split-objs": GHC's object-splitting mechanism for reducing binary size (`-split-objs`; deprecated since 8.10)

## Optimisations

 * ~"CPR analysis": Constructed Product Result analysis
 * ~"CSE": Common Subexpression Elimination optimisation
 * ~"demand analysis": Demand (strictness) analysis
 * ~float-out: The binding float-out optimisation
 * ~float-in: The binding float-in optimisation
 * ~"inlining": Code inlining optimisationn
 * ~"join points": GHC's join points optimisation
 * ~"late lambda lifting": GHC's late lambda-lifting optimisation
 * ~"SpecConstr": The constructor specicialization optimisation
 * ~"specialisation":  Support for type specialisation of bindings (e.g. via `SPECIALISE` pragmas or automatic specialisation of unfolding) 
 * ~"static argument transformation":  The static argument transformation optimization
 * ~"typechecker": Issues with GHC's typechecker

## Non-compiler subsystems

 * ~infrastructure: GHC's code review, issue tracking, and continuous integration infrastructure.
 * ~"core libraries"
 * ~documentation: GHC's user guide and Haddock documentation.
 * ~ghc-pkg: The ghc-pkg utility
 * ~"ghc-heap": GHC's `ghc-heap` library for heap object introspection
 * ~"integer-gmp": GHC's `integer-gmp` bignum library
 * ~"integer-simple": GHC's `integer-simple` bignum library
 * ~"nofib": The nofib benchmark suite
 * ~"packaging": Issues with GHC's released source and binary packages
 * ~"typechecker plugins": GHC's typechecker plugin mechanism
 * ~plugins: The compiler's other plugin mechanisms (e.g. source plugins, Core-to-Core plugins)
 * ~testsuite: GHC's testsuite tests and driver

## External projects

These labels are generally relics of when GHC's bugtracker was also used to
track issues in external projects. New issues with these projects should rather
be filed against the appropriate upstream.

 * ~external-directory: The `directory` library ([upstream](https://github.com/haskell/directory/issues))
 * ~external-hoopl: The `hoopl` library ([upstream](https://github.com/haskell/hoopl/issues))
 * ~external-hsc2hs: The `hsc2hs` library ([upstream](https://github.com/haskell/hsc2hs/issues))
 * ~external-old-time: The `old-time` library ([upstream](https://github.com/haskell/old-time/issues))
 * ~external-pretty: The `pretty` library ([upstream](https://github.com/haskell/pretty/issues))
 * ~external-process: The `process` library ([upstream](https://github.com/haskell/process/issues))
 * ~external-random: The `random` library ([upstream](https://github.com/haskell/random/issues))
 * ~external-unix: The `unix` library ([upstream](https://github.com/haskell/unix/issues))
 * ~haddock: The Haddock documentation generator([upstream](https://github.com/haskell/haddock/issues))


# Language features

These labels identify user-visible language features which a bug is triggered by.

 * ~CAFs: Handling of Constant Applicative Forms
 * ~"compact normal forms": Compact normal forms (also known as compact regions)
 * ~concurrency: GHC's threaded runtime and concurrency primitives 
 * ~CUSKs:  Complete User-Specified Kind annotations
 * ~"custom type errors": Custom type errors support (e.g. the `TypeError` typeclass)
 * ~"deferred type errors": GHC's handling of deferred type errors (i.e. `-fdefer-type-errors`)
 * ~deriving: Typeclass deriving features 
 * ~exceptions: Synchronous and asynchronous exception support
 * ~FFI: The Foreign Function Interface
 * ~generics: GHC's data-type generics mechanism (e.g. `GHC.Generics`)
 * ~holes: Typed holes
 * ~hs-boot: Handling of `.hs-boot` files
 * ~"injective type families": Injective type family support
 * ~"levity polymorphism": Support for levity polymorphic terms
 * ~"partial type sigs": Partial type signatures
 * ~"records": Record syntax, field accessors, etc.
 * ~"roles": Type variable roles and role annotations
 * ~"rules": Rewrite rules
 * ~"Safe Haskell": Rewrite rules
 * ~"SIMD": Support for Single-Instruction/Multiple-Data instructions
 * ~STM: Software Transaction Memory support
 * ~"stream fusion": Issues affect stream fusion applications
 * ~"strings": Treatment of literal strings (~OverloadedStrings may also be relevant)
 * ~"Typeable": The `Typeable` typeclass and associated machinery
 * ~"typed holes": Typed holes
 * ~"Unicode": Issues with Unicode support
 * ~"visible dependent quantification"

## Language extensions

These labels correspond to the language extensions after which they are named.

 * ~ApplicativeDo
 * ~Arrows
 * ~ConstraintKinds
 * ~DataKinds
 * ~DefaultSignatures
 * ~DerivingVia
 * ~ExistentialQuantification
 * ~FunctionalDependencies
 * ~GADTs
 * ~ImplicitParams
 * ~ImpredicativeTypes
 * ~IncoherentInstances
 * ~InstanceSigs
 * ~MultiParamTypeClasses
 * ~OverlappingInstances
 * ~OverloadedRecordFields
 * ~OverloadedStrings
 * ~PatternSynonyms
 * ~PolyKinds
 * ~QuantifiedConstraints
 * ~RankNTypes
 * ~RebindableSyntax
 * ~RecordWildCards
 * ~ScopedTypeVariables
 * ~StaticPointers
 * ~TemplateHaskell
 * ~TypeApplications
 * ~TypeFamilies
 * ~TypeInType
 * ~UnboxedSums
 * ~UnboxedTuples
 * ~UndecidableSuperClasses
 * ~UndecideableInstances
 * ~ViewPatterns

