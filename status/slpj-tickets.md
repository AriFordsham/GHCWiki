# This page collects tickets that Simon PJ is interested in, so that he remembers them

## Type inference

- [\#8450](https://gitlab.haskell.org//ghc/ghc/issues/8450): rewriting wanteds with wanteds gives confusing error messages
- [\#7730](https://gitlab.haskell.org//ghc/ghc/issues/7730): display `TyCons` in tidied form in `pprTyThing`
- [\#5610](https://gitlab.haskell.org//ghc/ghc/issues/5610), [\#7243](https://gitlab.haskell.org//ghc/ghc/issues/7243): Better error messages for `foreign` declarations
- [\#8264](https://gitlab.haskell.org//ghc/ghc/issues/8264): `checkValidInstance` for `Typeable`, `Coercible` etc.
- [\#8240](https://gitlab.haskell.org//ghc/ghc/issues/8240): better error messages for type-function equalities
- [\#8095](https://gitlab.haskell.org//ghc/ghc/issues/8095): very slow constraint solving
- [\#8171](https://gitlab.haskell.org//ghc/ghc/issues/8171): extended default rules
- [\#8177](https://gitlab.haskell.org//ghc/ghc/issues/8177): role signature for type families
- [\#8161](https://gitlab.haskell.org//ghc/ghc/issues/8161): associated type more specific than class decl
- [\#7259](https://gitlab.haskell.org//ghc/ghc/issues/7259): Eta-expansion of products in System FC
- [\#5682](https://gitlab.haskell.org//ghc/ghc/issues/5682): Parsing of pomoted kinds
- [\#6022](https://gitlab.haskell.org//ghc/ghc/issues/6022): over-general types are inferred
- [\#5642](https://gitlab.haskell.org//ghc/ghc/issues/5642): slow constraint solving
- [\#7346](https://gitlab.haskell.org//ghc/ghc/issues/7346), [\#7878](https://gitlab.haskell.org//ghc/ghc/issues/7878): Pedro's generics and deriving
- [\#7828](https://gitlab.haskell.org//ghc/ghc/issues/7828), [\#5267](https://gitlab.haskell.org//ghc/ghc/issues/5267), [\#5777](https://gitlab.haskell.org//ghc/ghc/issues/5777), [\#5333](https://gitlab.haskell.org//ghc/ghc/issues/5333), [\#344](https://gitlab.haskell.org//ghc/ghc/issues/344): bugs in arrows
- [\#7204](https://gitlab.haskell.org//ghc/ghc/issues/7204): a class to controll FFI marshalling; and newtype deriving for classes with ATs
- [\#7862](https://gitlab.haskell.org//ghc/ghc/issues/7862): overlap beween givens and instances
- [\#7643](https://gitlab.haskell.org//ghc/ghc/issues/7643): dark corner of sub-kinding and `unsafeCoerce#`
- [\#7842](https://gitlab.haskell.org//ghc/ghc/issues/7842): polymorphism in recursive do-blocks
- [\#7730](https://gitlab.haskell.org//ghc/ghc/issues/7730): better display in GHCi, via `IfaceSyn`.

## Template Haskell


See also [TemplateHaskell](template-haskell)

- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): allow splicing of import declarations (5 in cc list)
- [\#8100](https://gitlab.haskell.org//ghc/ghc/issues/8100): support standalone deriving
- [\#4364](https://gitlab.haskell.org//ghc/ghc/issues/4364): cycle in type declarations
- [\#4429](https://gitlab.haskell.org//ghc/ghc/issues/4429): `reinerp` work on `newName` and export reorganisation
- [\#7021](https://gitlab.haskell.org//ghc/ghc/issues/7021): make TH catch up with constraint kinds
- [\#7484](https://gitlab.haskell.org//ghc/ghc/issues/7484): TH allows you to build invalid names

## Demand analysis and CSE

- [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782): late demand analysis
- [\#7994](https://gitlab.haskell.org//ghc/ghc/issues/7994): Make foldl into a good consumer; needs better arity analysis
- [\#7520](https://gitlab.haskell.org//ghc/ghc/issues/7520): Cardinality analysis
- [\#7619](https://gitlab.haskell.org//ghc/ghc/issues/7619): make demand and CPR analysis cope with data families
- [\#6070](https://gitlab.haskell.org//ghc/ghc/issues/6070), [\#5949](https://gitlab.haskell.org//ghc/ghc/issues/5949), [\#5775](https://gitlab.haskell.org//ghc/ghc/issues/5775), [\#4267](https://gitlab.haskell.org//ghc/ghc/issues/4267), [\#5302](https://gitlab.haskell.org//ghc/ghc/issues/5302), [\#6087](https://gitlab.haskell.org//ghc/ghc/issues/6087): Demand analysis
- [\#5075](https://gitlab.haskell.org//ghc/ghc/issues/5075), [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138): CPR for sum types (mostly done, needs finishing)
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289), [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387), [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600), [\#1855](https://gitlab.haskell.org//ghc/ghc/issues/1855): nested CPR analysis
- [\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596), [\#5996](https://gitlab.haskell.org//ghc/ghc/issues/5996), [\#149](https://gitlab.haskell.org//ghc/ghc/issues/149), [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940), [\#947](https://gitlab.haskell.org//ghc/ghc/issues/947), [\#701](https://gitlab.haskell.org//ghc/ghc/issues/701), [\#5344](https://gitlab.haskell.org//ghc/ghc/issues/5344): CSE opportunities        
- [\#6040](https://gitlab.haskell.org//ghc/ghc/issues/6040), [\#5945](https://gitlab.haskell.org//ghc/ghc/issues/5945), [\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458), [\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216): lambda-lift functions with a few args, just before code gen
- [\#1171](https://gitlab.haskell.org//ghc/ghc/issues/1171): strictness and exceptions (long, open ticket)

## Optimisation

- [\#8351](https://gitlab.haskell.org//ghc/ghc/issues/8351): inline allocation for small arrays
- [\#8275](https://gitlab.haskell.org//ghc/ghc/issues/8275): loopification and profiling
- [\#8335](https://gitlab.haskell.org//ghc/ghc/issues/8335): more gc entry points
- [\#8336](https://gitlab.haskell.org//ghc/ghc/issues/8336): sinking pass does not sink
- [\#8327](https://gitlab.haskell.org//ghc/ghc/issues/8327): dead code (in heap checks) not eliminated by C--
- [\#8317](https://gitlab.haskell.org//ghc/ghc/issues/8317), [\#8326](https://gitlab.haskell.org//ghc/ghc/issues/8326): don't push heapchecks into case branches too aggressively
- [\#8308](https://gitlab.haskell.org//ghc/ghc/issues/8308): resurrect ticky histograms
- [\#8321](https://gitlab.haskell.org//ghc/ghc/issues/8321): branch prediction for stack/heap checks for LLVM
- [\#8279](https://gitlab.haskell.org//ghc/ghc/issues/8279): alignment and its effect on performance
- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498): heap checks in recursive functions
- [\#7367](https://gitlab.haskell.org//ghc/ghc/issues/7367): float-out causes extra allocation
- [\#7378](https://gitlab.haskell.org//ghc/ghc/issues/7378): (a) identical-alts when the RHSs are bigger; (b) nested comparisions (cf [\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132))
- [\#7511](https://gitlab.haskell.org//ghc/ghc/issues/7511): inlining can make programs allocate MORE!!
- [\#7307](https://gitlab.haskell.org//ghc/ghc/issues/7307): low hanging fruit for shrinking code size for string constants and top-level indirections
- [\#7206](https://gitlab.haskell.org//ghc/ghc/issues/7206), [\#7309](https://gitlab.haskell.org//ghc/ghc/issues/7309): avoid excessive sharing of enumerations
- [\#3872](https://gitlab.haskell.org//ghc/ghc/issues/3872), [\#5400](https://gitlab.haskell.org//ghc/ghc/issues/5400), [\#5448](https://gitlab.haskell.org//ghc/ghc/issues/5448), [\#5722](https://gitlab.haskell.org//ghc/ghc/issues/5722), [\#7057](https://gitlab.haskell.org//ghc/ghc/issues/7057), [\#7369](https://gitlab.haskell.org//ghc/ghc/issues/7369): contravariant data type loop in simplifier inliner
- [\#5954](https://gitlab.haskell.org//ghc/ghc/issues/5954): performance regression. Duplication of primops?
- [\#6047](https://gitlab.haskell.org//ghc/ghc/issues/6047): GHC retains unnecessary binding
- [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135): unboxed booleans
- [\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439): Strict dictionaries
- [\#5916](https://gitlab.haskell.org//ghc/ghc/issues/5916): `runST` isn't free
- [\#5522](https://gitlab.haskell.org//ghc/ghc/issues/5522), [\#6092](https://gitlab.haskell.org//ghc/ghc/issues/6092): liberate-case runs out of memory
- [\#3990](https://gitlab.haskell.org//ghc/ghc/issues/3990), [\#7647](https://gitlab.haskell.org//ghc/ghc/issues/7647): UNPACK for data families, and with phantom types, needs data con wrapper refactoring
- [\#5928](https://gitlab.haskell.org//ghc/ghc/issues/5928): running Specialise more than once

## Compile time

- [\#7898](https://gitlab.haskell.org//ghc/ghc/issues/7898): `SpecConstr` blowup
- [\#7450](https://gitlab.haskell.org//ghc/ghc/issues/7450), [\#7258](https://gitlab.haskell.org//ghc/ghc/issues/7258): deriving `Read` generates gigantic code. Better now, but still not linear.
- [\#7428](https://gitlab.haskell.org//ghc/ghc/issues/7428): Non-linear compile time: `addFingerprint`??

## Features

- [\#5972](https://gitlab.haskell.org//ghc/ghc/issues/5972): option to suppress record selectors

---

### Arity

- [\#7542](https://gitlab.haskell.org//ghc/ghc/issues/7542): optimisation of eta expansion/reduction
- [\#5809](https://gitlab.haskell.org//ghc/ghc/issues/5809): arity analysis could be better
- [\#3924](https://gitlab.haskell.org//ghc/ghc/issues/3924): weak arity leads to weak strictness
- [\#3698](https://gitlab.haskell.org//ghc/ghc/issues/3698): arity analysis again
- [\#3697](https://gitlab.haskell.org//ghc/ghc/issues/3697): class method selectors
- [\#3034](https://gitlab.haskell.org//ghc/ghc/issues/3034): divInt and arity
- [\#2915](https://gitlab.haskell.org//ghc/ghc/issues/2915): arity too small
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823): another arity expansion bug (related to dictionaries)
- [\#2440](https://gitlab.haskell.org//ghc/ghc/issues/2440): bad code with type families; I believe this is also arity-related
- [\#1547](https://gitlab.haskell.org//ghc/ghc/issues/1547): profiling and arity
- [\#5587](https://gitlab.haskell.org//ghc/ghc/issues/5587), [\#7364](https://gitlab.haskell.org//ghc/ghc/issues/7364): eta-expansion can imcrease termination.  These tickets are closed because we don't know how to fix them properly, not becuase they are really fixed.

### Inlining

- [\#4833](https://gitlab.haskell.org//ghc/ghc/issues/4833): finding the right loop breaker
- [\#3781](https://gitlab.haskell.org//ghc/ghc/issues/3781), [\#3755](https://gitlab.haskell.org//ghc/ghc/issues/3755): inlining for local functions: discount for scrutinising free vars
- [\#3765](https://gitlab.haskell.org//ghc/ghc/issues/3765): CONLIKE things and case expressions (need two unfoldings)
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073) and [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances): better desugaring for instances
- [\#2840](https://gitlab.haskell.org//ghc/ghc/issues/2840): top level unlifted string literals
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123): feature request: recursive inlining and peeling
- [\#4960](https://gitlab.haskell.org//ghc/ghc/issues/4960): better inlining tests

## New code generator

- Summary ticket: [\#4258](https://gitlab.haskell.org//ghc/ghc/issues/4258), and [wiki page](commentary/compiler/new-code-gen)
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462): allocate large objects with `allocateLocal()`
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): cheap check at start of case alternatives
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731): avoiding unnecessary evaluation when unpacking constructors

### Optimiser

- [\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059): specialise on value arguments
- [\#5974](https://gitlab.haskell.org//ghc/ghc/issues/5974): casts, RULES, and parametricity
- [\#5262](https://gitlab.haskell.org//ghc/ghc/issues/5262): seq magic
- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605): strict/unboxed enumerations
- [\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607): space leak: inlining defeats selector thunk optimisation
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470): merge identical counters
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988): better float-in
- [\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255), [\#3767](https://gitlab.haskell.org//ghc/ghc/issues/3767), [\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642), [\#3831](https://gitlab.haskell.org//ghc/ghc/issues/3831), [\#4941](https://gitlab.haskell.org//ghc/ghc/issues/4941), [\#2598](https://gitlab.haskell.org//ghc/ghc/issues/2598): Improve **`SpecConstr`** in various ways, including for free variables, and for join points.
- [\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374): SAT and `MutableByteArray`        Max?
- [\#7080](https://gitlab.haskell.org//ghc/ghc/issues/7080): inconsistent treatment of RULES and SPECIALISE
- [\#876](https://gitlab.haskell.org//ghc/ghc/issues/876): make `length` into a good consumer. Perhaps using `foldl`?

### Compiler performance

- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings
- Use wildcards for dead variables in interface files.

## Better numerics

- [\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858): `abs` on negative zero
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615): poor code for div with powers of 2
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101): constant folding for `(**)`
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676): `realToFrac` conversions
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744): comparisons against `minBound` and `maxBound` are not optimised away
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065): quot is sub-optimal
- [\#2269](https://gitlab.haskell.org//ghc/ghc/issues/2269): Word type to Double or Float conversions 
- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070): NaNs and divide-by-zero

## GHCi

- [\#7586](https://gitlab.haskell.org//ghc/ghc/issues/7586): `:kind` on a name should perhaps behave like `:info`
- [\#4017](https://gitlab.haskell.org//ghc/ghc/issues/4017): unhelpful GHCi message

---

## Outright bugs

- [\#2182](https://gitlab.haskell.org//ghc/ghc/issues/2182): GHCi session retains instance after removing a module from scope        
- [\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241): Lifting the Coverage Condition for functional dependencies isn’t the Right Thing        Manuel

---

## Type inference (longer term)

- [TypeFunctionsStatus](type-functions-status)
- [\#7908](https://gitlab.haskell.org//ghc/ghc/issues/7908): Kind polymorphism notation
- [\#7503](https://gitlab.haskell.org//ghc/ghc/issues/7503): Kind polymorphism and mutual recursion
- [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018), [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): Injective type families and type family overlap
- [\#4296](https://gitlab.haskell.org//ghc/ghc/issues/4296): SkolemOccurs
- [\#816](https://gitlab.haskell.org//ghc/ghc/issues/816). [\#3108](https://gitlab.haskell.org//ghc/ghc/issues/3108): extreme delicacy in solve order, involving fundeps

## Types and type inference

- [\#8109](https://gitlab.haskell.org//ghc/ghc/issues/8109): as-patterns in type-family declarations
- [\#6065](https://gitlab.haskell.org//ghc/ghc/issues/6065): GHC suggests a type signature that it then rejects
- [\#5320](https://gitlab.haskell.org//ghc/ghc/issues/5320), [\#7296](https://gitlab.haskell.org//ghc/ghc/issues/7296): overlap delicacy
- [\#4347](https://gitlab.haskell.org//ghc/ghc/issues/4347), [\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295), [\#7264](https://gitlab.haskell.org//ghc/ghc/issues/7264): impredicativity
- [\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965): make existentials have a really cheap implementation
- [\#5224](https://gitlab.haskell.org//ghc/ghc/issues/5224): make it cheaper to check for inconsistent type family instances
- [\#4466](https://gitlab.haskell.org//ghc/ghc/issues/4466), [\#5296](https://gitlab.haskell.org//ghc/ghc/issues/5296): explicit type application
- [\#3714](https://gitlab.haskell.org//ghc/ghc/issues/3714): distinguish type parameters from indices
- [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): overlapping type family instances
- [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490): superclasses and ambiguity
- [\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632): better update for records with existentials
- [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496), [\#4846](https://gitlab.haskell.org//ghc/ghc/issues/4846), [\#7148](https://gitlab.haskell.org//ghc/ghc/issues/7148), [\#7269](https://gitlab.haskell.org//ghc/ghc/issues/7269): Newtype deriving and type families type soundness problem
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641): revise what `-XExtendedDefaultRules` does

### Better error messages

- [\#7856](https://gitlab.haskell.org//ghc/ghc/issues/7856): function type decomposed a bit too eagerly =\> worse error
- [\#4921](https://gitlab.haskell.org//ghc/ghc/issues/4921): ambiguous type variables
- [\#1330](https://gitlab.haskell.org//ghc/ghc/issues/1330): another bad error message (Church2)
- [\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648): Report out of date interface files robustly        
- [\#1928](https://gitlab.haskell.org//ghc/ghc/issues/1928): Confusing type error message (Claus makes suggestions)        

---

## Template Haskell

- Blog post: [ http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)
- [\#1012](https://gitlab.haskell.org//ghc/ghc/issues/1012): mutually recursive modules and TH
- [\#5959](https://gitlab.haskell.org//ghc/ghc/issues/5959): top level splices and renaming
- [\#5416](https://gitlab.haskell.org//ghc/ghc/issues/5416): local modules
- [\#4364](https://gitlab.haskell.org//ghc/ghc/issues/4364): type synonym loop
- [\#4372](https://gitlab.haskell.org//ghc/ghc/issues/4372): better quasiquotation support
- [\#2041](https://gitlab.haskell.org//ghc/ghc/issues/2041): Splicing in concrete syntax
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Allow TH to generate import declarations        
- [\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340): Better TH error recovery.  Easy to do; change to signature of qRecover.
- [\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222): design of reification for TH

## Features

- [\#5462](https://gitlab.haskell.org//ghc/ghc/issues/5462): deriving for arbitrary classes
- [\#6024](https://gitlab.haskell.org//ghc/ghc/issues/6024): allow defining a *kind* without also getting the corresponding *type*
- [\#5927](https://gitlab.haskell.org//ghc/ghc/issues/5927): Constraints with universal quantification
- [\#5630](https://gitlab.haskell.org//ghc/ghc/issues/5630): External core needs love
- [\#5429](https://gitlab.haskell.org//ghc/ghc/issues/5429): docase and joinads
- [\#5391](https://gitlab.haskell.org//ghc/ghc/issues/5391): better deriving Typeable
- [\#5248](https://gitlab.haskell.org//ghc/ghc/issues/5248): Infer context in type signatures
- [\#5144](https://gitlab.haskell.org//ghc/ghc/issues/5144): pattern synonyms
- [\#5073](https://gitlab.haskell.org//ghc/ghc/issues/5073): `blockST` and friends
- [\#2895](https://gitlab.haskell.org//ghc/ghc/issues/2895): Class aliases 
- [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595): record updates
- [\#4823](https://gitlab.haskell.org//ghc/ghc/issues/4823): strength reduction for array indexing
- [\#4479](https://gitlab.haskell.org//ghc/ghc/issues/4479): type directed name resolution (TDNR)
- [\#4426](https://gitlab.haskell.org//ghc/ghc/issues/4426): simpler rule for implicit quantification
- [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595), [\#5728](https://gitlab.haskell.org//ghc/ghc/issues/5728), [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927), [\#5724](https://gitlab.haskell.org//ghc/ghc/issues/5724), [\#5762](https://gitlab.haskell.org//ghc/ghc/issues/5762), [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139), [\#6124](https://gitlab.haskell.org//ghc/ghc/issues/6124), [\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669), [\#322](https://gitlab.haskell.org//ghc/ghc/issues/322), [\#8016](https://gitlab.haskell.org//ghc/ghc/issues/8016): **pattern-match overlap checking**, including with GADTs
- [\#3701](https://gitlab.haskell.org//ghc/ghc/issues/3701): Implicitly declared existentials: a class as a type
- [\#3217](https://gitlab.haskell.org//ghc/ghc/issues/3217): better flag handling for GHCi
- [\#2600](https://gitlab.haskell.org//ghc/ghc/issues/2600), [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110): **Bind type variables and constraints in RULES**
- [\#1930](https://gitlab.haskell.org//ghc/ghc/issues/1930): Infix type operators:  a+b.        
- [\#960](https://gitlab.haskell.org//ghc/ghc/issues/960): Generate local info for ‘undefined’.  Implicit location parameters in general        
- [\#2135](https://gitlab.haskell.org//ghc/ghc/issues/2135): Warn when exporting a function whose type mentions a type constructor defined locally but not itself exported        
- [\#2526](https://gitlab.haskell.org//ghc/ghc/issues/2526): Add -fwarn-missing-export-signatures        
- [\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119): Explicitly importing deprecated symbols should generate deprecation warnings        
- [\#2207](https://gitlab.haskell.org//ghc/ghc/issues/2207): Load interfaces for GHC.\* even without -O        
- [\#1231](https://gitlab.haskell.org//ghc/ghc/issues/1231): Better deprecations        

---

## Not sure what to do about these

- [\#7897](https://gitlab.haskell.org//ghc/ghc/issues/7897): make the fingerprint in a `TypeRep` be a proper fingerprint, including the type definition all the way down (see [ Recompilation avoidance](http://hackage.haskell.org/trac/ghc/wiki/DependencyTracking))
- [\#2284](https://gitlab.haskell.org//ghc/ghc/issues/2284), [\#1168](https://gitlab.haskell.org//ghc/ghc/issues/1168), [\#7561](https://gitlab.haskell.org//ghc/ghc/issues/7561): the state-hack "optimisation" causes much re-computation
- [\#4005](https://gitlab.haskell.org//ghc/ghc/issues/4005): generational GC pathalogical case
- [\#1349](https://gitlab.haskell.org//ghc/ghc/issues/1349): strict function argument types
