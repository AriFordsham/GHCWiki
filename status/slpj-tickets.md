# This page collects tickets that Simon PJ is interested in, so that he remembers them

## New typechecker things to fix (with Dimitrios)

- [\#5934](https://gitlab.haskell.org//ghc/ghc/issues/5934): Bad substitution for flatten skolem
- [\#5913](https://gitlab.haskell.org//ghc/ghc/issues/5913), [\#5751](https://gitlab.haskell.org//ghc/ghc/issues/5751): type-class dictionary loops
- [\#5321](https://gitlab.haskell.org//ghc/ghc/issues/5321), [\#5837](https://gitlab.haskell.org//ghc/ghc/issues/5837), [\#5853](https://gitlab.haskell.org//ghc/ghc/issues/5853) (loop), [\#5970](https://gitlab.haskell.org//ghc/ghc/issues/5970), [\#3064](https://gitlab.haskell.org//ghc/ghc/issues/3064), [\#3108](https://gitlab.haskell.org//ghc/ghc/issues/3108): slow constraint solving
- [\#5612](https://gitlab.haskell.org//ghc/ghc/issues/5612), [\#5682](https://gitlab.haskell.org//ghc/ghc/issues/5682): PolyKinds
- [\#4347](https://gitlab.haskell.org//ghc/ghc/issues/4347), [\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295): impredicativity
- [\#5320](https://gitlab.haskell.org//ghc/ghc/issues/5320): overlap delicacy
- [\#5939](https://gitlab.haskell.org//ghc/ghc/issues/5939), [\#5936](https://gitlab.haskell.org//ghc/ghc/issues/5936): Pedro's generics

### Arity

- [\#5809](https://gitlab.haskell.org//ghc/ghc/issues/5809): arity analysis could be better
- [\#3924](https://gitlab.haskell.org//ghc/ghc/issues/3924): weak arity leads to weak strictness
- [\#3698](https://gitlab.haskell.org//ghc/ghc/issues/3698): arity analysis again
- [\#3697](https://gitlab.haskell.org//ghc/ghc/issues/3697): class method selectors
- [\#3034](https://gitlab.haskell.org//ghc/ghc/issues/3034): divInt and arity
- [\#2915](https://gitlab.haskell.org//ghc/ghc/issues/2915): arity too small
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823): another arity expansion bug (related to dictionaries)
- [\#2440](https://gitlab.haskell.org//ghc/ghc/issues/2440): bad code with type families; I believe this is also arity-related
- [\#1547](https://gitlab.haskell.org//ghc/ghc/issues/1547): profiling and arity

### Inlining

- [\#3872](https://gitlab.haskell.org//ghc/ghc/issues/3872), [\#5400](https://gitlab.haskell.org//ghc/ghc/issues/5400), [\#5448](https://gitlab.haskell.org//ghc/ghc/issues/5448), [\#5722](https://gitlab.haskell.org//ghc/ghc/issues/5722): contravariant data type loop in simplifier inliner
- [\#4833](https://gitlab.haskell.org//ghc/ghc/issues/4833): finding the right loop breaker
- [\#3781](https://gitlab.haskell.org//ghc/ghc/issues/3781), [\#3755](https://gitlab.haskell.org//ghc/ghc/issues/3755): inlining for local functions: discount for scrutinising free vars
- [\#3765](https://gitlab.haskell.org//ghc/ghc/issues/3765): CONLIKE things and case expressions (need two unfoldings)
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073) and [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances): better desugaring for instances
- [\#2078](https://gitlab.haskell.org//ghc/ghc/issues/2078): INLINing improvement; ask Christian.Maeder@… to see if it improves CASL
- [\#2840](https://gitlab.haskell.org//ghc/ghc/issues/2840): top level unlifted string literals
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123): feature request: recursive inlining and peeling

## New code generator

- Summary ticket: [\#4258](https://gitlab.haskell.org//ghc/ghc/issues/4258), and [wiki page](commentary/compiler/new-code-gen)
- [\#4065](https://gitlab.haskell.org//ghc/ghc/issues/4065): forward propagation
- [\#3132](https://gitlab.haskell.org//ghc/ghc/issues/3132): fix cgCase of PrimAlts
- [\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458), [\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216): undesirable allocation of something that is nearly a let-no-escape
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462): allocate large objects with `allocateLocal()`
- [\#2253](https://gitlab.haskell.org//ghc/ghc/issues/2253): NCG could do better. Look at this when John D’s ncg is working
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): cheap check at start of case alternatives
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731): avoiding unnecessary evaluation when unpacking constructors
- [\#3940](https://gitlab.haskell.org//ghc/ghc/issues/3940): propagate fix

### Optimiser

- [\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059): specialise on value arguments
- [\#5964](https://gitlab.haskell.org//ghc/ghc/issues/5964): casts and RULES interaction
- [\#5262](https://gitlab.haskell.org//ghc/ghc/issues/5262): seq magic
- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605): strict/unboxed enumerations
- [\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607): space leak: inlining defeats selector thunk optimisation
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470): merge identical counters
- [\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439): Strict dictionaries
- [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138): returning a known constructor (Lennart's cmonad package)
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988): better float-in
- [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940): do CSE after `CorePrep`
- [\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132): Optimise nested comparisons: if you know x\>0 then you know x\>=1 etc.  Maybe a special pass that knows about arithmetic?        
- [\#149](https://gitlab.haskell.org//ghc/ghc/issues/149): float-out/CSE        
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289), [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387), [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600): nested CPR analysis        
- [\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255), [\#3767](https://gitlab.haskell.org//ghc/ghc/issues/3767), [\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642), [\#3831](https://gitlab.haskell.org//ghc/ghc/issues/3831), [\#4941](https://gitlab.haskell.org//ghc/ghc/issues/4941): Improve **`SpecConstr`** for free variables, and for join points.
- [\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374): SAT and `MutableByteArray`        Max?

### Compiler performance

- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings
- Use wildcards for dead variables in interface files.

## Tiresome arithmetic things

- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615): poor code for div with powers of 2
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101): constant folding for `(**)`
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676): `realToFrac` conversions
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744): comparisons against `minBound` and `maxBound` are not optimised away
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065): quot is sub-optimal
- [\#2269](https://gitlab.haskell.org//ghc/ghc/issues/2269): Word type to Double or Float conversions 

## GHCi

- [\#4017](https://gitlab.haskell.org//ghc/ghc/issues/4017): unhelpful GHCi message

---

## Outright bugs

- [\#2182](https://gitlab.haskell.org//ghc/ghc/issues/2182): GHCi session retains instance after removing a module from scope        
- [\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241): Lifting the Coverage Condition for functional dependencies isn’t the Right Thing        Manuel

---

## New constraint simplifier

- [TypeFunctionsStatus](type-functions-status)
- [\#4296](https://gitlab.haskell.org//ghc/ghc/issues/4296): SkolemOccurs
- [\#816](https://gitlab.haskell.org//ghc/ghc/issues/816). [\#3108](https://gitlab.haskell.org//ghc/ghc/issues/3108): extreme delicacy in solve order, involving fundeps
- [\#4175](https://gitlab.haskell.org//ghc/ghc/issues/4175): better GHCi info for type-function instances

## Types and type inference

- [\#5267](https://gitlab.haskell.org//ghc/ghc/issues/5267), [\#5609](https://gitlab.haskell.org//ghc/ghc/issues/5609), [\#5777](https://gitlab.haskell.org//ghc/ghc/issues/5777): bugs in arrows; awaiting refactoring by Dan
- [\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965): make existentials have a really cheap implementation
- [\#5224](https://gitlab.haskell.org//ghc/ghc/issues/5224): make it cheaper to check for inconsistent type family instances
- [\#4466](https://gitlab.haskell.org//ghc/ghc/issues/4466), [\#5296](https://gitlab.haskell.org//ghc/ghc/issues/5296): explicit type application
- [\#3714](https://gitlab.haskell.org//ghc/ghc/issues/3714): distinguish type parameters from indices
- [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): overlapping type family instances
- [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490): superclasses and ambiguity
- [\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632): better update for records with existentials
- [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496), [\#4846](https://gitlab.haskell.org//ghc/ghc/issues/4846): Newtype deriving and type families type soundness problem
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641): revise what `-XExtendedDefaultRules` does

### Better error messages

- [\#4921](https://gitlab.haskell.org//ghc/ghc/issues/4921): ambiguous type variables
- [\#1330](https://gitlab.haskell.org//ghc/ghc/issues/1330): another bad error message (Church2)
- [\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648): Report out of date interface files robustly        
- [\#2588](https://gitlab.haskell.org//ghc/ghc/issues/2588): Better error message about ‘forall’        
- [\#1928](https://gitlab.haskell.org//ghc/ghc/issues/1928): Confusing type error message (Claus makes suggestions)        
- [\#2534](https://gitlab.haskell.org//ghc/ghc/issues/2534): Another confusing type error message        
- [\#2900](https://gitlab.haskell.org//ghc/ghc/issues/2900): Improve decomposition of function types

---

## Template Haskell

- Blog post: [ http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)
- [\#5959](https://gitlab.haskell.org//ghc/ghc/issues/5959): top level splices and renaming
- [\#5416](https://gitlab.haskell.org//ghc/ghc/issues/5416): local modules
- [\#4364](https://gitlab.haskell.org//ghc/ghc/issues/4364): type synonym loop
- [\#4372](https://gitlab.haskell.org//ghc/ghc/issues/4372): better quasiquotation support
- [\#2041](https://gitlab.haskell.org//ghc/ghc/issues/2041): Splicing in concrete syntax
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Allow TH to generate import declarations        
- [\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340): Better TH error recovery.  Easy to do; change to signature of qRecover.
- [\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222): design of reification for TH

## Features

- [\#5927](https://gitlab.haskell.org//ghc/ghc/issues/5927): Constraints with universal quantification
- [\#5630](https://gitlab.haskell.org//ghc/ghc/issues/5630): External core needs love
- [\#5429](https://gitlab.haskell.org//ghc/ghc/issues/5429): docase and joinads
- [\#5391](https://gitlab.haskell.org//ghc/ghc/issues/5391): better deriving Typeable
- [\#5248](https://gitlab.haskell.org//ghc/ghc/issues/5248): Infer context in type signatures
- [\#5144](https://gitlab.haskell.org//ghc/ghc/issues/5144): pattern synonyms
- [\#5073](https://gitlab.haskell.org//ghc/ghc/issues/5073): `blockST` and friends
- [\#788](https://gitlab.haskell.org//ghc/ghc/issues/788), [\#2895](https://gitlab.haskell.org//ghc/ghc/issues/2895): Class aliases 
- [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595): record updates
- [\#4823](https://gitlab.haskell.org//ghc/ghc/issues/4823): strength reduction for array indexing
- [\#4479](https://gitlab.haskell.org//ghc/ghc/issues/4479): type directed name resolution (TDNR)
- [\#4426](https://gitlab.haskell.org//ghc/ghc/issues/4426): simpler rule for implicit quantification
- [\#4359](https://gitlab.haskell.org//ghc/ghc/issues/4359): lambda case
- [\#4148](https://gitlab.haskell.org//ghc/ghc/issues/4148): improvements to mdo syntax
- [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595), [\#5728](https://gitlab.haskell.org//ghc/ghc/issues/5728), [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927), [\#5724](https://gitlab.haskell.org//ghc/ghc/issues/5724), [\#5762](https://gitlab.haskell.org//ghc/ghc/issues/5762), [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139): **pattern-match overlap checking**, including with GADTs
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

- [\#2284](https://gitlab.haskell.org//ghc/ghc/issues/2284): the state-hack "optimisation" causes much re-computation
- [\#4005](https://gitlab.haskell.org//ghc/ghc/issues/4005): generational GC pathalogical case
- [\#1349](https://gitlab.haskell.org//ghc/ghc/issues/1349): strict function argument types
