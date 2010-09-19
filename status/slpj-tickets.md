# This page collects tickets that Simon PJ is interested in, so that he remembers them

## Performance

### Arity

- [\#3924](https://gitlab.haskell.org//ghc/ghc/issues/3924): weak arity leads to weak strictness
- [\#3698](https://gitlab.haskell.org//ghc/ghc/issues/3698): arity analysis again
- [\#3697](https://gitlab.haskell.org//ghc/ghc/issues/3697): class method selectors
- [\#3034](https://gitlab.haskell.org//ghc/ghc/issues/3034): divInt and arity
- [\#3627](https://gitlab.haskell.org//ghc/ghc/issues/3627): profiling and eta expansion
- [\#2915](https://gitlab.haskell.org//ghc/ghc/issues/2915): arity too small
- [\#2762](https://gitlab.haskell.org//ghc/ghc/issues/2762): arity analysis would fix a space leak
- [\#2902](https://gitlab.haskell.org//ghc/ghc/issues/2902): an excellent example of the need for arity analysis
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823): another arity expansion bug (related to dictionaries)
- [\#2440](https://gitlab.haskell.org//ghc/ghc/issues/2440): bad code with type families; I believe this is also arity-related
- [\#2762](https://gitlab.haskell.org//ghc/ghc/issues/2762): Arity analysis
- [\#2831](https://gitlab.haskell.org//ghc/ghc/issues/2831): error expressions and arities

### Inlining

- [\#4227](https://gitlab.haskell.org//ghc/ghc/issues/4227): SPECIALISE pragmas for functions in other modules
- [\#3781](https://gitlab.haskell.org//ghc/ghc/issues/3781): inlining for local functions: discount for scrutinising free vars
- [\#3765](https://gitlab.haskell.org//ghc/ghc/issues/3765): CONLIKE things and case expressions (need two unfoldings)
- [\#3755](https://gitlab.haskell.org//ghc/ghc/issues/3755): idea for "wrapper peeling" for join points.
- [\#3586](https://gitlab.haskell.org//ghc/ghc/issues/3586): inlining default methods
- [\#3526](https://gitlab.haskell.org//ghc/ghc/issues/3526): Inliner behaviour confusing (waiting on Bryan)
- [\#3181](https://gitlab.haskell.org//ghc/ghc/issues/3181): check unboxing regression fixed in Big INLINE Patch
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073) and [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances): better desugaring for instances
- [\#2422](https://gitlab.haskell.org//ghc/ghc/issues/2422): interaction of inlining and specialisation
- [\#2396](https://gitlab.haskell.org//ghc/ghc/issues/2396): default class method not inlined
- [\#2354](https://gitlab.haskell.org//ghc/ghc/issues/2354): NOINLINE pragma ignored
- [\#2353](https://gitlab.haskell.org//ghc/ghc/issues/2353): GHC inliner doesn't inline (with follow-up from Ross)
- [\#2078](https://gitlab.haskell.org//ghc/ghc/issues/2078): INLINing improvement; ask Christian.Maeder@… to see if it improves CASL
- [\#2840](https://gitlab.haskell.org//ghc/ghc/issues/2840): top level unlifted string literals
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123): feature request: recursive inlining and peeling

## New code generator

- [New code generator tickets](commentary/compiler/new-code-gen)
- [\#4065](https://gitlab.haskell.org//ghc/ghc/issues/4065): forward propagation
- [\#3969](https://gitlab.haskell.org//ghc/ghc/issues/3969): better loops
- [\#3132](https://gitlab.haskell.org//ghc/ghc/issues/3132): fix cgCase of PrimAlts
- [\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458), [\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216): undesirable allocation of something that is nearly a let-no-escape
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462): allocate large objects with `allocateLocal()`
- [\#2253](https://gitlab.haskell.org//ghc/ghc/issues/2253): NCG could do better. Look at this when John D’s ncg is working
- [\#783](https://gitlab.haskell.org//ghc/ghc/issues/783): SRTs getting big        
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): cheap check at start of case alternatives
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731): avoiding unnecessary evaluation when unpacking constructors
- [\#3940](https://gitlab.haskell.org//ghc/ghc/issues/3940): propagate fix
- [\#3969](https://gitlab.haskell.org//ghc/ghc/issues/3969): bad code for tight loop
- [\#4090](https://gitlab.haskell.org//ghc/ghc/issues/4090): failed `getRegister` in asm codegen

### Other performance

- [\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439): Strict dictionaries
- [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138): returning a known constructor (Lennart's cmonad package)
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988): better float-in
- [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940): do CSE after `CorePrep`
- [\#2670](https://gitlab.haskell.org//ghc/ghc/issues/2670): record selectors behaving badly wrt optimisation
- [\#1434](https://gitlab.haskell.org//ghc/ghc/issues/1434): Slow conversion Double to Int        
- [\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132): Optimise nested comparisons: if you know x\>0 then you know x\>=1 etc.  Maybe a special pass that knows about arithmetic?        
- [\#149](https://gitlab.haskell.org//ghc/ghc/issues/149): float-out/CSE        
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289), [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387), [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600): nested CPR analysis        
- [\#2092](https://gitlab.haskell.org//ghc/ghc/issues/2092): Possible quadratic-sized Eq instances. Does it really go quadratic, or does the join-point inlining machinery prevent it?  Still to check: delicacy wrt case-of-case
- [\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255), [\#3767](https://gitlab.haskell.org//ghc/ghc/issues/3767), [\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642), [\#3831](https://gitlab.haskell.org//ghc/ghc/issues/3831): Improve **`SpecConstr`** for free variables, and for join points.
- [\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374): SAT and `MutableByteArray`        Max?
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065): better code in quot/rem

### Compiler performance

- [\#1969](https://gitlab.haskell.org//ghc/ghc/issues/1969): quadratic behaviour in the specialiser
- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings
- Use wildcards for dead variables in interface files.

## Tiresome arithmetic things

- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101): constant folding for `(**)`
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676): `realToFrac` conversions
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744): comparisons against `minBound` and `maxBound` are not optimised away
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065): quot is sub-optimal
- [\#2269](https://gitlab.haskell.org//ghc/ghc/issues/2269): Word type to Double or Float conversions 
- [\#2271](https://gitlab.haskell.org//ghc/ghc/issues/2271): floor, ceiling, round :: Double -\> Int are awesomely slow
- [\#1434](https://gitlab.haskell.org//ghc/ghc/issues/1434): slow conversion Double to Int
- [\#2270](https://gitlab.haskell.org//ghc/ghc/issues/2270): gcd specialisation

## GHCi

- [\#4017](https://gitlab.haskell.org//ghc/ghc/issues/4017): unhelpful GHCi message

---

## Outright bugs

- [\#1148](https://gitlab.haskell.org//ghc/ghc/issues/1148), [\#2267](https://gitlab.haskell.org//ghc/ghc/issues/2267), [\#1074](https://gitlab.haskell.org//ghc/ghc/issues/1074), [\#2436](https://gitlab.haskell.org//ghc/ghc/issues/2436), [\#1792](https://gitlab.haskell.org//ghc/ghc/issues/1792), (related [\#3082](https://gitlab.haskell.org//ghc/ghc/issues/3082)): “Unused import” warnings should be generated from `RdrNames`
- [\#2182](https://gitlab.haskell.org//ghc/ghc/issues/2182): GHCi session retains instance after removing a module from scope        
- [\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241): Lifting the Coverage Condition for functional dependencies isn’t the Right Thing        Manuel
- [\#1954](https://gitlab.haskell.org//ghc/ghc/issues/1954): Incorrect “defined but not used” msg        

---

## New constraint simplifier

- **[\#4232](https://gitlab.haskell.org//ghc/ghc/issues/4232): main meta-ticket for the new type checker**
- [\#1823](https://gitlab.haskell.org//ghc/ghc/issues/1823): refinement
- [\#3714](https://gitlab.haskell.org//ghc/ghc/issues/3714): error message if associated family has excess parameters
- [\#3554](https://gitlab.haskell.org//ghc/ghc/issues/3554): assertion failure
- [\#3330](https://gitlab.haskell.org//ghc/ghc/issues/3330): type checker loops
- [\#3500](https://gitlab.haskell.org//ghc/ghc/issues/3500): recursive dictionaries
- [\#3787](https://gitlab.haskell.org//ghc/ghc/issues/3787): bad program produced
- [\#4226](https://gitlab.haskell.org//ghc/ghc/issues/4226): strange implicit-parameter behaviour
- [\#4160](https://gitlab.haskell.org//ghc/ghc/issues/4160): Type families crash in HEAD
- [\#4200](https://gitlab.haskell.org//ghc/ghc/issues/4200): don't require `UndecidableInstances` for equality constraints
- [\#4178](https://gitlab.haskell.org//ghc/ghc/issues/4178): unnecessary skolem escape check
- [\#4179](https://gitlab.haskell.org//ghc/ghc/issues/4179): loop in type inference
- [\#4174](https://gitlab.haskell.org//ghc/ghc/issues/4174): bad error message
- [\#4093](https://gitlab.haskell.org//ghc/ghc/issues/4093): constraint simplifier loops
- [\#2296](https://gitlab.haskell.org//ghc/ghc/issues/2296): position info for fundep message
- [\#2683](https://gitlab.haskell.org//ghc/ghc/issues/2683): muttering about boxy type ASSERT
- [\#4044](https://gitlab.haskell.org//ghc/ghc/issues/4044), [\#4045](https://gitlab.haskell.org//ghc/ghc/issues/4045): these were crashes something to do with type functions when building GHC
- [\#3064](https://gitlab.haskell.org//ghc/ghc/issues/3064): performance of constraint simplifier
- [TypeFunctionsStatus](type-functions-status)
- [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927), [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139): overlap warnings with GADTs
- [\#3851](https://gitlab.haskell.org//ghc/ghc/issues/3851): type family expansion
- [\#3826](https://gitlab.haskell.org//ghc/ghc/issues/3826): equality reasoning failure
- [\#3692](https://gitlab.haskell.org//ghc/ghc/issues/3692): bogus error message with constraint after arrow
- [\#3742](https://gitlab.haskell.org//ghc/ghc/issues/3742): implication constraints and inference
- [\#3731](https://gitlab.haskell.org//ghc/ghc/issues/3731): recursive dictionaries
- [\#3738](https://gitlab.haskell.org//ghc/ghc/issues/3738): don't float `MethodInsts` out of INLINE right hand sides.
- [\#2256](https://gitlab.haskell.org//ghc/ghc/issues/2256): Incomplete inference due to lack of quantification over implication constraints.  Also, see “BUG WARNING” in `TcSimplify` line 717 or thereabouts.  `fdPredsOfInsts` is returning preds that mention quantified variables, which is quite wrong        Manuel
- [\#2239](https://gitlab.haskell.org//ghc/ghc/issues/2239): Lack of improvement with type functions        Manuel
- [\#700](https://gitlab.haskell.org//ghc/ghc/issues/700): universals in pattern matching
- [\#3696](https://gitlab.haskell.org//ghc/ghc/issues/3696): better error message fr missing signature
- [\#4175](https://gitlab.haskell.org//ghc/ghc/issues/4175): better GHCi info for type-function instances
- [\#4254](https://gitlab.haskell.org//ghc/ghc/issues/4254): fundeps
- [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): overlapping instances

- [\#816](https://gitlab.haskell.org//ghc/ghc/issues/816): needs lazier use of instance declarations
- [\#4296](https://gitlab.haskell.org//ghc/ghc/issues/4296): SkolemOccurs
- [\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295): Higher rank and impredicative 

## Types and type inference

- [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490): superclasses and ambiguity
- [\#3638](https://gitlab.haskell.org//ghc/ghc/issues/3638): rules and rigidity for GADTs
- [\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632): better update for records with existentials
- [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496): Newtype deriving and type families type soundness problem
- [\#1897](https://gitlab.haskell.org//ghc/ghc/issues/1897): **Ambiguity: don't infer a type that can't be checked if the type is given as a signature** (this one is important).  See SPJ's mailbox: Haskell type system/Ambiguity.
- [\#2859](https://gitlab.haskell.org//ghc/ghc/issues/2859): optimise coercion terms
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641): revise what `-XExtendedDefaultRules` does
- [\#1634](https://gitlab.haskell.org//ghc/ghc/issues/1634): deep skolemisation; also this one [\#3592](https://gitlab.haskell.org//ghc/ghc/issues/3592)
- [\#3018](https://gitlab.haskell.org//ghc/ghc/issues/3018): be lazier about solving class instances
- [\#3108](https://gitlab.haskell.org//ghc/ghc/issues/3108): interaction of fundeps and type class solving
- [\#2357](https://gitlab.haskell.org//ghc/ghc/issues/2357): **Implement the Haskell Prime proposal for polymorphic pattern bindings**

### Impredicativity

- [\#2846](https://gitlab.haskell.org//ghc/ghc/issues/2846): polymorphism leaking into constraints
- [\#2193](https://gitlab.haskell.org//ghc/ghc/issues/2193): Bad error message with impredicative types
- [\#1330](https://gitlab.haskell.org//ghc/ghc/issues/1330): another bad error message (Church2)

### Better error messages

- [\#3440](https://gitlab.haskell.org//ghc/ghc/issues/3440): type families and GADT error message
- [\#3169](https://gitlab.haskell.org//ghc/ghc/issues/3169): better occurs-check error message
- [\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648): Report out of date interface files robustly        
- [\#2599](https://gitlab.haskell.org//ghc/ghc/issues/2599): Better error message for non-rigid types        
- [\#2588](https://gitlab.haskell.org//ghc/ghc/issues/2588): Better error message about ‘forall’        
- [\#2360](https://gitlab.haskell.org//ghc/ghc/issues/2360): Better location info in occurs-check message.        
- [\#1856](https://gitlab.haskell.org//ghc/ghc/issues/1856): Better error message for mutually recursive modules        
- [\#1928](https://gitlab.haskell.org//ghc/ghc/issues/1928): Confusing type error message (Claus makes suggestions)        
- [\#2534](https://gitlab.haskell.org//ghc/ghc/issues/2534): Another confusing type error message        
- [\#2442](https://gitlab.haskell.org//ghc/ghc/issues/2442): Better error message heuristics for “not in scope”        Max B’s patch
- [\#2900](https://gitlab.haskell.org//ghc/ghc/issues/2900): Improve decomposition of function types
- [\#3023](https://gitlab.haskell.org//ghc/ghc/issues/3023): Apply fundeps before printing error

---

## Template Haskell

- [\#3492](https://gitlab.haskell.org//ghc/ghc/issues/3492): refactor `TyThing` to `HsSyn` code
- [\#3497](https://gitlab.haskell.org//ghc/ghc/issues/3497): Template Haskell support for GADTs
- [\#3507](https://gitlab.haskell.org//ghc/ghc/issues/3507): use "`type T`" instead of `''T`
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Allow TH to generate import declarations        
- [\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340): Better TH error recovery.  Easy to do; change to signature of qRecover.
- [\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222): design of reification for TH

## Features

- [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595): pattern-match overlap checking
- [\#3843](https://gitlab.haskell.org//ghc/ghc/issues/3843): Add plugins
- [\#3701](https://gitlab.haskell.org//ghc/ghc/issues/3701): Implicitly declared existentials: a class as a type
- [\#3480](https://gitlab.haskell.org//ghc/ghc/issues/3480): **Fingerprints in `TypeRep`**
- [\#3217](https://gitlab.haskell.org//ghc/ghc/issues/3217): better flag handling for GHCi
- [\#788](https://gitlab.haskell.org//ghc/ghc/issues/788): Class aliases        
- [\#2806](https://gitlab.haskell.org//ghc/ghc/issues/2806): Require bang patterns on unlifted let-bindings        
- [\#2600](https://gitlab.haskell.org//ghc/ghc/issues/2600), [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110): **Bind type variables and constraints in RULES**
- [\#1930](https://gitlab.haskell.org//ghc/ghc/issues/1930): Infix type operators:  a+b.        
- [\#960](https://gitlab.haskell.org//ghc/ghc/issues/960): Generate local info for ‘undefined’.  Implicit location parameters in general        
- [\#2135](https://gitlab.haskell.org//ghc/ghc/issues/2135): Warn when exporting a function whose type mentions a type constructor defined locally but not itself exported        
- [\#2526](https://gitlab.haskell.org//ghc/ghc/issues/2526): Add -fwarn-missing-export-signatures        
- [\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119): Explicitly importing deprecated symbols should generate deprecation warnings        
- [\#2116](https://gitlab.haskell.org//ghc/ghc/issues/2116): GHCi should load as much of the module as it can        
- [\#2207](https://gitlab.haskell.org//ghc/ghc/issues/2207): Load interfaces for GHC.\* even without -O        
- [\#1231](https://gitlab.haskell.org//ghc/ghc/issues/1231): Better deprecations        

---

## Not sure what to do about these

- [\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607): space leak: inlining defeats selector thunk optimisation
- [\#2284](https://gitlab.haskell.org//ghc/ghc/issues/2284): the state-hack "optimisation" causes much re-computation
- [\#3872](https://gitlab.haskell.org//ghc/ghc/issues/3872): divergence in simplifier
- [\#4005](https://gitlab.haskell.org//ghc/ghc/issues/4005): generational GC pathalogical case
- [\#1349](https://gitlab.haskell.org//ghc/ghc/issues/1349): strict function argument types
