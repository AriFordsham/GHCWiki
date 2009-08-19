# This page collects tickets that Simon PJ is interested in, so that he remembers them

## Performance

### Arity

- [\#2915](https://gitlab.haskell.org//ghc/ghc/issues/2915): arity too small
- [\#2762](https://gitlab.haskell.org//ghc/ghc/issues/2762): arity analysis would fix a space leak
- [\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216): array indexing, inlining/arity bug        
- [\#2902](https://gitlab.haskell.org//ghc/ghc/issues/2902): an excellent example of the need for arity analysis
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823): another arity expansion bug (related to dictionaries)
- [\#2440](https://gitlab.haskell.org//ghc/ghc/issues/2440): bad code with type families; I believe this is also arity-related
- [\#2762](https://gitlab.haskell.org//ghc/ghc/issues/2762): Arity analysis
- [\#2368](https://gitlab.haskell.org//ghc/ghc/issues/2368): ASSERT fails in `CorePrep`.  Seems to be arity-related.
- [\#2831](https://gitlab.haskell.org//ghc/ghc/issues/2831): error expressions and arities

### Inlining

- [\#3198](https://gitlab.haskell.org//ghc/ghc/issues/3198): check this is still ok after the Big INLINE Patch
- [\#3181](https://gitlab.haskell.org//ghc/ghc/issues/3181): check unboxing regression fixed in Big INLINE Patch
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073) and [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances): better desugaring for instances
- [\#2422](https://gitlab.haskell.org//ghc/ghc/issues/2422): interaction of inlining and specialisation
- [\#2396](https://gitlab.haskell.org//ghc/ghc/issues/2396): default class method not inlined
- [\#2354](https://gitlab.haskell.org//ghc/ghc/issues/2354): NOINLINE pragma ignored
- [\#2353](https://gitlab.haskell.org//ghc/ghc/issues/2353): GHC inliner doesn't inline
- [\#2078](https://gitlab.haskell.org//ghc/ghc/issues/2078): INLINing improvement; ask Christian.Maeder@… to see if it improves CASL
- [\#2840](https://gitlab.haskell.org//ghc/ghc/issues/2840): top level unlifted string literals
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123): feature request: recursive inlining and peeling

## New code generator

- [\#2253](https://gitlab.haskell.org//ghc/ghc/issues/2253): NCG could do better. Look at this when John D’s ncg is working
- [\#783](https://gitlab.haskell.org//ghc/ghc/issues/783): SRTs getting big        
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): cheap check at start of case alternatives        
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731): avoiding unnecessary evaluation when unpacking constructors

### Other performance

- [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138): returning a known constructor (Lennart's cmonad package)
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988): better float-in
- [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940): do CSE after `CorePrep`
- [\#2670](https://gitlab.haskell.org//ghc/ghc/issues/2670): record selectors behaving badly wrt optimisation
- [\#1434](https://gitlab.haskell.org//ghc/ghc/issues/1434): Slow conversion Double to Int        
- [\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439): Strict dictionaries        
- [\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132): Optimise nested comparisons: if you know x\>0 then you know x\>=1 etc.  Maybe a special pass that knows about arithmetic?        
- [\#149](https://gitlab.haskell.org//ghc/ghc/issues/149): float-out/CSE        
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289), [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387), [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600): nested CPR analysis        
- [\#2092](https://gitlab.haskell.org//ghc/ghc/issues/2092): Possible quadratic-sized Eq instances. Does it really go quadratic, or does the join-point inlining machinery prevent it?  Still to check: delicacy wrt case-of-case
- [\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255), [\#2643](https://gitlab.haskell.org//ghc/ghc/issues/2643): Improve **`SpecConstr`** for free variables, and for join points.
- [\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374): SAT and `MutableByteArray`        Max?
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065): better code in quot/rem

### Compiler performance

- [\#1969](https://gitlab.haskell.org//ghc/ghc/issues/1969): quadratic behaviour in the specialiser
- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings
- Use wildcards for dead variables in interface files.

---

## Outright bugs

- [\#1148](https://gitlab.haskell.org//ghc/ghc/issues/1148), [\#2267](https://gitlab.haskell.org//ghc/ghc/issues/2267), [\#1074](https://gitlab.haskell.org//ghc/ghc/issues/1074), [\#2436](https://gitlab.haskell.org//ghc/ghc/issues/2436), [\#1792](https://gitlab.haskell.org//ghc/ghc/issues/1792), (related [\#3082](https://gitlab.haskell.org//ghc/ghc/issues/3082)): “Unused import” warnings should be generated from `RdrNames`
- [\#2182](https://gitlab.haskell.org//ghc/ghc/issues/2182): GHCi session retains instance after removing a module from scope        
- [\#2152](https://gitlab.haskell.org//ghc/ghc/issues/2152): Bogus inlining of foregn import.  Arises from seeing through a NOINLINE in `exprIsConApp_maybe`
- [\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241): Lifting the Coverage Condition for functional dependencies isn’t the Right Thing        Manuel
- [\#2256](https://gitlab.haskell.org//ghc/ghc/issues/2256): Incomplete inference due to lack of quantification over implication constraints.  Also, see “BUG WARNING” in `TcSimplify` line 717 or thereabouts.  `fdPredsOfInsts` is returning preds that mention quantified variables, which is quite wrong        Manuel
- [\#2239](https://gitlab.haskell.org//ghc/ghc/issues/2239): Lack of improvement with type functions        Manuel
- [\#1954](https://gitlab.haskell.org//ghc/ghc/issues/1954): Incorrect “defined but not used” msg        

---

## Types and type inference

- [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496): Newtype deriving and type families type soundness problem
- [\#1897](https://gitlab.haskell.org//ghc/ghc/issues/1897): **Ambiguity: don't infer a type that can't be checked if the type is given as a signature** (this one is important).  See SPJ's mailbox: Haskell type system/Ambiguity.
- [\#2859](https://gitlab.haskell.org//ghc/ghc/issues/2859): optimise coercion terms
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641): revise what `-XExtendedDefaultRules` does
- [\#1634](https://gitlab.haskell.org//ghc/ghc/issues/1634): deep skolemisation
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
- [\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340): Better TH error recovery.  Easy to do; change to signature of qRecover.
- [\#2900](https://gitlab.haskell.org//ghc/ghc/issues/2900): Improve decomposition of function types
- [\#3023](https://gitlab.haskell.org//ghc/ghc/issues/3023): Apply fundeps before printing error

---

## Features

- [\#3217](https://gitlab.haskell.org//ghc/ghc/issues/3217): better flag handling for GHCi
- [\#788](https://gitlab.haskell.org//ghc/ghc/issues/788):        Class aliases        
- [\#2806](https://gitlab.haskell.org//ghc/ghc/issues/2806): Require bang patterns on unlifted let-bindings        
- [\#2600](https://gitlab.haskell.org//ghc/ghc/issues/2600): Bind type variables in RULES        
- [\#1930](https://gitlab.haskell.org//ghc/ghc/issues/1930): Infix type operators:  a+b.        
- [\#960](https://gitlab.haskell.org//ghc/ghc/issues/960): Generate local info for ‘undefined’.  Implicit location parameters in general        
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Allow TH to generate import declarations        
- [\#2135](https://gitlab.haskell.org//ghc/ghc/issues/2135): Warn when exporting a function whose type mentions a type constructor defined locally but not itself exported        
- [\#2526](https://gitlab.haskell.org//ghc/ghc/issues/2526): Add -fwarn-missing-export-signatures        
- [\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119): Explicitly importing deprecated symbols should generate deprecation warnings        
- [\#2116](https://gitlab.haskell.org//ghc/ghc/issues/2116): GHCi should load as much of the module as it can        
- [\#2207](https://gitlab.haskell.org//ghc/ghc/issues/2207): Load interfaces for GHC.\* even without -O        
- [\#1231](https://gitlab.haskell.org//ghc/ghc/issues/1231): Better deprecations        

---

## Not sure what to do about these

- [\#2284](https://gitlab.haskell.org//ghc/ghc/issues/2284): the state-hack "optimisation" causes much re-computation
