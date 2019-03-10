# This page collects tickets that Simon PJ is interested in, so that he remembers them

## Status pages

- `hs-boot` files: [Commentary/Compiler/TyingTheKnot](commentary/compiler/tying-the-knot)
- Type-indexed type representations: [plan and status](typeable/ben-gamari)
- [TypeInType](dependent-haskell/phase1): [DependentHaskell](dependent-haskell), esp [\#12919](https://gitlab.haskell.org//ghc/ghc/issues/12919) (outright bug)
- `TypeApplications`: [visible type application](type-application)
- [Custom type errors](proposal/custom-type-errors)
- [UnliftedDataTypes](unlifted-data-types)
- [UnpackedSumTypes](unpacked-sum-types), [\#9214](https://gitlab.haskell.org//ghc/ghc/issues/9214)
- [TypeNats](type-nats): type-level literals
- [Exceptions](exceptions): exceptions
- [LevityPolymorphism](levity-polymorphism)
- [\#11715](https://gitlab.haskell.org//ghc/ghc/issues/11715), [\#11621](https://gitlab.haskell.org//ghc/ghc/issues/11621): `Constraint` vs `*`
- [Generic deriving](commentary/compiler/generic-deriving)
- [Join points](sequent-core)
- [The new pattern-match overlap/exhaustiveness checker](pattern-match-check)
- [Pattern-synonym status page](pattern-synonyms)
- [Overloaded record fields](records/overloaded-record-fields)

## INLINE problems / ticks exhausted

- [\#13027](https://gitlab.haskell.org//ghc/ghc/issues/13027): let/app invariant
- [\#12603](https://gitlab.haskell.org//ghc/ghc/issues/12603)
- [\#12747](https://gitlab.haskell.org//ghc/ghc/issues/12747)
- [\#12790](https://gitlab.haskell.org//ghc/ghc/issues/12790): too much inlining 

- [\#8613](https://gitlab.haskell.org//ghc/ghc/issues/8613), [\#9070](https://gitlab.haskell.org//ghc/ghc/issues/9070), [\#8319](https://gitlab.haskell.org//ghc/ghc/issues/8319), [\#7521](https://gitlab.haskell.org//ghc/ghc/issues/7521), [\#10459](https://gitlab.haskell.org//ghc/ghc/issues/10459), [\#10584](https://gitlab.haskell.org//ghc/ghc/issues/10584), [\#10565](https://gitlab.haskell.org//ghc/ghc/issues/10565), [\#12274](https://gitlab.haskell.org//ghc/ghc/issues/12274): simplifier ticks exhausted (there are others). 

## Urgent or in-flight

- [\#12088](https://gitlab.haskell.org//ghc/ghc/issues/12088): phase ordering for type-in-type
- [\#11371](https://gitlab.haskell.org//ghc/ghc/issues/11371): bogus in-scope set for substitution
- [\#9479](https://gitlab.haskell.org//ghc/ghc/issues/9479), [\#10954](https://gitlab.haskell.org//ghc/ghc/issues/10954), [\#9091](https://gitlab.haskell.org//ghc/ghc/issues/9091): more info in "hole" error messages
- [\#10844](https://gitlab.haskell.org//ghc/ghc/issues/10844), [\#10845](https://gitlab.haskell.org//ghc/ghc/issues/10845), [\#10846](https://gitlab.haskell.org//ghc/ghc/issues/10846): implicit call stacks
  \*
- [\#7262](https://gitlab.haskell.org//ghc/ghc/issues/7262), [\#10333](https://gitlab.haskell.org//ghc/ghc/issues/10333), [\#7672](https://gitlab.haskell.org//ghc/ghc/issues/7672), [\#10083](https://gitlab.haskell.org//ghc/ghc/issues/10083): hs-boot niggles
- [\#10114](https://gitlab.haskell.org//ghc/ghc/issues/10114): `AnyK` and kind generalisation in type synonyms
- [\#10068](https://gitlab.haskell.org//ghc/ghc/issues/10068): runtime reflection API for packages, modules, names; `SrcLoc` etc.
- [\#9960](https://gitlab.haskell.org//ghc/ghc/issues/9960), [\#9805](https://gitlab.haskell.org//ghc/ghc/issues/9805): `TrieMap` stuff
- [\#10016](https://gitlab.haskell.org//ghc/ghc/issues/10016): UNPACK support for existentials
- [\#10803](https://gitlab.haskell.org//ghc/ghc/issues/10803): type signature sections (Lennart)
- [\#8634](https://gitlab.haskell.org//ghc/ghc/issues/8634), [\#9267](https://gitlab.haskell.org//ghc/ghc/issues/9267), [\#9227](https://gitlab.haskell.org//ghc/ghc/issues/9227): relaxing functional dependency rules `-XDysFunctionalDependencies`
- [\#9123](https://gitlab.haskell.org//ghc/ghc/issues/9123): higher order roles
- [\#9352](https://gitlab.haskell.org//ghc/ghc/issues/9352): allow `State#` in FFI types.
- [\#9725](https://gitlab.haskell.org//ghc/ghc/issues/9725): kind equalities (Richard)
- [\#9717](https://gitlab.haskell.org//ghc/ghc/issues/9717), [\#9729](https://gitlab.haskell.org//ghc/ghc/issues/9729), [\#10420](https://gitlab.haskell.org//ghc/ghc/issues/10420): orphan modules

## Type families, roles, `Coercible`, `Typeable`, kind polymorphism


 
Refactoring/optimisation

- [\#11598](https://gitlab.haskell.org//ghc/ghc/issues/11598), [\#8095](https://gitlab.haskell.org//ghc/ghc/issues/8095): drop coercions 
- [\#11739](https://gitlab.haskell.org//ghc/ghc/issues/11739): coercion axioms applied to types, not coercions
- [\#11735](https://gitlab.haskell.org//ghc/ghc/issues/11735), [\#11598](https://gitlab.haskell.org//ghc/ghc/issues/11598): `coercionKind`
- [\#11196](https://gitlab.haskell.org//ghc/ghc/issues/11196): resurrect `FunTy`
- Make `CoVar` a proper `Var`
- [\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965): make existentials have a really cheap implementation


Other tickets

- [\#12369](https://gitlab.haskell.org//ghc/ghc/issues/12369): data families with non-\* kind (Kmett)
- [\#11622](https://gitlab.haskell.org//ghc/ghc/issues/11622): parens around kind signatures in types
- [\#1311](https://gitlab.haskell.org//ghc/ghc/issues/1311): newtypes over unboxed types
- [\#11203](https://gitlab.haskell.org//ghc/ghc/issues/11203), [\#11453](https://gitlab.haskell.org//ghc/ghc/issues/11453): Kind inference with `SigTvs` is wrong
- [\#11080](https://gitlab.haskell.org//ghc/ghc/issues/11080): open data kinds
- [\#7102](https://gitlab.haskell.org//ghc/ghc/issues/7102): type instance overlap in GHCi is unsound
- [\#9858](https://gitlab.haskell.org//ghc/ghc/issues/9858), [\#10343](https://gitlab.haskell.org//ghc/ghc/issues/10343): `Typeable` and kind polymorphism
- [\#10347](https://gitlab.haskell.org//ghc/ghc/issues/10347): wrong "unused data constructor" warning for a newtype (using `Coercible`)
- [\#10184](https://gitlab.haskell.org//ghc/ghc/issues/10184),[\#10185](https://gitlab.haskell.org//ghc/ghc/issues/10185): incompleteness in `Coercible` solver
- [\#8165](https://gitlab.haskell.org//ghc/ghc/issues/8165): GND for classes with associated types
- [\#10056](https://gitlab.haskell.org//ghc/ghc/issues/10056): more systematic treatment of `(~)` in parser and renamer 
- [\#9918](https://gitlab.haskell.org//ghc/ghc/issues/9918): closed type families, apartness, and overlapping instances (inconsistency)
- [\#9118](https://gitlab.haskell.org//ghc/ghc/issues/9118): Eta reduction for `Coercible` (not expressible in Core, yet)
- [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018), [\#10227](https://gitlab.haskell.org//ghc/ghc/issues/10227), [\#10832](https://gitlab.haskell.org//ghc/ghc/issues/10832), [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): injective type families
- [\#8161](https://gitlab.haskell.org//ghc/ghc/issues/8161): associated type more specific than class decl
- [\#8177](https://gitlab.haskell.org//ghc/ghc/issues/8177): role signature for type families
- [\#9547](https://gitlab.haskell.org//ghc/ghc/issues/9547): kinding for empty constraint tuples

## Data types and pattern matching

- [\#12159](https://gitlab.haskell.org//ghc/ghc/issues/12159) naughty record selectors
- [\#11253](https://gitlab.haskell.org//ghc/ghc/issues/11253): duplicate warnings with new pattern checker
- [\#10183](https://gitlab.haskell.org//ghc/ghc/issues/10183): warning for redundant constraints, and pattern-match overlap warnings
- [\#9113](https://gitlab.haskell.org//ghc/ghc/issues/9113): pattern match overlap/exhaustiveness checked in Typed TH
- [\#10393](https://gitlab.haskell.org//ghc/ghc/issues/10393), [\#10116](https://gitlab.haskell.org//ghc/ghc/issues/10116), [\#9951](https://gitlab.haskell.org//ghc/ghc/issues/9951), [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595), [\#5728](https://gitlab.haskell.org//ghc/ghc/issues/5728), [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927), [\#5724](https://gitlab.haskell.org//ghc/ghc/issues/5724), [\#5762](https://gitlab.haskell.org//ghc/ghc/issues/5762), [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139), [\#6124](https://gitlab.haskell.org//ghc/ghc/issues/6124), [\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669), [\#322](https://gitlab.haskell.org//ghc/ghc/issues/322), [\#8016](https://gitlab.haskell.org//ghc/ghc/issues/8016), [\#8494](https://gitlab.haskell.org//ghc/ghc/issues/8494), [\#8853](https://gitlab.haskell.org//ghc/ghc/issues/8853), [\#8970](https://gitlab.haskell.org//ghc/ghc/issues/8970), [\#9113](https://gitlab.haskell.org//ghc/ghc/issues/9113), [\#2204](https://gitlab.haskell.org//ghc/ghc/issues/2204): **pattern-match overlap checking**, including with GADTs

## Impredicativity and higher rank

- [\#11514](https://gitlab.haskell.org//ghc/ghc/issues/11514): Impredicativity sneaking in (should be easy)
- [\#10619](https://gitlab.haskell.org//ghc/ghc/issues/10619): higher rank and order of equations
- [\#4281](https://gitlab.haskell.org//ghc/ghc/issues/4281), [\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295), [\#4347](https://gitlab.haskell.org//ghc/ghc/issues/4347), [\#7264](https://gitlab.haskell.org//ghc/ghc/issues/7264), [\#8808](https://gitlab.haskell.org//ghc/ghc/issues/8808), [\#9420](https://gitlab.haskell.org//ghc/ghc/issues/9420), [\#9730](https://gitlab.haskell.org//ghc/ghc/issues/9730), [\#10709](https://gitlab.haskell.org//ghc/ghc/issues/10709): impredicativity

## Compile-time performance

[Compile time perf page](performance/compiler)

- [\#11375](https://gitlab.haskell.org//ghc/ghc/issues/11375): type synonyms slower than type families
- [\#8095](https://gitlab.haskell.org//ghc/ghc/issues/8095): very slow constraint solving; perhaps discard large constraints using `UnivCo`
- [\#5642](https://gitlab.haskell.org//ghc/ghc/issues/5642): slow constraint solving
- [\#9979](https://gitlab.haskell.org//ghc/ghc/issues/9979), [\#8814](https://gitlab.haskell.org//ghc/ghc/issues/8814), [\#8835](https://gitlab.haskell.org//ghc/ghc/issues/8835): attoparsec regressed 7.8.4 to HEAD
- [\#8852](https://gitlab.haskell.org//ghc/ghc/issues/8852), [\#8980](https://gitlab.haskell.org//ghc/ghc/issues/8980), [\#8941](https://gitlab.haskell.org//ghc/ghc/issues/8941) (possibly), [\#9803](https://gitlab.haskell.org//ghc/ghc/issues/9803), [\#8960](https://gitlab.haskell.org//ghc/ghc/issues/8960), [\#7898](https://gitlab.haskell.org//ghc/ghc/issues/7898), [\#7068](https://gitlab.haskell.org//ghc/ghc/issues/7068), [\#7944](https://gitlab.haskell.org//ghc/ghc/issues/7944), [\#5550](https://gitlab.haskell.org//ghc/ghc/issues/5550), [\#8836](https://gitlab.haskell.org//ghc/ghc/issues/8836): `SpecConstr` blowup
- [\#10289](https://gitlab.haskell.org//ghc/ghc/issues/10289): 2.5k static `HashSet` takes too much memory to compile
- [\#10228](https://gitlab.haskell.org//ghc/ghc/issues/10228): compile-time regression from 7.8.4 to 7.10.1
- [\#7428](https://gitlab.haskell.org//ghc/ghc/issues/7428): Non-linear compile time: `addFingerprint`??
- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings
- Use wildcards for dead variables in interface files.


Code-size blowup when using `deriving`; see tickets with keyword `deriving-perf`

- [\#7450](https://gitlab.haskell.org//ghc/ghc/issues/7450), [\#7258](https://gitlab.haskell.org//ghc/ghc/issues/7258): deriving `Read` generates gigantic code. Better now, but still not linear.
- [\#9583](https://gitlab.haskell.org//ghc/ghc/issues/9583), [\#9630](https://gitlab.haskell.org//ghc/ghc/issues/9630): code blowup in Generics/Binary
- [\#9669](https://gitlab.haskell.org//ghc/ghc/issues/9669), [\#9557](https://gitlab.haskell.org//ghc/ghc/issues/9557), [\#8731](https://gitlab.haskell.org//ghc/ghc/issues/8731): slow compilation with lots of `deriving` clauses
- [\#10858](https://gitlab.haskell.org//ghc/ghc/issues/10858): `Ord` instances
- [\#1544](https://gitlab.haskell.org//ghc/ghc/issues/1544): `Read1` instances

## Run-time performance

[Run time perf page](performance/runtime)

- The notorious state hack, and `replicateM` in particular

  - [\#1168](https://gitlab.haskell.org//ghc/ghc/issues/1168) has a list of related tickets
  - [\#11677](https://gitlab.haskell.org//ghc/ghc/issues/11677)
  - [\#11365](https://gitlab.haskell.org//ghc/ghc/issues/11365)
  - [\#6166](https://gitlab.haskell.org//ghc/ghc/issues/6166): apparently something with NOINLINE is getting inlined 
  - [\#9388](https://gitlab.haskell.org//ghc/ghc/issues/9388) has ideas and preliminary work on how to limit the scope of the hack
  - Search for "`replicateM`" to find other tickets
  - [\#7411](https://gitlab.haskell.org//ghc/ghc/issues/7411): state hack changes exception semantics
  - [\#2284](https://gitlab.haskell.org//ghc/ghc/issues/2284), [\#1168](https://gitlab.haskell.org//ghc/ghc/issues/1168), [\#7561](https://gitlab.haskell.org//ghc/ghc/issues/7561), [\#9349](https://gitlab.haskell.org//ghc/ghc/issues/9349): the state-hack "optimisation" causes much re-computation

## Type inference

- [\#12201](https://gitlab.haskell.org//ghc/ghc/issues/12201): overlapping instances and instance declarations
- [\#11515](https://gitlab.haskell.org//ghc/ghc/issues/11515): better computation for constraint holes
- [\#10614](https://gitlab.haskell.org//ghc/ghc/issues/10614): display constraints for typed holes
- [\#10179](https://gitlab.haskell.org//ghc/ghc/issues/10179): displaying kinds
- [\#10089](https://gitlab.haskell.org//ghc/ghc/issues/10089): better warnings for unused data types (involves instances)
- [\#10027](https://gitlab.haskell.org//ghc/ghc/issues/10027): export behaviour for data families
- [\#9223](https://gitlab.haskell.org//ghc/ghc/issues/9223): error message mentioning untouchables
- [\#9198](https://gitlab.haskell.org//ghc/ghc/issues/9198): slow typechecker (big types!)
- [\#9210](https://gitlab.haskell.org//ghc/ghc/issues/9210), [\#10675](https://gitlab.haskell.org//ghc/ghc/issues/10675): functional dependencies and overlapping instances
- [\#8673](https://gitlab.haskell.org//ghc/ghc/issues/8673): more generous GADT record selectors
- [\#8550](https://gitlab.haskell.org//ghc/ghc/issues/8550): Don't build recursive type-function dictionaries
- [\#8281](https://gitlab.haskell.org//ghc/ghc/issues/8281): `UnliftedFFITypes` etc
- [\#8441](https://gitlab.haskell.org//ghc/ghc/issues/8441): family instances in hs-boot files
- [\#9450](https://gitlab.haskell.org//ghc/ghc/issues/9450): need more eager checking of compatibility with hs-boot files
- [\#7259](https://gitlab.haskell.org//ghc/ghc/issues/7259): Eta-expansion of products in System FC
- [\#7204](https://gitlab.haskell.org//ghc/ghc/issues/7204): a class to controll FFI marshalling; and newtype deriving for classes with ATs
- [\#7842](https://gitlab.haskell.org//ghc/ghc/issues/7842): polymorphism in recursive do-blocks
- [\#8171](https://gitlab.haskell.org//ghc/ghc/issues/8171): extended default rules
- [\#7828](https://gitlab.haskell.org//ghc/ghc/issues/7828), [\#5267](https://gitlab.haskell.org//ghc/ghc/issues/5267), [\#5777](https://gitlab.haskell.org//ghc/ghc/issues/5777), [\#5333](https://gitlab.haskell.org//ghc/ghc/issues/5333), [\#344](https://gitlab.haskell.org//ghc/ghc/issues/344): bugs in arrows
- [\#7862](https://gitlab.haskell.org//ghc/ghc/issues/7862): overlap beween givens and instances

## Template Haskell


See also [TemplateHaskell](template-haskell)

- [\#10267](https://gitlab.haskell.org//ghc/ghc/issues/10267): typed holes and TH
- [\#10548](https://gitlab.haskell.org//ghc/ghc/issues/10548): partial type sigs and TH
- [\#10047](https://gitlab.haskell.org//ghc/ghc/issues/10047): inconsistency in name binding between splice and quasiquotation
- [\#10486](https://gitlab.haskell.org//ghc/ghc/issues/10486): allow `addTopDecls` to create annotations
- [\#10541](https://gitlab.haskell.org//ghc/ghc/issues/10541): reify kind information
- [\#10385](https://gitlab.haskell.org//ghc/ghc/issues/10385): TH and annotations
- [\#10279](https://gitlab.haskell.org//ghc/ghc/issues/10279), [wiki page](template-haskell/package-key-changes): `reifyPackage`
- [\#10330](https://gitlab.haskell.org//ghc/ghc/issues/10330): better TH error locations
- [\#6089](https://gitlab.haskell.org//ghc/ghc/issues/6089): nested declaration splices
- [\#10271](https://gitlab.haskell.org//ghc/ghc/issues/10271): difficulty resolving overloading in typed TH
- [TemplateHaskell/Annotations](template-haskell/annotations) Gergely's work on annotations in TH
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): allow splicing of import declarations (5 in cc list)
- [\#1012](https://gitlab.haskell.org//ghc/ghc/issues/1012): mutually recursive modules and TH
- [\#5959](https://gitlab.haskell.org//ghc/ghc/issues/5959): top level splices and renaming
- [\#5416](https://gitlab.haskell.org//ghc/ghc/issues/5416): local modules
- [\#4372](https://gitlab.haskell.org//ghc/ghc/issues/4372): better quasiquotation support
- [\#2041](https://gitlab.haskell.org//ghc/ghc/issues/2041): Splicing in concrete syntax
- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Allow TH to generate import declarations        
- [\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340): Better TH error recovery.  Easy to do; change to signature of qRecover.
- [\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222): design of reification for TH

## Demand analysis and CSE

- [\#10918](https://gitlab.haskell.org//ghc/ghc/issues/10918): float in used-once things into recursive bindings
- [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782): late demand analysis
- [\#10069](https://gitlab.haskell.org//ghc/ghc/issues/10069): bad reboxing in data types with many fields
- [\#8655](https://gitlab.haskell.org//ghc/ghc/issues/8655): analysis for quick-to-evaluate thunks
- [\#6070](https://gitlab.haskell.org//ghc/ghc/issues/6070), [\#5949](https://gitlab.haskell.org//ghc/ghc/issues/5949), [\#5775](https://gitlab.haskell.org//ghc/ghc/issues/5775), [\#4267](https://gitlab.haskell.org//ghc/ghc/issues/4267), [\#5302](https://gitlab.haskell.org//ghc/ghc/issues/5302), [\#6087](https://gitlab.haskell.org//ghc/ghc/issues/6087): Demand analysis
- [\#5075](https://gitlab.haskell.org//ghc/ghc/issues/5075), [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138): CPR for sum types (mostly done, needs finishing)
- [\#10678](https://gitlab.haskell.org//ghc/ghc/issues/10678), [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289), [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387), [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600), [\#1885](https://gitlab.haskell.org//ghc/ghc/issues/1885): nested CPR analysis
- [\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596), [\#5996](https://gitlab.haskell.org//ghc/ghc/issues/5996), [\#149](https://gitlab.haskell.org//ghc/ghc/issues/149), [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940), [\#947](https://gitlab.haskell.org//ghc/ghc/issues/947), [\#701](https://gitlab.haskell.org//ghc/ghc/issues/701), [\#5344](https://gitlab.haskell.org//ghc/ghc/issues/5344), [\#9441](https://gitlab.haskell.org//ghc/ghc/issues/9441): CSE opportunities; see [MoreCSE](more-cse)
- [\#6040](https://gitlab.haskell.org//ghc/ghc/issues/6040), [\#5945](https://gitlab.haskell.org//ghc/ghc/issues/5945), [\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458), [\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216): lambda-lift functions with a few args, just before code gen
- [\#1171](https://gitlab.haskell.org//ghc/ghc/issues/1171): strictness and exceptions (long, open ticket)

### Arity

- [\#10181](https://gitlab.haskell.org//ghc/ghc/issues/10181): unsound eta-reduction
- [\#11029](https://gitlab.haskell.org//ghc/ghc/issues/11029): eta expansion causes slow-down
- [\#11146](https://gitlab.haskell.org//ghc/ghc/issues/11146): manual eta expansion is a big win
- [\#10260](https://gitlab.haskell.org//ghc/ghc/issues/10260), [\#9020](https://gitlab.haskell.org//ghc/ghc/issues/9020), [\#10319](https://gitlab.haskell.org//ghc/ghc/issues/10319): not enough eta expansion
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

[Inlining wiki page](inlining)

- [\#13011](https://gitlab.haskell.org//ghc/ghc/issues/13011), [\#11240](https://gitlab.haskell.org//ghc/ghc/issues/11240), [\#8833](https://gitlab.haskell.org//ghc/ghc/issues/8833), [\#3872](https://gitlab.haskell.org//ghc/ghc/issues/3872), [\#5400](https://gitlab.haskell.org//ghc/ghc/issues/5400), [\#5448](https://gitlab.haskell.org//ghc/ghc/issues/5448), [\#5722](https://gitlab.haskell.org//ghc/ghc/issues/5722), [\#7057](https://gitlab.haskell.org//ghc/ghc/issues/7057), [\#7369](https://gitlab.haskell.org//ghc/ghc/issues/7369), [\#9235](https://gitlab.haskell.org//ghc/ghc/issues/9235): Russell's paradox; Y combinator; contravariant data type loop in simplifier inliner
- [\#4833](https://gitlab.haskell.org//ghc/ghc/issues/4833): finding the right loop breaker
- [\#3781](https://gitlab.haskell.org//ghc/ghc/issues/3781), [\#3755](https://gitlab.haskell.org//ghc/ghc/issues/3755): inlining for local functions: discount for scrutinising free vars
- [\#3765](https://gitlab.haskell.org//ghc/ghc/issues/3765): CONLIKE things and case expressions (need two unfoldings)
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073) and [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances): better desugaring for instances
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123): feature request: recursive inlining and peeling
- [\#4960](https://gitlab.haskell.org//ghc/ghc/issues/4960): better inlining tests

## Optimisation

- [\#13227](https://gitlab.haskell.org//ghc/ghc/issues/13227): loss of full laziness in `mapFB`
- [\#11475](https://gitlab.haskell.org//ghc/ghc/issues/11475): Lint should check for inexhaustive cases expressions
- [\#9476](https://gitlab.haskell.org//ghc/ghc/issues/9476): late lambda lifting
- [\#7374](https://gitlab.haskell.org//ghc/ghc/issues/7374), [\#10417](https://gitlab.haskell.org//ghc/ghc/issues/10417), [\#10418](https://gitlab.haskell.org//ghc/ghc/issues/10418), [\#7287](https://gitlab.haskell.org//ghc/ghc/issues/7287), [\#7398](https://gitlab.haskell.org//ghc/ghc/issues/7398), [\#10528](https://gitlab.haskell.org//ghc/ghc/issues/10528), [\#11688](https://gitlab.haskell.org//ghc/ghc/issues/11688), [\#10595](https://gitlab.haskell.org//ghc/ghc/issues/10595), [\#7141](https://gitlab.haskell.org//ghc/ghc/issues/7141), [\#5973](https://gitlab.haskell.org//ghc/ghc/issues/5973), [\#2271](https://gitlab.haskell.org//ghc/ghc/issues/2271), [\#1434](https://gitlab.haskell.org//ghc/ghc/issues/1434): rules not firing; e.g. class methods, newtype constructors
- [\#8457](https://gitlab.haskell.org//ghc/ghc/issues/8457) (summarises), [\#917](https://gitlab.haskell.org//ghc/ghc/issues/917), [\#1945](https://gitlab.haskell.org//ghc/ghc/issues/1945), [\#3273](https://gitlab.haskell.org//ghc/ghc/issues/3273), [\#4276](https://gitlab.haskell.org//ghc/ghc/issues/4276), [\#5729](https://gitlab.haskell.org//ghc/ghc/issues/5729) (closed as dups, but not fixed): full laziness sometimes makes things worse.
- [\#10528](https://gitlab.haskell.org//ghc/ghc/issues/10528), [\#10595](https://gitlab.haskell.org//ghc/ghc/issues/10595): RULES for class methods
- [\#10535](https://gitlab.haskell.org//ghc/ghc/issues/10535), [\#7367](https://gitlab.haskell.org//ghc/ghc/issues/7367), [\#7206](https://gitlab.haskell.org//ghc/ghc/issues/7206), [\#7309](https://gitlab.haskell.org//ghc/ghc/issues/7309): float-out and excessive sharing 
- [\#9370](https://gitlab.haskell.org//ghc/ghc/issues/9370), [\#8635](https://gitlab.haskell.org//ghc/ghc/issues/8635): respecting mixed `-O` and `-O0` in `ghc --make`, especially concerning cross-module inlining
- [\#9279](https://gitlab.haskell.org//ghc/ghc/issues/9279): local wrapper remains in final program
- [\#9246](https://gitlab.haskell.org//ghc/ghc/issues/9246): Too much case-of-case leads to bad code
- [\#9136](https://gitlab.haskell.org//ghc/ghc/issues/9136): better constant folding
- [\#9041](https://gitlab.haskell.org//ghc/ghc/issues/9041): bad code from native code generator (NCG)
- [\#6056](https://gitlab.haskell.org//ghc/ghc/issues/6056): conflict between the w/w unfolding and the one from INLINEABLE
- [\#8472](https://gitlab.haskell.org//ghc/ghc/issues/8472): top level unlifted string literals
- [\#8335](https://gitlab.haskell.org//ghc/ghc/issues/8335): more gc entry points
- [\#8336](https://gitlab.haskell.org//ghc/ghc/issues/8336): sinking pass does not sink
- [\#8327](https://gitlab.haskell.org//ghc/ghc/issues/8327): dead code (in heap checks) not eliminated by C--
- [\#8317](https://gitlab.haskell.org//ghc/ghc/issues/8317), [\#8326](https://gitlab.haskell.org//ghc/ghc/issues/8326), [\#9661](https://gitlab.haskell.org//ghc/ghc/issues/9661), [\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231), [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): don't push heapchecks into case branches too aggressively.  This is closely related to work Jan Stolarek did on unboxed booleans [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135); and perhaps also to [\#10124](https://gitlab.haskell.org//ghc/ghc/issues/10124), [\#10137](https://gitlab.haskell.org//ghc/ghc/issues/10137)
- [\#8308](https://gitlab.haskell.org//ghc/ghc/issues/8308): resurrect ticky histograms
- [\#8321](https://gitlab.haskell.org//ghc/ghc/issues/8321): branch prediction for stack/heap checks for LLVM
- [\#8279](https://gitlab.haskell.org//ghc/ghc/issues/8279): alignment and its effect on performance
- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498): heap checks in recursive functions
- [\#7378](https://gitlab.haskell.org//ghc/ghc/issues/7378): (a) identical-alts when the RHSs are bigger; (b) nested comparisions (cf [\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132))
- [\#7511](https://gitlab.haskell.org//ghc/ghc/issues/7511): inlining can make programs allocate MORE!!
- [\#7307](https://gitlab.haskell.org//ghc/ghc/issues/7307): low hanging fruit for shrinking code size for string constants and top-level indirections
- [\#5954](https://gitlab.haskell.org//ghc/ghc/issues/5954): performance regression. Duplication of primops?
- [\#6047](https://gitlab.haskell.org//ghc/ghc/issues/6047): GHC retains unnecessary binding
- [\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439): Strict dictionaries; see also map/coerce rule [\#9792](https://gitlab.haskell.org//ghc/ghc/issues/9792)
- [\#5916](https://gitlab.haskell.org//ghc/ghc/issues/5916), [\#10678](https://gitlab.haskell.org//ghc/ghc/issues/10678): `runST` isn't free
- [\#5522](https://gitlab.haskell.org//ghc/ghc/issues/5522), [\#6092](https://gitlab.haskell.org//ghc/ghc/issues/6092): liberate-case runs out of memory
- [\#3990](https://gitlab.haskell.org//ghc/ghc/issues/3990), [\#7647](https://gitlab.haskell.org//ghc/ghc/issues/7647): UNPACK for data families, and with phantom types, needs data con wrapper refactoring; see also [\#9655](https://gitlab.haskell.org//ghc/ghc/issues/9655) if you are in this area.
- [\#5928](https://gitlab.haskell.org//ghc/ghc/issues/5928): running Specialise more than once

## Numerics, floating point, and primops

- [\#10555](https://gitlab.haskell.org//ghc/ghc/issues/10555): division primops on LHS of a RULE
- [\#9407](https://gitlab.haskell.org//ghc/ghc/issues/9407): 64-bit floating point behaves differently with and without -O
- [\#5780](https://gitlab.haskell.org//ghc/ghc/issues/5780): aggressive-primop attempt that didn't quite work
- [\#9328](https://gitlab.haskell.org//ghc/ghc/issues/9328), [\#10215](https://gitlab.haskell.org//ghc/ghc/issues/10215): negative zero and case expressions
- [\#9304](https://gitlab.haskell.org//ghc/ghc/issues/9304): floating point woes: 32 vs 64 bit
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615): poor code for div with powers of 2
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101): constant folding for `(**)`
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676): `realToFrac` conversions
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744): comparisons against `minBound` and `maxBound` are not optimised away
- [\#2269](https://gitlab.haskell.org//ghc/ghc/issues/2269): Word type to Double or Float conversions 
- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070): NaNs and divide-by-zero
- [\#9251](https://gitlab.haskell.org//ghc/ghc/issues/9251): branchless max/min
- [\#9276](https://gitlab.haskell.org//ghc/ghc/issues/9276): auditing for IEEE compliance

## Features

- [\#9049](https://gitlab.haskell.org//ghc/ghc/issues/9049): explicit call stack, abstractable call site information
- [\#5972](https://gitlab.haskell.org//ghc/ghc/issues/5972): option to suppress record selectors

---

## Cmm and code generation


See [Edward's collection of code-generation stupidities](commentary/compiler/new-code-gen-stupidity)

- [\#10012](https://gitlab.haskell.org//ghc/ghc/issues/10012): Cheap-to-compute values aren't pushed into case branches
- [\#9718](https://gitlab.haskell.org//ghc/ghc/issues/9718): avoiding `TidyPgm` having to predict arity and CAF-ref-ness
- [\#9159](https://gitlab.haskell.org//ghc/ghc/issues/9159): jump tables for dense cases
- [\#9157](https://gitlab.haskell.org//ghc/ghc/issues/9157): common block elimination
- [\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905): spilling around an eval
- [\#8903](https://gitlab.haskell.org//ghc/ghc/issues/8903): dead stores
- [\#8887](https://gitlab.haskell.org//ghc/ghc/issues/8887): double assignment
- [\#8871](https://gitlab.haskell.org//ghc/ghc/issues/8871): Cmm optimisation opportunity
- [\#8585](https://gitlab.haskell.org//ghc/ghc/issues/8585), [\#11372](https://gitlab.haskell.org//ghc/ghc/issues/11372): loopification; see also [Commentary/Compiler/Loopification](commentary/compiler/loopification)
- Summary ticket: [\#4258](https://gitlab.haskell.org//ghc/ghc/issues/4258), and [wiki page](commentary/compiler/new-code-gen)
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462): allocate large objects with `allocateLocal()`
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289): cheap check at start of case alternatives
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731): avoiding unnecessary evaluation when unpacking constructors
- [\#4121](https://gitlab.haskell.org//ghc/ghc/issues/4121): arity and CAF info computation is horribly fragile

### Optimiser

- [\#10346](https://gitlab.haskell.org//ghc/ghc/issues/10346), [\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255), [\#3767](https://gitlab.haskell.org//ghc/ghc/issues/3767), [\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642), [\#3831](https://gitlab.haskell.org//ghc/ghc/issues/3831), [\#4941](https://gitlab.haskell.org//ghc/ghc/issues/4941), [\#2598](https://gitlab.haskell.org//ghc/ghc/issues/2598): Improve **`SpecConstr`** in various ways, including for free variables, and for join points.
- [\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059): specialise on value arguments
- [\#5974](https://gitlab.haskell.org//ghc/ghc/issues/5974): casts, RULES, and parametricity
- [\#5262](https://gitlab.haskell.org//ghc/ghc/issues/5262): seq magic
- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605): strict/unboxed enumerations
- [\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607): space leak: inlining defeats selector thunk optimisation
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470): merge identical counters
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988), [\#11197](https://gitlab.haskell.org//ghc/ghc/issues/11197): better float-in
- [\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374): SAT and `MutableByteArray`        Max?
- [\#7080](https://gitlab.haskell.org//ghc/ghc/issues/7080): inconsistent treatment of RULES and SPECIALISE
- [\#876](https://gitlab.haskell.org//ghc/ghc/issues/876): make `length` into a good consumer. Perhaps using `foldl`?

## GHCi

- [\#9394](https://gitlab.haskell.org//ghc/ghc/issues/9394): Display type/data family instance information in `:info`
- [\#4017](https://gitlab.haskell.org//ghc/ghc/issues/4017): unhelpful GHCi message

---

## Outright bugs

- [\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241): Lifting the Coverage Condition for functional dependencies isn’t the Right Thing        Manuel

---

## Types and type inference

- [TypeFunctionsStatus](type-functions-status)
- [\#10338](https://gitlab.haskell.org//ghc/ghc/issues/10338): delicacy in generalisation: `MonoLocalBinds` isn't enough
- [\#9637](https://gitlab.haskell.org//ghc/ghc/issues/9637): type level error messages
- [\#9587](https://gitlab.haskell.org//ghc/ghc/issues/9587), [\#9607](https://gitlab.haskell.org//ghc/ghc/issues/9607): `-XAllowAmbiguousTypes` debate
- [\#9334](https://gitlab.haskell.org//ghc/ghc/issues/9334): instance chains
- [\#9427](https://gitlab.haskell.org//ghc/ghc/issues/9427): SCC analysis for type and class decls
- [\#9422](https://gitlab.haskell.org//ghc/ghc/issues/9422): Glomming all instances in EPT is too crude for `--make`
- [\#7503](https://gitlab.haskell.org//ghc/ghc/issues/7503): Kind polymorphism and mutual recursion
- [\#4296](https://gitlab.haskell.org//ghc/ghc/issues/4296): SkolemOccurs
- [\#816](https://gitlab.haskell.org//ghc/ghc/issues/816). [\#3108](https://gitlab.haskell.org//ghc/ghc/issues/3108): extreme delicacy in solve order, involving fundeps
- [\#8109](https://gitlab.haskell.org//ghc/ghc/issues/8109): as-patterns in type-family declarations
- [\#6065](https://gitlab.haskell.org//ghc/ghc/issues/6065): GHC suggests a type signature that it then rejects
- [\#5320](https://gitlab.haskell.org//ghc/ghc/issues/5320), [\#7296](https://gitlab.haskell.org//ghc/ghc/issues/7296): overlap delicacy
- [\#5224](https://gitlab.haskell.org//ghc/ghc/issues/5224): make it cheaper to check for inconsistent type family instances
- [\#4466](https://gitlab.haskell.org//ghc/ghc/issues/4466), [\#5296](https://gitlab.haskell.org//ghc/ghc/issues/5296), [\#8631](https://gitlab.haskell.org//ghc/ghc/issues/8631): explicit type application
- [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): overlapping type family instances
- [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490): superclasses and ambiguity
- [\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632): better update for records with existentials
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641): revise what `-XExtendedDefaultRules` does

### Better error messages

- [\#10450](https://gitlab.haskell.org//ghc/ghc/issues/10450): poor error message when arg is insufficiently polymorphic
- [\#9901](https://gitlab.haskell.org//ghc/ghc/issues/9901): `f is applied to two arguments, but its type has only two`
- [\#9456](https://gitlab.haskell.org//ghc/ghc/issues/9456): more info in "relevant bindings" message
- [\#9244](https://gitlab.haskell.org//ghc/ghc/issues/9244): suggest scoped type variables
- [\#9173](https://gitlab.haskell.org//ghc/ghc/issues/9173): inferred/expected error messages
- [\#1330](https://gitlab.haskell.org//ghc/ghc/issues/1330): another bad error message (Church2)
- [\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648): Report out of date interface files robustly        
- [\#1928](https://gitlab.haskell.org//ghc/ghc/issues/1928): Confusing type error message (Claus makes suggestions)        

---

## Features

- [\#6024](https://gitlab.haskell.org//ghc/ghc/issues/6024): allow defining a *kind* without also getting the corresponding *type*
- [\#5927](https://gitlab.haskell.org//ghc/ghc/issues/5927): Constraints with universal quantification
- [\#5429](https://gitlab.haskell.org//ghc/ghc/issues/5429): docase and joinads
- [\#5073](https://gitlab.haskell.org//ghc/ghc/issues/5073): `blockST` and friends
- [\#2895](https://gitlab.haskell.org//ghc/ghc/issues/2895): Class aliases 
- [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595): record updates
- [\#4823](https://gitlab.haskell.org//ghc/ghc/issues/4823): strength reduction for array indexing
- [\#4479](https://gitlab.haskell.org//ghc/ghc/issues/4479): type directed name resolution (TDNR)
- [\#4426](https://gitlab.haskell.org//ghc/ghc/issues/4426): simpler rule for implicit quantification
- [\#3701](https://gitlab.haskell.org//ghc/ghc/issues/3701): Implicitly declared existentials: a class as a type
- [\#3217](https://gitlab.haskell.org//ghc/ghc/issues/3217): better flag handling for GHCi
- [\#2600](https://gitlab.haskell.org//ghc/ghc/issues/2600), [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110): **Bind type variables and constraints in RULES**
- [\#960](https://gitlab.haskell.org//ghc/ghc/issues/960): Generate local info for ‘undefined’.  Implicit location parameters in general        
- [\#2135](https://gitlab.haskell.org//ghc/ghc/issues/2135): Warn when exporting a function whose type mentions a type constructor defined locally but not itself exported        
- [\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119): Explicitly importing deprecated symbols should generate deprecation warnings        
- [\#2207](https://gitlab.haskell.org//ghc/ghc/issues/2207): Load interfaces for GHC.\* even without -O        
- [\#1231](https://gitlab.haskell.org//ghc/ghc/issues/1231): Better deprecations        

---

## Not sure what to do about these

- [\#7897](https://gitlab.haskell.org//ghc/ghc/issues/7897): make the fingerprint in a `TypeRep` be a proper fingerprint, including the type definition all the way down (see [ Recompilation avoidance](http://hackage.haskell.org/trac/ghc/wiki/DependencyTracking))
- [\#4005](https://gitlab.haskell.org//ghc/ghc/issues/4005): generational GC pathalogical case
- [\#1349](https://gitlab.haskell.org//ghc/ghc/issues/1349): strict function argument types
