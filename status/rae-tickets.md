# Tickets that Richard E. is interested in

## Type system

**Easy**

- [\#7494](https://gitlab.haskell.org//ghc/ghc/issues/7494): Allow type synonyms in GADT return types
- [\#8109](https://gitlab.haskell.org//ghc/ghc/issues/8109): As-patterns in type patterns
- [\#8634](https://gitlab.haskell.org//ghc/ghc/issues/8634): Dysfunctional dependencies
- [\#9122](https://gitlab.haskell.org//ghc/ghc/issues/9122): Check for bogus `unsafeCoerce`
- [\#9636](https://gitlab.haskell.org//ghc/ghc/issues/9636): Should `F Bool` be well-formed, if `F` is an empty closed type family? (blocked by [\#9637](https://gitlab.haskell.org//ghc/ghc/issues/9637))
- [\#9687](https://gitlab.haskell.org//ghc/ghc/issues/9687): Need `Typeable (,,,,,,,,,,)` and friends

**Medium**

- [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018): Injective type families (Jan is working on)
- [\#7495](https://gitlab.haskell.org//ghc/ghc/issues/7495): Allowing list syntax for `HList`
- [\#8128](https://gitlab.haskell.org//ghc/ghc/issues/8128): Derived instances sometimes have inaccessible code
- [\#8165](https://gitlab.haskell.org//ghc/ghc/issues/8165): GND should make associated types, too (not very well specified)
- [\#8388](https://gitlab.haskell.org//ghc/ghc/issues/8388): Have a consistent story around non-`*` types in a forall
- [\#9180](https://gitlab.haskell.org//ghc/ghc/issues/9180): Compile-time `staticError` function; seems easy, but I don't know how to do this.
- [\#9260](https://gitlab.haskell.org//ghc/ghc/issues/9260): Type-lits solver falls short (given to Iavor)
- [\#9427](https://gitlab.haskell.org//ghc/ghc/issues/9427): Break cycles in recursive class/type definitions (the second half of the fix for [\#9200](https://gitlab.haskell.org//ghc/ghc/issues/9200))
- [\#9547](https://gitlab.haskell.org//ghc/ghc/issues/9547): Better inference for whether `() :: *` or `() :: Constraint`
- [\#9569](https://gitlab.haskell.org//ghc/ghc/issues/9569): Tuple constraints should probably be flattened
- [\#9637](https://gitlab.haskell.org//ghc/ghc/issues/9637): Type-level `Error` that aborts compilation
- [\#9649](https://gitlab.haskell.org//ghc/ghc/issues/9649): Proper type-level strings
- [\#9667](https://gitlab.haskell.org//ghc/ghc/issues/9667): Don't make tyvars untouchable when a GADT pattern-match isn't informative

**Hard**

- [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927): Warnings are broken for GADTs (Dimitrios is working on)

  - [\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669): Empty case warnings are broken
- [\#8828](https://gitlab.haskell.org//ghc/ghc/issues/8828): Type pattern synonyms

**Rocket Science**

- [\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965): Allow existentials in newtypes
- [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259): Allow recursive checks for compatibility in type families
- [\#7259](https://gitlab.haskell.org//ghc/ghc/issues/7259): Eta expansion of products
- [\#7961](https://gitlab.haskell.org//ghc/ghc/issues/7961): Implement "nokinds" (RAE is working on!)

  - [\#9017](https://gitlab.haskell.org//ghc/ghc/issues/9017): Bad error message b/c of missing kind equality
- [\#8338](https://gitlab.haskell.org//ghc/ghc/issues/8338): Incoherent instances without `-XIncoherentInstances`
- [\#9429](https://gitlab.haskell.org//ghc/ghc/issues/9429): An alternative to `Any`. For example, we want `Typeable (forall x. x -> x)`.
- [\#9562](https://gitlab.haskell.org//ghc/ghc/issues/9562): Type families + hs-boot files = `unsafeCoerce`

## Typechecker

- [\#7947](https://gitlab.haskell.org//ghc/ghc/issues/7947): Names generated when deriving `Data` should be more unique
- [\#9450](https://gitlab.haskell.org//ghc/ghc/issues/9450): Interleave checking against an hs-boot file while typechecking definitions
- [\#9554](https://gitlab.haskell.org//ghc/ghc/issues/9554): `-XUndecidableInstances` causes runtime loop
- [\#9557](https://gitlab.haskell.org//ghc/ghc/issues/9557): Deriving instances is slow
- [\#9566](https://gitlab.haskell.org//ghc/ghc/issues/9566): Core Lint error
- [\#9567](https://gitlab.haskell.org//ghc/ghc/issues/9567): Core Lint error
- [\#9582](https://gitlab.haskell.org//ghc/ghc/issues/9582): Type families don't work in instance signatures (Andreas Abel is on the case, but stalled)
- [\#9662](https://gitlab.haskell.org//ghc/ghc/issues/9662): Stack overflow during compilatinon. (SPJ is on the case)

## Roles & such

**More/better roles**

- [\#8177](https://gitlab.haskell.org//ghc/ghc/issues/8177): Roles for type families (RAE owns, but is *not* working on)
- [\#9112](https://gitlab.haskell.org//ghc/ghc/issues/9112): GND with `Vector`/`MVector`
- [\#9118](https://gitlab.haskell.org//ghc/ghc/issues/9118): No eta-reduction possible
- [\#9123](https://gitlab.haskell.org//ghc/ghc/issues/9123): Need higher-kinded roles

**Error messages**

- [\#8984](https://gitlab.haskell.org//ghc/ghc/issues/8984)
- [\#9444](https://gitlab.haskell.org//ghc/ghc/issues/9444)
- [\#9518](https://gitlab.haskell.org//ghc/ghc/issues/9518)

**Solver**

- [\#9117](https://gitlab.haskell.org//ghc/ghc/issues/9117): Doesn't find eta expanded versions
- [\#9153](https://gitlab.haskell.org//ghc/ghc/issues/9153): Context reduction stack overflow

## Front end

- [\#2526](https://gitlab.haskell.org//ghc/ghc/issues/2526): Warn only about exported missing signatures
- [\#7169](https://gitlab.haskell.org//ghc/ghc/issues/7169): Warn about incomplete record selectors
- [\#7668](https://gitlab.haskell.org//ghc/ghc/issues/7668): Better locations in deferred type errors
- [\#9109](https://gitlab.haskell.org//ghc/ghc/issues/9109): Better "untouchable" errors
- [\#9194](https://gitlab.haskell.org//ghc/ghc/issues/9194): Remove the magic from parsing `~`. Some open design questions.
- [\#9376](https://gitlab.haskell.org//ghc/ghc/issues/9376): Improve error messages for closed type families that get stuck on the dark corners
- [\#9378](https://gitlab.haskell.org//ghc/ghc/issues/9378): Make unknown LANGUAGE pragmas warnings
- [\#9394](https://gitlab.haskell.org//ghc/ghc/issues/9394): `:info` should show instances of data and type families.
- [\#9497](https://gitlab.haskell.org//ghc/ghc/issues/9497): Report other errors before complaining about holes
- [\#9778](https://gitlab.haskell.org//ghc/ghc/issues/9778): Allow warnings for unticked promoted things
- [\#9784](https://gitlab.haskell.org//ghc/ghc/issues/9784): Report better error for `Foo.'Z`

**Medium**

- [\#7401](https://gitlab.haskell.org//ghc/ghc/issues/7401): Derive `Eq` and friends for empty datatypes

**Design needed**

- [\#7870](https://gitlab.haskell.org//ghc/ghc/issues/7870): Customized error messages

## Template Haskell

**Easy**

- [\#7808](https://gitlab.haskell.org//ghc/ghc/issues/7808): Allow reification of a data instance name
- [\#9022](https://gitlab.haskell.org//ghc/ghc/issues/9022): Fix semicolons in pretty-printer
- [\#9113](https://gitlab.haskell.org//ghc/ghc/issues/9113): Warn about incomplete patterns in quotes. Fixed by [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927)?
- [\#9699](https://gitlab.haskell.org//ghc/ghc/issues/9699): Function to list all names in scope
- [\#9703](https://gitlab.haskell.org//ghc/ghc/issues/9703): Add missing calling conventions (owned by cmsaperstein)

*Unknown*

- [\#9160](https://gitlab.haskell.org//ghc/ghc/issues/9160): Some panic with optimizations and `singletons`.
- [\#9693](https://gitlab.haskell.org//ghc/ghc/issues/9693): Stale state left in failed compilation with GHCi and TH.

**Design needed**

- [\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475): Adding imports/exports
- [\#1831](https://gitlab.haskell.org//ghc/ghc/issues/1831): `reify` never provides variable info
- [\#8679](https://gitlab.haskell.org//ghc/ghc/issues/8679): Include value and function signatures in their declarations
- [\#8761](https://gitlab.haskell.org//ghc/ghc/issues/8761): Pattern synonyms

## Generic programming

- [\#8560](https://gitlab.haskell.org//ghc/ghc/issues/8560): Generic representation for GADTs

## Documentation

- [\#8253](https://gitlab.haskell.org//ghc/ghc/issues/8253): Bad example of Core
- [\#9247](https://gitlab.haskell.org//ghc/ghc/issues/9247): Document `-XDatatypeContexts`
- [\#9248](https://gitlab.haskell.org//ghc/ghc/issues/9248): Document `-X` extensions
- [\#9665](https://gitlab.haskell.org//ghc/ghc/issues/9665): Add a "since" field to language extensions
- [\#9737](https://gitlab.haskell.org//ghc/ghc/issues/9737): Document `ANN` in pragmas chapter

## Tasks

- Search for email with subject "Branched and unbranched" in my GHC folder -- about refactoring some `CoAxiom` stuff
- Simplification for axioms: they can be applied to types, not coercions. See email with subject "checkAxInstCo"
