Introducing Semigroup as a superclass of Monoid has been proposed several times (in reverse chronological order):

* http://thread.gmane.org/gmane.comp.lang.haskell.libraries/24494
* http://thread.gmane.org/gmane.comp.lang.haskell.libraries/19649
* TODO

## Final API

The final API (suitable for Haskell Report inclusion) we want to end up with is

```haskell
module Prelude 
    ( Semigroup((<>))
    , Monoid(mempty, mconcat)
    -- …
    ) where

-- …

module Data.Semigroup where

class Semigroup a where
    (<>) :: a -> a -> a

    -- not (re)exported from Prelude
    sconcat :: NonEmpty a -> a
    sconcat (a :| as) = go a as
      where
        go b (c:cs) = b <> go c cs
        go b []     = b

    -- GHC extension, not needed for Haskell Report
    -- & not (re)exported from Prelude
    stimes :: Integral b => b -> a -> a
    stimes y0 x0 = {- default impl -}
```

```haskell
module Data.Monoid where

class Semigroup a => Monoid a where
    mempty  :: a

    mconcat :: [a] -> a
    mconcat = foldr (<>) mempty

    -- GHC extension, not needed for Haskell Report
    -- & not (re)exported from Prelude
    mtimes :: Integral b => b -> a -> a
    mtimes y0 x0 = {- default impl -}

-- GHC Extension: Legacy alias not needed for Haskell Report
mappend :: Semigroup a => a -> a -> a
mappend = (<>)
```

## Migration plan

### Phase 1 (GHC 8.0) ​ghc:#10365

Move Data.Semigroup & Data.List.NonEmpty from semigroups-0.18 to base. 

(maybe) Implement a warning about definitions of an operator named (<>) that indicate it will be coming into Prelude in 8.2. We should warn about missing Semigroup instances at any use site of (<>) as they'll break in phase 2. 

### Phase 2a (GHC 8.4) ​ghc:#14191

move Semigroup class into prelude in anticipation of it becoming a superclass of Monoid 

### Phase 2b (GHC 8.4) ​ghc:#14191

Make Semigroup a superclass of Monoid 

### Phase 3

Deprecate manual definitions of mappend (c.f. "Monad of no return Proposal")
    encourage overriding the current default-implementation of (<>) via MINIMAL pragma 

### Phase 4

Move the now deprecated mappend method out of the Monoid class, and possibly turn mappend into a legacy top-level binding (c.f. "Monad of no return Proposal") 

## Writing compatible code
### Recommended Variant

The code below is expected to be -Wcompat -Wall clean (see also ​https://groups.google.com/forum/#!msg/haskell-core-libraries/PyxpE2ebS9Q/Ni0ywo_GCgAJ)

```haskell
import Data.Semigroup as Sem
-- base >= 4.8: `Monoid` class is exported via `Prelude`
-- base < 4.11: re-exports `Monoid` class & common newtype wrappers
-- base >= 4.11: doesn't reexport `Monoid` class anymore

instance Sem.Semigroup Foo where
  (<>) = …

instance Monoid Foo where
  mempty = …

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
#endif
```

If you need compatiblity with GHC prior to version 8.0 you can avoid -XCPP by depending conditionally on semigroups via

```haskell
if !impl(ghc >= 8.0)
  build-depends: semigroups == 0.18.*
```

to provide a legacy Semigroup class.
Alternative w/ conditionally defined Semigroup instance & -XCPP

This variant can be used if you require support for GHC < 8.0 and you do not want to depend on the semigroups package.

```haskell
#if MIN_VERSION_base(4,9,0)
-- Data.Semigroup was added in base-4.9
import Data.Semigroup as Sem
#endif
#if !(MIN_VERSION_base(4,8,0))
-- starting with base-4.8, Monoid is rexported from Prelude
import Data.Monoid
#endif

appendFoo :: Foo -> Foo -> Foo
appendFoo = …

#if MIN_VERSION_base(4,9,0)
instance Sem.Semigroup Foo where
  (<>) = appendFoo
#endif

instance Monoid Foo where
  mempty = …

#if MIN_VERSION_base(4,11,0)
-- starting with base-4.11, mappend definitions are redundant;
-- at some point `mappend` will be removed from `Monoid`
#elif MIN_VERSION_base(4,9,0)
  mappend = (Sem.<>)
#else // base < 4.9
-- prior to GHC 8.0 / base-4.9 where no `Semigroup` class existed
  mappend = appendFoo
#endif
```

