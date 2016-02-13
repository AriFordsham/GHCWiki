# Semigroup (as superclass of) Monoid Proposal


THIS DESCRIPTION IS STILL WORK IN PROGRESS


Please comment on ghc:\#10365 if you notice some show-stopper issue


Introducing `Semigroup` as a superclass of `Monoid` has been proposed several times (in reverse chronological order):

- [ http://thread.gmane.org/gmane.comp.lang.haskell.libraries/24494](http://thread.gmane.org/gmane.comp.lang.haskell.libraries/24494)
- [ http://thread.gmane.org/gmane.comp.lang.haskell.libraries/19649](http://thread.gmane.org/gmane.comp.lang.haskell.libraries/19649)
- TODO ...

## Final API


The final API (suitable for Haskell Report inclusion) we want to end up with is

```
moduleData.SemigroupwhereclassSemigroup a where(<>):: a -> a -> a

    sconcat ::NonEmpty a -> a
    sconcat (a :| as)= go a as
      where
        go b (c:cs)= b <> go c cs
        go b []= b

    -- GHC extension, not needed for Haskell Report
    stimes ::Integral b => b -> a -> a
    stimes y0 x0 ={- default impl -}
```

```
moduleData.MonoidwhereclassSemigroup a =>Monoid a where
    mempty  :: a

    mconcat ::[a]-> a
    mconcat = foldr (<>) mempty

    -- GHC extension, not needed for Haskell Report
    mtimes ::Integral b => b -> a -> a
    mtimes y0 x0 ={- default impl -}-- GHC Extension: Legacy alias not needed for Haskell Reportmappend::Semigroup a => a -> a -> a
mappend=(<>)
```

## Migration plan

<table><tr><th>**Milestone**</th>
<th>**GHC release**</th>
<th>**Date**</th>
<th>**What changed?**</th>
<th>**Action required**</th></tr>
<tr><th>**warnings introduced**</th>
<th> 8.0               </th>
<th> Winter 2016       </th>
<th>`-Wmonoid-semigroup` introduced                                     </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>*optional* Add Semigroup instances  and `semigroups` dependency 
</th></tr>
<tr><th>**warnings actionable**</th>
<th> 8.4               </th>
<th> Winter 2018       </th>
<th>`Data.Semigroup` available in `base` under three-release policy     </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th> Add `Semigroup` instances                                         
</th></tr>
<tr><th>**warnings become errors**</th>
<th> TDB               </th>
<th> TBD               </th>
<th>`Semigroup` becomes superclass of `Monoid`</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th> Remove `mappend` from `Monoid` instances                          
</th></tr>
<tr><th>**warnings become errors**</th>
<th> TDB               </th>
<th> TBD               </th>
<th>`mappend` removed from `Monoid`</th>
<th></th></tr></table>

## Phases

### Phase 1 (GHC 8.0)  [\#10365](https://gitlab.haskell.org//ghc/ghc/issues/10365)

- Move `Data.Semigroup` & `Data.List.NonEmpty` from `semigroups-0.18` to `base`.

- (maybe) Implement a warning about definitions of an operator named `(<>)` that indicate it will be coming into Prelude in 8.2. We should warn about missing Semigroup instances at any use site of `(<>)` as they'll break in 8.2.

### Phase 2a

- move `Semigroup` class into prelude in anticipation of it becoming a superclass of `Monoid`

### Phase 2b (either merge with 2a or with 3)

- Make `Semigroup` a superclass of `Monoid`

### Phase 3

- Deprecate manual definitions of `mappend` (c.f. "Monad of no `return` Proposal") 
- encourage overriding the current default-implementation of `(<>)` via `MINIMAL` pragma

### Phase 4

- Move the now deprecated `mappend` method out of the `Monoid` class, and possibly turn `mappend` into a legacy top-level binding (c.f. "Monad of no `return` Proposal")

## Writing compatible code

```
importData.Semigroup-- re-exports Data.Monoid (w/ Semigroup((<>)))instanceSemigroupFoowhere(<>)=…instanceMonoidFoowhere
  mempty =…#if!(MIN_VERSION_base(5,0,0))-- assumption: Semigroup becomes superclass w/ base-5.0.0
  mappend =(<>)#endif
```

---

TODO ...integrate migration roadmap outlined in [ http://permalink.gmane.org/gmane.comp.lang.haskell.libraries/24526](http://permalink.gmane.org/gmane.comp.lang.haskell.libraries/24526)