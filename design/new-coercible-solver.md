
(In the text below, "I" = Richard Eisenberg)

## Proposed change


In the work on my Dependent Haskell branch, I need to think about representational kind equalities (explanation below). The problem is that nominal equality and representational equality are treated very, very differently in the solver. It seems unifying the two treatments, as much as possible, would be good. Concretely, I propose a new constructor for `PredTree`, `EqReprPred`, that would be quite like `EqPred` but for representational equality. (It's possible that we should just add a new `Role` field to `EqPred`, but my hunch is that representational equality and nominal equality are still different enough that a new top-level constructor will be easier to work with.) We would then have functions like `canEqRepr` in TcCanonical to canonicalize representational equality predicates, and corresponding changes in TcInteract.

## Benefits


There are several benefits of this change:

- I think a more similar treatment of representational equality and nominal equality would be easier to think about. It happens to be true that `Coercible` is a class, but looking back now, that fact seems like more historical accident than the Right Design (though I most certainly agreed with making `Coercible` a class at the time). After all, `Coercible`'s nearest cousin is `(~)`, which is resolutely *not* a class.

- A custom canonicalizer for representational equality would allow code like

```
importData.Type.CoercionimportData.Coercefoo::Coercible(Maybe a)(Maybe b)=>Coercion a b
foo=Coercion
```

>
> That module is currently rejected, because GHC can't unpack the `Coercible a b` from the given.

- To my shock and horror, if I want kind equalities, I need the solver to think about both lifted and unlifted nominal equality (I knew that) and also (very sadly) lifted and unlifted representational equality. The current setup has just enough room for me to somewhat-ungracefully shoehorn unlifted nominal equality into the solver, but there's just no way to squeeze in unlifted representational equality without drastic changes.

- This change might address [\#9117](https://gitlab.haskell.org//ghc/ghc/issues/9117) more fully.

## Drawbacks


This change would add more complexity to something already complex.

## Explanation of representational kind equalities in Dependent Haskell


(This section can safely be skipped.)


When we kind-cast a type like `t |> g`, the `g` must be representational. This is to have uniformity with the term level, and therefore the ability to promote more expressions to types. For the system to stay glued together, we also must be able to prove `t ~N (t |> g)` -- in other words, that coercions are irrelevant, even in nominal equality.


The enhanced FC has a coercion former `kind` that extract a kind equality out of a heterogeneous type equality. Suppose `t :: k1)`, `g :: k1 ~R k2`, and `h :: t ~N (t |> g)`. Then, `kind h :: k1 ~? k2`. It would be disastrous for `kind h` to have a nominal role, because we've effectively then promoted `g`'s representational role to be nominal. Thus, it is necessary for `kind h` always to be representational.


Now, suppose we have `t :: k1`, `s :: k2`, and `[G] g :: t ~N s`. I've retained the invariant that canonical equalities are homogeneous, so that they can be used in substitution. (An alternate design would be to let them be heterogeneous but then add a homogenizing coercion during substitution, but that seems strictly worse.) 
During canonicalization, I wish to break apart `g` into two equalities, `[G] h1 :: k1 ~ k2` and `[G] h2 :: t ~ (s |> sym h1)`. What should the roles be? The evidence for `h1` must be `kind g`. Thus, `h1`'s role is representational. Given that `g` has a nominal role, we would want `h2`'s role to be nominal as well. So, we get `[G] h1 :: k1 ~R k2` and `[G] h2 :: t ~N (s |> sym h1)`, and we can continue canonicalizing. And, here is where the problem shows up: we now have `h1`, an unavoidable (to me) representational kind equality.

## Conclusion


I'm happy to do this work, and would do the majority of the rewrite in the master branch and then merge into my Dependent Haskell branch.


What do we think of this?

# Implementation notes


Nov 17, 2014: Essentially, I want to parameterize the handling of equalities over the choice of equality relation. There are two (non-trivial) equality relations in play: nominal and representational. Currently, everything in the solver thinks only in terms of nominal equality, and I'm looking to generalize it. There are a few consequences of this design:

- I've written a new type `data EqRel = NomEq | ReprEq` in Type.lhs. The motivation for the new datatype (instead of just using `Role`) is because we don't want phantom equalities (which are meaningless) floating around in the solver. But I may revisit this decision.

- The `EqPred` constructor of `PredTree` now has a field of type `EqRel`.

- We can just use `topNormaliseNewType_maybe` to unwrap newtypes in the canonicalizer. I thought, at one point (see "Old, wrong idea" below) that we should use flattening to squeeze out newtypes, much like it is used to get rid of type functions. However, this kind of power is *not* necessary. The flattener works bottom-up in types, getting rid of all type family applications. This is helpful when dealing with type families because flattening a part of a type might get a reduction to fire higher up in the tree. However, newtypes can *always* fire, and so simplifying lower down in the tree doesn't help. It's much simpler just to unwrap in the canonicalizer.

- A further consequence is that we now need to store representational `CTyEqCan`s in the inert set, for use during representational flattening. But, these should be separated from nominal `CTyEqCan`s, because most of the time, we only want the nominal ones. I've added a new field `inert_repr_eqs` to `InertCans` that's just like `inert_eqs` but with representational equalities only. This new field is used when zonking tyvars in the flattener.


This all seems like a lot of work, but it also seems like it will create a nice structure, treating nominal equality and representational equality as "equals" -- neither one is really more central to the solver design. Having them treated in parallel seems to be the most robust design.

## Old, wrong idea

- As I was updating the cases of `can_eq_nc'`, I was trying to figure out how to deal with newtypes. In previous implementation plans, I thought more of the struggle to deal with `AppTy`s, which have funny roles. (The type on the right of an `AppTy` must be nominal.) Bit. of course, the canonicalizer must also deal with newtypes. I thought of writing a clause of `can_eq_nc'` which unwraps one newtype, but I got worried about recursive newtypes and infinite loops. (Recall that recursive newtypes are allowed without any extensions, and I *don't* want GHC to be able to loop without any extensions enabled!) So, I was stuck.

>
> Then, I hit on this key idea: *When solving for representational equality, newtypes are just like type families.* (Well, not exactly like, because newtypes can appear unsaturated, but it's close.) So, instead of trying to unwrap newtypes in the canonicalizer, it seems more sensible to unwrap newtypes in the flattener.

>
> Accordingly, I've rewritten `FlattenEnv` like this:

```wiki
data FlattenEnv
  = FE { fe_mode   :: FlattenMode
       , fe_loc    :: CtLoc
       , fe_nature :: CtNature
       , fe_eq_rel :: EqRel }   -- See Note [Flattener EqRels]
```

>
> This gets rid of the `CtEvidence` field, which was only used for G/W/D distinction and `CtLoc`, in favor of storing the individual pieces. (The new `CtNature` type is just an enumeration between G, W, and D.) There is also now an `EqRel` field, saying the equality relation that should be respected during flattening.

>
> The idea of flattening is to replace one type with another "equivalent" type (and to produce evidence of the equality). Previously, we've always used nominal equality. But, this same idea applies equally well to representational equality. So, the `EqRel` field says what equality should be respected during flattening. If `fe_eq_rel` is `ReprEq`, then the flattener will unwrap newtypes just as it reduces type families.


The aborted implementation of this idea is at [ https://github.com/goldfirere/ghc/tree/two-flatteners](https://github.com/goldfirere/ghc/tree/two-flatteners)

## Open questions

- How do we avoid occurs-check problems? I believe that the substitution embodied by `inert_eqs` is *not* idempotent, and thus there must be a mechanism to prevent occurs-check problems. This mechanism will have to be extended to `inert_repr_eqs`. It's conceivable that zonking an individual tyvar (in a representational flattener call) will end up using equalities from both sets of equalities, possibly in an interleaved manner, so this may be delicate.
