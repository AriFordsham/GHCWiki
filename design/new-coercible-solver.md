
For ongoing (as of Mar '15) work about the solver, see [here](design/new-coercible-solver/v2).


(In the text below, "I" = Richard Eisenberg)

## Proposed change


In the work on my Dependent Haskell branch, I need to think about representational kind equalities (explanation below). The problem is that nominal equality and representational equality are treated very, very differently in the solver. It seems unifying the two treatments, as much as possible, would be good. Concretely, I propose a new constructor for `PredTree`, `EqReprPred`, that would be quite like `EqPred` but for representational equality. (It's possible that we should just add a new `Role` field to `EqPred`, but my hunch is that representational equality and nominal equality are still different enough that a new top-level constructor will be easier to work with.) We would then have functions like `canEqRepr` in TcCanonical to canonicalize representational equality predicates, and corresponding changes in TcInteract.

## Benefits


There are several benefits of this change:

- I think a more similar treatment of representational equality and nominal equality would be easier to think about. It happens to be true that `Coercible` is a class, but looking back now, that fact seems like more historical accident than the Right Design (though I most certainly agreed with making `Coercible` a class at the time). After all, `Coercible`'s nearest cousin is `(~)`, which is resolutely *not* a class.

- A custom canonicalizer for representational equality would allow code like

```
import Data.Type.Coercion
import Data.Coerce

foo :: Coercible (Maybe a) (Maybe b) => Coercion a b
foo = Coercion
```

>
>
> That module is currently rejected, because GHC can't unpack the `Coercible a b` from the given.
>
>

- To my shock and horror, if I want kind equalities, I need the solver to think about both lifted and unlifted nominal equality (I knew that) and also (very sadly) lifted and unlifted representational equality. The current setup has just enough room for me to somewhat-ungracefully shoehorn unlifted nominal equality into the solver, but there's just no way to squeeze in unlifted representational equality without drastic changes.

- This change might address #9117 more fully.

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
>
> Then, I hit on this key idea: *When solving for representational equality, newtypes are just like type families.* (Well, not exactly like, because newtypes can appear unsaturated, but it's close.) So, instead of trying to unwrap newtypes in the canonicalizer, it seems more sensible to unwrap newtypes in the flattener.
>
>

>
>
> Accordingly, I've rewritten `FlattenEnv` like this:
>
>

```wiki
data FlattenEnv
  = FE { fe_mode   :: FlattenMode
       , fe_loc    :: CtLoc
       , fe_nature :: CtNature
       , fe_eq_rel :: EqRel }   -- See Note [Flattener EqRels]
```

>
>
> This gets rid of the `CtEvidence` field, which was only used for G/W/D distinction and `CtLoc`, in favor of storing the individual pieces. (The new `CtNature` type is just an enumeration between G, W, and D.) There is also now an `EqRel` field, saying the equality relation that should be respected during flattening.
>
>

>
>
> The idea of flattening is to replace one type with another "equivalent" type (and to produce evidence of the equality). Previously, we've always used nominal equality. But, this same idea applies equally well to representational equality. So, the `EqRel` field says what equality should be respected during flattening. If `fe_eq_rel` is `ReprEq`, then the flattener will unwrap newtypes just as it reduces type families.
>
>


The aborted implementation of this idea is at [https://github.com/goldfirere/ghc/tree/two-flatteners](https://github.com/goldfirere/ghc/tree/two-flatteners)

## Open questions

- How do we avoid occurs-check problems? I believe that the substitution embodied by `inert_eqs` is *not* idempotent, and thus there must be a mechanism to prevent occurs-check problems. This mechanism will have to be extended to `inert_repr_eqs`. It's conceivable that zonking an individual tyvar (in a representational flattener call) will end up using equalities from both sets of equalities, possibly in an interleaved manner, so this may be delicate.

# Termination


It's a delicate thing, this termination. This section is about how we know that rewriting terminates.

**UPDATE:** With the decision by Simon and Dimitrios to eliminate D-rewrite-D, this section becomes moot. Adding roles does not complicate rewriting without D-rewrite-D.

## Current situation


There are three flavours of equalities: Givens (G), Deriveds (D), and Wanteds (W). There are restrictions on which flavour can rewrite which other flavour: G can rewrite anything, D can rewrite only D. (W cannot rewrite.)


An invariant on the inert set is that it truly is inert: no equality can rewrite any other, taking the flavours into account. Thus, the following is inert:

```wiki
  [G] c1 : a ~ [b]
  [D] c2 : b ~ Int
```


It looks like `c2` can rewrite `c1`, but the flavours prevent it.


So, assuming an inert set that is truly inert, how do we know that rewriting a work item will terminate? We need the following two conditions on the flavours:

1. Pick a flavour `g`. Now, consider the set of flavours `F` such that every flavour in `F` can rewrite the flavour `g`. Pick `f1` and `f2` from the set `F`. It must be the case that either `f1` can rewrite `f2` or `f2` can rewrite `f1`.

1. The rewrite relation among flavours is transitive.

**Theorem:** The two conditions on flavours above, along with the inertness of the inert set, imply that rewriting a work item terminates.


I have discovered a truly remarkable proof of this theorem which this margin is too small to contain. In any case, I believe the theorem.

## Situation with roles


Adding the ability to rewrite by representational equalities essentially adds a new dimension to the flavours, because nominal equalities can rewrite representational ones, but not vice versa. We now write flavours as, for example, G/N for a nominal Given or D/R for a representational Derived. So, adding this naively to the can-rewrite relation, we get the following:

- G/N can rewrite anything
- G/R can rewrite G/R, D/R and W/R
- D/N can rewrite D/N and D/R
- D/R can rewrite D/R
- W/N cannot rewrite
- W/R cannot rewrite


Sadly, condition (1), above, doesn't hold! Specifically, let `g` be D/R. Then, `F` contains G/N, G/R, and D/N. Pick `f1` = G/R and `f2` = D/N. Neither `f1` nor `f2` can rewrite the other. This means we risk non-termination. And, this isn't purely a theoretical concern. Here's a concrete example:

```wiki
  inert set:
    [G/R] c1 : a ~ [b]
    [D/N] c2 : b ~ [a]

  work item:
    [D/R] c3 : a ~ Int
```


Note that the inert set really is inert. Yet, rewriting `c3` will continue forever.


What to do?

### Solution 1: Split the inert set into nominal and representational components


The idea here is to forbid nominal equalities from rewriting representational ones, but also to duplicate all nominal equalities as representational equivalents. So, when canonicalizing `[D/N] c2 : b ~ [a]`, that equality would get added to the inert set, but we would also try to canonicalize `[D/R] c2 : b ~ [a]`, which would then be rewritten by `c1` to show an occurs-check problem.


This works quite nicely, except for one caveat: it makes GHC take up lots more memory when there are lots of nominal equalities around! This was discovered with test case `perf/compiler/T5837`, but I imagine the problem is not just there. We're *doubling* our work, in some sense. Argh.

### Solution 2: Forbid \[D/R\]


If we just eliminate the possibility of \[D/R\] equalities, then we're OK again. Deriveds are useful only for improvement: when we eventually discover that some unification variable equals some type and then we can fill the unification variable. Representational equalities can't be solved by unification, so it seems that \[D/R\] is a useless point in the space. But, representational equalities *can* give rise to nominal ones. Take `[D/R] c4 : G a ~ G Bool`, where `G` is a GADT and its parameter has a nominal role. Canonicalizing `c4` will then produce `[D/N] a ~ Bool`, which is a very useful Derived nominal. So, while this solution works, it seems to reduce the number of programs that typecheck.

### Solution 3: Like solution 1, but copy only \[D/N\] equalities


Solution 1 means that *no* nominal equality can rewrite a representational one. But, we don't need to prune the can-rewrite relation that much to restore termination. Instead, if we just remove one edge in the graph, we can get it: the \[D/N\]-to-\[D/R\] edge. If we remove just that one can-rewrite, then we have conditions (1) and (2) above, and thus termination.


However, we still want \[D/N\] to have an opportunity to rewrite \[D/R\] equalities. So, we copy the \[D/N\]s, like in Solution 1. When canonicalizing a \[D/N\], we'll spawn off an equivalent \[D/R\] equality as well. We then say that \[D/N\] cannot rewrite \[D/R\]. This has the unfortunate effect of copying some equalities, but \[D/N\] equalities should be *much* less common in practice than \[G/N\] equalities, making this copying not so painful.


As of now (Dec. 5, 2014, afternoon) Solution 3 is my plan going forward.
