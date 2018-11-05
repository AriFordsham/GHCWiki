
I've created this wiki page to track my learning/research as I try to improve my feature request that I made in the comments of [\#15009](https://gitlab.haskell.org//ghc/ghc/issues/15009).

## 20181104


(I think this day's entry is self-contained.)


My core hypothesis is that levels can help us determine if a wanted equality `w` can be floated out from under a set `gs` of given constraints without thereby losing access to some possible solutions of `w` (those involving at least one `g` in `gs`). Currently, GHC is quite conservative and does not float `w` if it's possible that one of the givens might be an equality. With more effort, it should be possible to determine that particular givens are not relevant to a particular wanted equality, and that's what I'm exploring. I'm specifically focused on skolems with a greater `TcLevel`, which arise from `RankNTypes`.


Notation: single character tyvars are skolems (i.e. bound by some `forall`), greek tyvars are metavars (i.e. unification variables or "taus"), `fsk0``fsk1` and so on are flattening skolems. I haven't even begun considerings flattening meta vars, but they would be `fmv0``fmv1` and so on. I write the `TcLevel` of a metavar as a suffix in brackets on at least one of its occurrences, e.g. `alpha[2]`. I'll only do that for skolems if I'm not showing their binding `forall`, which is where I prefer to put the level. (This all seems pretty standard for discussions on Trac at least.)

### Example 1 - the motivator


Is `g` relevant to `w`?

```wiki
forall[2] x. () =>
forall[3] (g : x ~ Int) =>
  (w :: alpha[1] ~ Int)
```


GHC 8.6 conservatively concludes "Yes", just because `g` is an equality. But `g` is indeed not relevant to `w`: allowing the use of `g` does not increase the set of possible solutions of `w`. The intuitive explanation is that `x` does not occur in `w`'s type and cannot be introduced into it when we learn new things. As an example of "learning a new thing", consider if some other constraint somewhere else in the scope of `alpha` causes the solver to assign `alpha := a[1]`. Because of that assignment, GHC would replace `alpha` by `a[1`} thereby introducing `a` as a new free variable of `w`'s type. The fundamental insight is that `x` cannot be introduced that way, because `x` is level 2 while `alpha` is level 1, so it'd be a skolem-escape bug if `alpha` were assigned to something with `x` free.

>
> Remark. The "elsewhere" constraints that assigned `alpha := a[1]` in that example aren't typically shown or even mentioned: we just assume that any metavar can be replaced at any time by any type whose free variables exclude the metavar and do not have a level greater than that of the metavar.


Thus, we could actually soundly float `w` all the way to level 1, and once it's there, GHC could solve `w` by unification, assigning `alpha := Int` and then discharging `w` by reflexivity. (GHC can't do that here at level 3, because a metavar is "untouchable" at any level but its own, and `alpha` is level 1. See `jfp-outsidein.pdf` for the motivation of that rule, which is the whole reason we need to be able to float some equalities in the first place!)

### Example 2


This example is just the previous one with the skolems' levels reduced by one.

```wiki
forall[1] x. () =>
forall[2] (g : x ~ Int) =>
  (w :: alpha[1] ~ Int)
```


This time `g` actually might be relevant to `w`, so we should not float. Though `x` is not in the type of `w` at the moment, it could be later, because now that `x`'s level is not greater than `alpha`'s, `alpha` might be assigned to a type involving `x`, and making that subtitution into `w` would result in a wanted constraint that could (clearly would, in this example) require `g` in order to be solved.

### Example 3 - metavars


The givens are not limited to skolems. What if we take Example 1 (which could safely float) and make the tyvar in `g` a metavar instead?

```wiki
forall[2] x. () =>
forall[3] (g : beta[2] ~ Int) =>
  (w :: alpha[1] ~ Int)
```


We should not float. For example, both metavars might reduce to the same level 1 skolem variable, in which case `w := g` might be the only possible solution.


The relation of the metavars' levels does not matter.

### Example 4 - flattening skolems


Type family applications have similar consequences as metavars.

```wiki
forall[2] x. () =>
forall[3] (g1 : F ~ fsk, g2 : fsk ~ Int) =>
  (w : alpha[1] ~ Int)
```


Again, we should not float past `g2`. For example, we might learn `alpha := F`.


What if the application has tyvars with greater levels?

```wiki
forall[2] x. () =>
forall[3] (g1 : G x ~ fsk, g2 : fsk ~ Int) =>
  (w : alpha[1] ~ Int)
```


It is tempting to conclude that we could safely float: `alpha[1]` can't become `G x` because `x` is level 2. But! An example in `jfp-outsidein.pdf` motivates the "open world assumption," which in this case means we should assume that we could "learn" a new top-level `G` axiom at any time (e.g. orphan instances). And if that axiom were `G _ := F`, we shouldn't float for exactly the same reasoning as above.


In general, the "flattening constraint" (i.e. `CFunEqCan`) `g1` should not itself prevent floating; the following would be safe to float.

```wiki
forall[2] x. () =>
forall[3] (g1 : G x ~ fsk) =>
  (w : alpha[1] ~ Int)
```

### Example 5 - "level inversion"


Some givens make me question my reliance on levels.

```wiki
forall[1] x. () =>
forall[2] y. (gx : x ~ Maybe y) =>
forall[3]. (gy : y ~ Int) =>
  (w : alpha[1] ~ Maybe Int
```

`gx` is an interesting given equality because as a rewrite rule it replaces a level 1 variable with a type with free variable of level 2! Our reasoning in the above examples has relied on the fact that a metavar of level 1 could not become a type with a free variable whose level was greater than 1. But here, if `alpha := x`, then next `gx` rewrites that `x` to `Maybe y`, and so transitively `alpha` has become `Maybe y`. Disaster?


I think I have a simple answer to a tricky question: Is it safe to float `w` out from under `gy`? If we were to ignore `gx`, the answer would be simple: yes, because `alpha` is level 1 and `y` is level 2. But with `gx` and its "level inversion" of `x`, we instead should not float: `gy` might be relevant to `w`, regardless of levels because `gx` confuses them.


The simplifying observation is that floating `w` is ultimately only helpful if we can float it all that way to `alpha`'s level, level 1. And to do so, we'd have to float `w` past `gx` itself. That'd clearly be unsound by our usual reasoning: `x` is the same level as `alpha`.


But what if `w` isn't trying to float past `gx`, only past `gy`? That would mean either `alpha` has a higher-level or `gx` has a lesser level than in the example we just considered. In general, we have the following.

```wiki
forall[A] x. () =>
...
forall[B] (gx : x ~ Maybe y) =>
...
forall[C]. (gy : y ~ Int) =>
  (w : alpha[L] ~ Maybe Int
```


If L \< B then `gx` itself will stop `w`. If B \<= L, then `y` is of a level \<= B (else it's out of scope), hence \<= L, and so `gy` will stop `w` regardless of `gx`.


The lesson is that we need not work out how a level inverting constraint like `gx` affects other givens in its scope, we can judge all givens individually. We might judge some incorrectly (`gy`), but only if those farther out (`gx`) will render that judgement irrelevant.  Thus we should not float a constraint at all unless we can float it all the way to its final destination. (Maybe we should just perform the unification instead of floating it? Otherwise, what if we learn something "risky" after having floated only half way to our destination? This might just be a matter of error message quality.)


The way `gx` and `gy` interact here corresponds to the `EQDIFF` given-given interaction rule in `jfp-outsidein.pdf`. GHC doesn't implement that rule as written; it does not add a third constraint `gx_gy : x ~ Maybe Int` alongside `gy`. If it did, then `gx_gy` would be enough to prevent floating `w`.

### Example 6 - ignore CTyEqCan RHS unless it's a fsk


We assume the variable on the LHS is a skolem, otherwise previous examples apply.


Does the RHS of the given equality matter?

```wiki
forall[M] x. ... =>
...
forall ... . (g : x ~ <RHS>) =>
  (w : alpha[L] ~ Int)
```


GHC uses CTyEqCan constraints as left-to-right rewrite rules, so the RHS `<RHS>` does not affect `w` unless `x` first becomes free in `w`'s type. If M \< L, that can't happen and we float `w` -- that's the basic reasoning from Example 1. Note that `<RHS>` wasn't a concern. If M \>= L, then `g` prevents floating `w` as-is, so `<RHS>` again didn't matter.


That line of reasoning overlooks two important possibilities.

1. The `EQSAME` given-given interaction from `jfp-outsidein.pdf`: two CTyEqCans with the same LHS will induce a new equality between their RHSs.

1. If `<RHS>` is a flattening skolem (i.e. a type family application), then it might reduce in such a way that causes the CTyEqCan to flip over, giving it a new LHS.

TODO Demonstrate that flipping is the only possibility (i.e. `x` is atomic and immutable, so it will remain one side of the equality).

TODO I'm currently under the impression that fmvs cannot occur in a given CTyEqCan, so I'm not considering them.


If it weren't for the mutability issue, we could address the `EQSAME` issue by withholding our judgement until all the givens are inert. Unlike `EQDIFF`, GHC implements `EQSAME` as written, by creating a new constraint. We'll wait until the givens are inert so we can simply judge any induced equalities instead of trying to anticipate the `EQSAME` interactions and their possible cascades.


However, an inert set of CTyEqCans might not be inert after one of their LHSs changes, so the mutability issue subverts our planned reliance on inertness.

>
> 💫 😵 💫


I'm getting dizzy. Let's zoom back out. Our basic rule is: float `w` past `g` if `M < L` and do not otherwise. So if we float, and then the LHS of `g` somehow becomes a tyvar with a level \>= L, we might have thus lost a possible solution to `w`.


It seems too complicated to try to determine if `EQSAME` interactions can or cannot lead to that happening, so I'm going to opt for a conservative heuristic and refuse to judge a float if there's a risk of losing inertness. Specifically, I will withhold judgment until we're certain that no new `EQSAME` interactions can arise.


So I'm going to focus on the case where the RHS is a flattening skolem, because that's only way that the LHS might change, thereby invalidating a premature float judgment and/or enabling further `EQSAME` interactions.

TODO Demonstrate that no other RHS threatens the LHS. DRAFT "A metavar of level N can only become a tyvar of level \<= N, which wouldn't change the orientation of `g`. (Even a level inverting given like `gx` from the above example would only allow a metavar to become a type headed by a constructor -- the easy first case of this example.) So we can ignore the RHS if it's a metavar."


So how does `g : x[M] ~ fsk0` with `x` an inert skolem become `g' : tv[N] ~ x`? In general, a type family application `F <t1> <t2> ...` can reduce to a tyvar `tv` only if that tyvar is a free variable of one of its arguments. If `fsk0` reduces to a flattening skolem `fsk1`, we'll end up back in this same analysis case, and so recur. If it reduces to a metavar or skolem, then GHC will flip the equality only if N \>= M (skolems of the same level are resolved by their syntactic names; seems fragile to rely on that, so let's not.) -- see `TcUnify.Note [TyVar/TyVar orientation]`.


Thus, the RHS should prevent floating only if it is a flattening skolem whose definition's free non-flattening variables (recurring on free flattening skolems) include a variable with a level N \>= M. If such a free var exists, then the equality might flip, which might enable more `EQSAME` interactions, which we do not attempt to anticipate, choosing the simpler pessimism. Even if `EQSAME` that didn't happen, the level of the LHS has increased, and so a former decision float might now be revealed as premature; if we some how knew `EQSAME` interactions were not a risk, we could strengthen the predicate to `N >= L`: let it flip as long as it still wouldn't prevent `w` from floating.

### Rule for Consideration


After working through the above examples, I'm considering the following rule. The basic shape is informed by Example 5.

>
> Rule 20181104. Float a (canonical) wanted `w : alpha[L] ~ <t>` from level K \> L to level L if all the constraints given by the levels from L+1 to K (inclusive) are inert and each satifies P\[L\], where
>
> ```wiki
> nonflat_fvs t = the free variables of the type t, recurring on fsks instead of including them
>
> -- P[L] g iff g is not relevant to any wanted CTyEqCan if its LHS is a metavar whose level is <= L
> P[-] : TcLevel -> GivenConstraint -> Bool
> P[L] CDictCan g = forall sc in superclass-constraints(g). P[L] sc
> P[L] CFunEqCan _ ~ fsk = True
> P[L] CTyEqCan alpha ~ _ = False   -- see Example 3
> P[L] CTyEqCan fsk ~ _ = False   -- see Example 4
> P[L] CTyEqCan x[M] ~ fsk = L < M && forall v[N] in nonflat_fvs(fsk). N < M   -- see Example 6
> P[L] CTyEqCan x[M] ~ _ = L < M
> P[L] g = forall v[M] in fvs(g). isSkolem(v) && L < M   -- assume any fv could become the LHS
> ```
>
>
> and `nonflat_fvs(<t>)` doesn't have any skolems with level \> L.

TODO Any other restrictions on `<t>`?

## 20181014


Just using small examples today, no reference to earlier days -- kind of a fresh start.

### Example 1

```wiki
forall[2] x. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ Int)
```

`Note [Let-bound skolems]` applies here: it is safe to float `w` past `gx`, regardless of skolems and givens outside the scope of `x`.


Note that a more general `w : <W>` can only float after being simplified by `gx`, otherwise the skolem `x` would escape!

### Example 2

```wiki
forall[2] x. () =>
forall[3]. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ Int)
```


In this case, I think it is also safe to float `w` past `gx`, even though `Note [Let-bound skolems]` doesn't apply. This kind of example motivates my investigation here.

### Example 3

```wiki
forall[2] x. () => (
    (u : <U>)
  ,
    forall[3]. (gx : x ~ Int) =>
      (w : alpha[tau:2] ~ Int)
  )
```


In this case, it \*NOT\* safe to float `w` past `gx`. Simplification of `u` might assign `alpha := x`, in which case `gx` may be required in order to solve `w`. If that assignment happened after the float, we'd be stuck.

### Example 4

```wiki
forall[2] x. (gx : x ~ Int) => (
    (u : <U>)
  ,
    (w : alpha[tau:2] ~ Int)
  )
```

`Note [Let-bound skolems]` still applies, and it's safe to float `w`! (GHC 8.6 does.) This is because \*all\* occurrences of `x`, even those in `<U>`, will have been eliminated by `gx`, and so no problematic assignment can take place after the float. `x` is a dead-binder once `gx` has been applied within the scope that it shares with `x`.


I will no longer explicitly write the "elsewhere" constraint `u`; I will instead simply assume that any unification variable `alpha[tau:i]` where `i > 0` can rewrite to any type whose free variables' levels are all `<= i`. (Level `0` is reserved for flattening variables, and I anticipate I'll have to treat those somewhat differently.)


Note that Examples 3-4 with `u` left implicit are the same as Examples 1-2 with `alpha`'s level increased from 1 to 2.

### Example 5

```wiki
forall[1] x. () =>
forall[2] y. (gx : x ~ T y) =>
forall[3]. (gy : y ~ Int) =>
  (w : alpha[tau:1] ~ T Int)
```

`w` should not float past `gy`. If we decide elsewhere that `alpha := x`, then `w` can only be solved via a combination of `gx` and `gy`.


The line of reasoning is:

1. The type of `w` has `alpha` free, by observation.
1. `alpha` can rewrite to something with `x` free, by our heuristic regarding unification variable levels.
1. `x` can rewrite to something with `y` free, by `gx`.
1. The type of `w` can rewrite to something with `y` free, by transitivity.
1. Thus, floating `w` out from under `gy` could ultimately cause an avoidable type error.


Note that contrary to the given-given interaction rules in `jpf-outsidein.pdf`, GHC does not interact `gx` and `gy` to create a new given `x ~ T Int` (presumably at level 3). So we have to explicitly guard against this transitive scenario. (For example, inspect the `-ddump-tc-trace` for `f :: (x :~: [y]) -> (y :~: Int) -> x -> c; f Refl Refl = id`; it seems like the transitivity is actually "handled" during flattening.)


I found intermediate conclusion within that line of reasoning that `alpha[tau:1]` can rewrite to something with `y[sk:2]` free to be counter-intuitive because that rewriting _increases_ the maximum level of the source term's free type variables. This increase originates in the otherwise completely typical `gx : x[sk:1] ~ T y[sk:2]` CTyEqCan.

### Refinement of Example 5


I see a much simpler argument against floating here.

1. `w` would have to float past both `gy` \*and also\* `gx` for its `alpha` to become touchable.
1. `w` manifestly cannot pass `gx`.
1. Thus, do not float `w` at all.


If we consider an example with `alpha`'s level increased to `2` so that it would not need to pass `gx`, then it manifestly cannot pass `gy`.


So maybe an inherited attributed along the lines of "the lowest level tyvar with a CTyEqCan in the outer givens between level 1 and here is 1" would be enough to know that `w` cannot float all the way to its target level (`1`, the level of its LHS `alpha`) and so should not float at all.

### Example 6


This case is simpler than it looks at first.

```wiki
forall[1] x. () =>
forall[2] y. () =>
forall[3]. (gy1 : y ~ T x) =>
forall[4]. (gy2 : y ~ T Int) =>
  (w : alpha[tau:1] ~ T Int)
```


The subtlety is that those givens are actually not inert; GHC will interact `gy1` and `gy2` to create a constraint at level 4 `x ~ T Int`, according to the `EQSAME` rule from `jfp-outsidein.pdf` (somewhat in contrast to Example 5, where `EQDIFF` is apparently not directly implemented in GHC).

```wiki
forall[1] x. () =>
forall[2] y. () =>
forall[3]. (gy1 : y ~ T x) =>
forall[4]. (gx : x ~ Int) =>   -- evidence binding gx from gy1 and gy2 interacting
  (w : alpha[tau:1] ~ T Int)
```


Now it's clear `w` should not float, because of the new `gx` --- it's quite similar to Example 5 now.

### Example 7


Similar to the previous example, this set is not yet inert.

```wiki
forall[1] x. () =>
forall[2]. (gx : x ~ Int) =>
  (w : alpha[tau:1] ~ x)
```

`gx` can simplify `w`, which yields Example 3, essentially.

### Example 8


Unification variables can occur in givens.

```wiki
forall[1]. () =>
forall[2]. (gbeta : beta[tau:1] ~ Int) =>   -- beta must be < level 2, according to (GivenInv) of TcType.Note [TcLevel and untouchable type variables]
  (w : alpha[tau:1] ~ Int)
```

`w` should not float past `gbeta`, since we might elsewhere decide that `beta := alpha` or `alpha := beta`.


Even if `alpha` were of a greater level

```wiki
forall[2]. () =>
forall[3]. (gbeta : beta[tau:1] ~ Int) =>
  (w : alpha[tau:2] ~ Int)
```


the assignment {{{alpha := beta}} is still possible.

### Example 9


This is my actual original motivation.

```wiki
forall[2] x y. () =>
forall[3]. ( (g1 : x ~ alpha[tau:1]) , (g2 : y ~ beta[tau:1]) ) => (
    (w1 : alpha[tau:1] ~ Int)
  ,
    (w2 : beta[tau:1] ~ Int)
  )
```


I think they should both float.


The remaining examples will explore variations of this. I'll leave out `g2` and `w2` when possible.

```wiki
forall[2] x. () =>
forall[3]. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ Int)
```

### Example 10

`TcUnify.Note [TyVar/TyVar orientation]` specifies that `x[sk:i] ~ alpha[sk:i]` should be swapped to `alpha[sk:i] ~ x[sk:i]`.

```wiki
forall[2] x. () =>
forall[3]. (g : alpha[tau:2] ~ x) =>
  (w : x ~ Int)
```


GHC won't consider floating that, because `w` is not of the form `alpha[tau:_] ~ <T>`.


I definitely don't have enough context to seriously challenge the current swap decision procedure. But, if we didn't reorient, then I don't see why `w` shouldn't float...

```wiki
forall[2] x. () =>
forall[3]. (g : x ~ alpha[tau:2]) =>   -- hypothetically ignoring current swap decision procedure
  (w : alpha[tau:2] ~ Int)
```

### Example 11


Of course, occurrences of skolems on the righthand-side of `w` should prevent floating past their binder.

```wiki
forall[2] x. () =>
forall[3] y. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ Maybe y)                 -- Decide no float, since 1 < maximum (map level (fvs (Maybe y)))
```

### Example 12


I don't understand irreducible constraints terribly well, so I'll just treat them all conservatively for now by refining the inherited attribute.

```wiki
forall[2] x. () =>
forall[3]. beta[tau:1] =>
forall[4] y. (g : x ~ alpha[tau:1]) =>
  (w : alpha[tau:1] ~ y)
```


The inherited attribute is a vector, one for each parent implication, in which each vector component is the level of the lowest level tyvar on the LHS of a CTyEqCan or that occurs free in an irreducible constraint.


Maybe insoluble constraints just set the vector component to `0`, thereby preventing floating past them?

```wiki
forall[2] x. () =>                       -- vec [Inf,Inf,Inf]   (i.e. levels 2,1,0)
forall[3]. beta[tau:1] =>                -- vec [1,1,1,1]
forall[4] y. (g : x ~ alpha[tau:1]) =>   -- vec [2,1,1,1,1]     (i.e. levels 4,3,2,1,0)
  (w : alpha[tau:1] ~ y)                 -- Decide no float, since 1 >= vec !! (4-1).
```

### Conclusion

TODO ... I haven't yet thought of anything clever that isn't caught by the inherited attribute test I proposed in the discussion of Example 5, even with type family applications and/or constraint kinds.

## 20181013


This section distills the narrative of the 20181008 section into narrower observations and questions.


My current goal is a "simple enough", algorithm that could conservatively decide whether it is safe to float `w : alpha[tau:k] ~ <R>` out from under `g : x[sk:i] ~ <X>` where `k < i` and `alpha` might occur free in `<X>`. Informally, it's only safe to do so if the eventual right-hand side of `w` will definitely not (transitively) depend on `g`.


A wanted `w : <W>` can gain `x[_:i]` as a free variable in two ways.

- WAY1 A uvar `alpha[tau:j]` that occurs in `<W>` could be assigned a type `alpha := <T>` where `x` occurs free in `<T>`, which would require `j >= i`.
- WAY2 `w` could be simplified by a given CTyEqCan `g : y ~ <T>` where `x` occurs free in `<T>`.


WAY1 cannot increase the maximum level of the free variables in `<W>`. In contrast, WAY2 can: consider `g : y[_:2] ~ Maybe x[sk:3]`.


Within one subtree of a binding group's entire forest of implication trees, we must assume that a uvar `alpha[tau:j]` could be assigned a type `<T>` in which any variable of level `<= j` other than `alpha` itself could occur free.


The reduction of a type family application `F <X>` cannot introduce new free type variables beyond those already free in `<X>` -- except for variables that arise from flattening the type family applications possibly introduced by the reduction. But it might remove some free variables.


An assignment to a uvar `alpha[tau:j]` that occurs in the givens could directly or indirectly (via enabled given-given interactions) yield the given `g` of WAY2.

## 20181008


I've found a useful perspective on this. Consider this implication tree.

```wiki
forall[3] y. () =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


The naive intuition is

>
> We can float `w1` out from under `g` if `y` doesn't occur (free) in `<W1>`.


In fact, according to the jfp-outsidein.pdf "Simplification Rules", `g` will eventually simplify `w1` into some `w2 : <W2>` such that `y` does not occur in `<W2>`.


The subtlety, however, is that `y` might occur in `<U>` and therefore the solving of `u` may assign a u(nification)var(iable) `alpha[tau:i]` where `i >= 3` to some type in which `y` occurs; if `alpha` occurs in `<W2>`, then now `<W2>` suddenly has an occurrence of `y` again!


This example demonstrates:

```wiki
forall[3] y. () =>
  ( (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   (w1 : alpha[3] ~ y)
  )

-- use g to simplify w1 -->

forall[3] y. () =>
  ( (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   (w2 : alpha[3] ~ Int)
  )

-- float w2, since y does not occur in its type -->

forall[3] y. () =>
  ( (w3 : alpha[3] ~ Int)
  , (u : alpha[3] ~ y)
  , forall[4]. (g : y ~ Int)   =>   ()
  )

-- solve u, by alpha[3] := y -->

forall[3] y. () =>
  ( (w3 : y ~ Int)
  , forall[4]. (g : y ~ Int)   =>   ()
  )

-- Stuck because we floated!
```

### Comparison to status quo


Compare this to the case where `Note [Let-bound skolems]` applies.

```wiki
forall[3] y. (g : y ~ <Y>)   =>   (w1 : <W1>)
```


There is no `u : <U>` that might assign to a uvar `alpha` shared with `<W1>` here, so the failure mode describe above can't happen. If we add a `u` alongside `w1`,

```wiki
forall[3] y. (g : y ~ <Y>)   =>   ( (w1 : <W1>) , (u : <U>) )
```


then now it's possible solving `u` could assign `alpha[tau:3] := ...y...`. However, as part of floating `w1`, we immediately promote any uvars that occur in it, so the problematic `alpha[tau:3]` would be assigned `alpha[tau:3] := alpha[tau:2]`, which cannot be assigned `alpha[tau:2] := ...y...` since `y` is `[sk:3]` (and also `alpha[tau:2]` is untouchable in `u[3]`).


This silly example demonstrates:

```wiki
forall[3] y. (g : y ~ Int)   =>   (w1 : alpha[3] ~ y,u : alpha[3] ~ y)

-- use g to simplify w1 -->

forall[3] y. (g : y ~ Int)   =>   (w2 : alpha[3] ~ Int,u : alpha[3] ~ y)

-- float w2, since y does not occur in its type (NB the promotion of alpha 3 to 2) -->

(w3 : alpha[2] ~ Int)
forall[3] y. (g : y ~ Int)   =>   (u : alpha[2] ~ y)

-- Cannot solve u as in previous example, because alpha is now untouchable in u.
-- solve w3, by alpha[2] := Int -->

forall[3] y. (g : y ~ Int)   =>   (u : Int ~ y)

-- re-orient, reflexivity -->

-- Solved!
```

### Refinement for Assignment


So we can refine the rule for

```wiki
forall[3] y. () =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


from

>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>`.


to

>
> We can float `w1` out from under `g` if `y` does not **and can never again** occur in `<W1>`.


How do we decide that much stronger predicate? There are a few options (e.g. there is no sibling `u` wanted), but I currently favor checking that no uvar in `<W1>` has a level `>= 3` (this check should inspect the RHS of any flattening vars that occur in `<W1>`). That prohibits the problematic reintroduction of `y` via uvar assignment after floating.


Thus:

>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>`**and all unification variables in `<W1>` have level `< 3`**.


After I thought about this for a bit, I realized there might be another way for `y` to be reintroduced into `<W1>` in the more general case where that outer implication has givens.

```wiki
forall[3] y. (o : <O>) =>
  forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
```


If it's something like `o : x[sk:2] ~ Maybe y`, and `x` occurs in `<W1>`, then simplifying via `g` and floating `w1` and then simplifying it via `o` might reintroduce `y`.

### Refinement for Outer Givens (a wrong turn)


So we can generalize the rule for

```wiki
forall[3] y. (o : <O>) =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```


by refining from

>
> We can float `w1` out from under `g` if `y` does not occur in `<W1>` and all unification variables in `<W1>` have level `< 3`.


to

>
> We can float `w1` out from under `g` if **all givens are inert** and `y` does not occur in `<W1>` and all unification variables in `<W1>` have level `< 3`.


If I understand the jfp-outsidein.pdf "Interaction Rules", `o : x[sk:2] ~ Maybe y` and `g` ought to interact to yield something like

```wiki
forall[3] y. (o : x[sk:2] ~ Maybe y) =>
  ( (u : <U>)
  , forall[4]. ( (g : y ~ <Y>) , (g2 : x[sk:2] ~ Maybe <Y>) )  =>   (w1 : <W1>)
  )
```


Now that givens are inert, our rule will force `g` and `g2` to eliminate the skolems and assess uvars (level `< 2` now) in `w1` before floating it.

NOPE, the above is not how `g` and `o` "interact". I searched `TcInteract` for something like `EQDIFF`, but I ended up at 

[ line 1630](https://github.com/ghc/ghc/blob/8bed140099f8ab78e3e728fd2e50dd73d7210e84/compiler/typecheck/TcInteract.hs#L1630)

```wiki
  | isGiven ev         -- See Note [Touchables and givens]
  = continueWith workItem
```


That Note hasn't existed since

```wiki
commit 27310213397bb89555bb03585e057ba1b017e895
Author: simonpj@microsoft.com <unknown>
Date:   Wed Jan 12 14:56:04 2011 +0000

    Major refactoring of the type inference engine
```


I don't see what touchables has to do with line 1630. /shrug


So I whipped up a test

```wiki
f :: (x :~: [y]) -> (y :~: Int) -> x -> c
f Refl Refl = id
```


which never gets further than

```wiki
  Implic {
    TcLevel = 2
    Skolems =
    No-eqs = False
    Status = Unsolved
    Given = co_a1bQ :: [y_a1bM[sk:1]] GHC.Prim.~# x_a1bL[sk:1]
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 3
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given = co_a1bR :: Int GHC.Prim.~# y_a1bM[sk:1]
              Wanted =
                WC {wc_simple =
                      [WD] hole{co_a1c2} {2}:: c_a1bN[sk:1]
                                               GHC.Prim.~# [Int] (CNonCanonical)}
              Binds = EvBindsVar<a1bV>
              a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
              in an equation for `f_a1bO' }}
    Binds = EvBindsVar<a1bW>
    a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
    in an equation for `f_a1bO' }
```


and even explicitly says

```wiki
  Inerts: {Equalities: [G] co_a1bZ {1}:: x_a1bL[sk:1]
                                         GHC.Prim.~# [y_a1bM[sk:1]] (CTyEqCan)
                       [G] co_a1c0 {1}:: y_a1bM[sk:1] GHC.Prim.~# Int (CTyEqCan)
           Unsolved goals = 0}
```


Note that `co_a1bZ` and `co_a1c0` never "interacted" as `EQDIFF` suggests they should. However, they both rewrote the wanted. (In fact, it's unflattening that seems to do this, even though there are no type families involved! See [ the definition of](https://github.com/ghc/ghc/blob/8bed140099f8ab78e3e728fd2e50dd73d7210e84/compiler/typecheck/TcFlatten.hs#L1846)`flatten_tyvar2`.)

### Comparison to status quo

`Note [Let-bound skolems]` need not worry about outer givens because `y` is not in scope outside of this implication.

### Refinement for Outer Givens with Interactions (a turn too far)


So we'll need a different refinement to ensure that `y` cannot be reintroduced by an outer given.


In our `o : x[sk:2] ~ Maybe y` example, `x` has a significant relationship with `y`: any occurrence of `x` in the scope of `o` will be rewritten to have an occurrence of `y` instead. Loosely, "`x` can become `y`".


Similarly, any uvar `alpha[tau:i]` where i `>= 3` "can become `y`" due to solving other parts of the implication tree. So the "can become" relation is a nice commonality between the two failure modes I've considered.


How might we determine if "`x` can become `y`" in general? Here's another failure, also based on the "`x` can become `y`" idea, but with more moving parts.

```wiki
forall[3] y. ( (o1 : z[sk:2] ~ T x[sk:2] ) , (o2 : alpha[tau:2] ~ T (Maybe y)) ) =>
  ( (u : <U>)
  , forall[4]. ( (g : y ~ <Y>) , (g2 : x[sk:2] ~ Maybe <Y>) )  =>   (w1 : <W1>)
  )
```


Note that the equalities `o1` and `o2` contain `x` and `y` respectively, but do not have any shared variables. However, solving another part of the implication tree might assign `alpha := z`, in which case `o1` and `o2` will interact, yielding a constraint that then canonicalizes to the familiar `o : x ~ Maybe y`. This is actually the same lesson as in the first failure mode "`alpha[tau:i]` can become any variable at level `<= i`", but in the givens this time and one step removed from affecting our float.


Our second refinement makes

>
> We can float `w1` out from under `g` if `y` does not and can never again occur in `<W1>`.


more formal as

>
> We can float `w1` out from under `g` if the transitive closure of the "can step to" relation does not relate any free variable of `<W1>` to `y`.


which we can shorten to

>
> We can float `w1` out from under `g` if no free variable of `<W1>` "can become" `y`.


by defining the following (over-estimating) properties.

- CB1 "can become" includes the transitive closure of "can step to"
- CS1 `x` (any flavor and level) can step to itself
- CS2 `alpha[tau:i]` can step to any variable at level `<= i`
- CS3 `x` (any flavor and level) can step to any free variable of `<X>`, given CTyEqCan `x ~ <X>`
- CS4 Any free variable of `<A>` can step to any free variable of `<B>` or vice versa, given a non-CTyEqCan equality `<A> ~ <B>`


That's enough to catch the `o : x[sk:2] ~ Maybe y` failure. But it's not enough for the `o1` and `o2` failure that involved the interaction after assignment. So we must further extend "can become".

- CB2 Any free variable of `<A>` can become any free variable of `D` or vice versa if a free variable of `<B>` can become a free variable of `<C>` or vice versa, given `<A> ~ <B>` and `<C> ~ <D>` (or either/both of their symmetries)


(Could there be less symmetric special cases of `CB2` for CTyEqCans-like things?)


Let "can become" be the smallest relation satisfying all the CB\* properties.


... That seems simultaneously too complicated and also too conservative. So maybe we should start with relatively simple and therefore acceptably too conservative.

### Another Refinement for Outer Givens with Interactions (a turn too far in the other direction?)


This seems reasonably balanced for starters.

```wiki
forall[3] y. theta =>
  ( (u : <U>)
  , forall[4]. (g : y ~ <Y>)   =>   (w1 : <W1>)
  )
```

>
> We can float `w1` out from under `g` if no free variable of `<W1>` "can become" `y`.

- CB1 "can become" includes the transitive closure of "can step to"
- CS1 `x` (any flavor and level) can step to itself
- CS2 `alpha[tau:i]` can step to any variable at level `<= i`
- CS3 `x` (any flavor and level) can step to any free variable of `<X>`, given CTyEqCan `x ~ <X>`
- CS4 Any free variable of `<A>` can step to any free variable of `<B>` or vice versa, given a non-CTyEqCan equality `<A> ~ <B>`
- CS5 Any free variable of the possible equalities in `theta` can step to any other and to any variable of level `<= i`, if there is a uvar of level `i` in `theta`


Let "can become" be the smallest relation satisfying CB1.

### Yet Another Refinement for Outer Givens with Interactions (are we going in circles?)

TODO I'm currently thinking about about what to do if `g` has siblings. I suspect CS5 should consider those too. Should it also consider `g`?


My current goal is for `w : alpha[tau:1] ~ Int` to float out from under `g : x[sk:2] ~ alpha[tau:1]`. Thus CS5 is likely disappointingly conservative if it includes siblings:

```wiki
forall[3] x y. () =>
  ( (u : <U>)
  , forall[4]. ( (g1 : x ~ alpha[tau:1]) , (g1 : y ~ beta[tau:1]) )   =>   ( (w1 : alpha[tau:1] ~ Int) , (w2 : beta[tau:1] ~ Int) )
  )
```


Maybe CS5 should ignore uvars on the RHS of a skolem's CTyEqCan?

## Experiments

### How do given-given interactions manifest?

```wiki
f :: (x :~: [y]) -> (x :~: z) -> x -> c
f Refl Refl = id
```


gives a trace that includes

```wiki
addTcEvBind
  a1c0
  [G] co_a1c5 = CO: co_a1bW ; co_a1c4
```


and

```wiki
getNoGivenEqs
  May have given equalities
  Skols: []
  Inerts: {Equalities: [G] co_a1c4 {1}:: x_a1bP[sk:1]
                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CTyEqCan)
                       [G] co_a1c5 {1}:: z_a1bR[sk:1]
                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CTyEqCan)
           Unsolved goals = 0}
```


(The number is braces is the SubGoalDepth -- see TcRnTypes.Note \[SubGoalDepth\].)


and

```wiki
reportImplic
  Implic {
    TcLevel = 1
    Skolems = x_a1bP[sk:1] y_a1bQ[sk:1] z_a1bR[sk:1] c_a1bS[sk:1]
    No-eqs = True
    Status = Unsolved
    Given =
    Wanted =
      WC {wc_impl =
            Implic {
              TcLevel = 2
              Skolems =
              No-eqs = False
              Status = Unsolved
              Given = co_a1bV :: [y_a1bQ[sk:1]] GHC.Prim.~# x_a1bP[sk:1]
              Wanted =
                WC {wc_impl =
                      Implic {
                        TcLevel = 3
                        Skolems =
                        No-eqs = False
                        Status = Unsolved
                        Given = co_a1bW :: z_a1bR[sk:1] GHC.Prim.~# x_a1bP[sk:1]
                        Wanted =
                          WC {wc_simple =
                                [WD] hole{co_a1c7} {2}:: c_a1bS[sk:1]
                                                         GHC.Prim.~# [y_a1bQ[sk:1]] (CNonCanonical)}
                        Binds = EvBindsVar<a1c0>
                        a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
                        in an equation for `f_a1bT' }}
              Binds = EvBindsVar<a1c1>
              a pattern with constructor: Refl :: forall k (a :: k). a :~: a,
              in an equation for `f_a1bT' }}
    Binds = EvBindsVar<a1c2>
    the type signature for:
      f :: forall x y z c. (x :~: [y]) -> (x :~: z) -> x -> c }
```