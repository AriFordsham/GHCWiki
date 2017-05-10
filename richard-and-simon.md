# Summary of tasks to be completed


... as discussed by Richard and Simon. This page is mostly for our own notes, but others are welcome to read it.

- Change flattener to be homogeneous ([\#12919](https://gitlab.haskell.org//ghc/ghc/issues/12919), [\#13643](https://gitlab.haskell.org//ghc/ghc/issues/13643))
- Implement [\#13650](https://gitlab.haskell.org//ghc/ghc/issues/13650) (subsumes `mkCastTy` stuff below)
- Implement homogeneous as per Stephanie's paper (but what about Refl2?  Does Refl2 mean that our new equality will need to be indexed by a coercion??)
- Fix [\#11715](https://gitlab.haskell.org//ghc/ghc/issues/11715) according to Richard's plan
- Remove `solveSomeEqualities`
- Generalized injectivity [\#10832](https://gitlab.haskell.org//ghc/ghc/issues/10832), vis-a-vis Constrained Type Families paper
- Taking better advantage of levity polymorphism:

  - Could `[]` be a data family?
  - Unlifted newtypes
  - Unlifted datatypes
  - generalized classes in base
  - ...
- [\#11739](https://gitlab.haskell.org//ghc/ghc/issues/11739) (simplify axioms)
- Fix all the `TypeInType` bugs
- Clean up pure unifier to make the fact that kind coercions *only* affect type variables by using, e.g., `getCastedTyVar_maybe`.
- Is it possible to remove all the pushing in `mkCastTy`? I think so. The pushing doesn't help `splitTyConApp` at all, because pushing a coercion into a `TyConApp` doesn't make it become another `TyConApp`. The pushing *does* help `splitAppTy`, but perhaps all usages of `splitAppTy` already account for casts. *Dangling problem:* what on earth to do about `LRCo`, given that it's not always possible to push? It seems that it may be impossible to make `LRCo` work in a way that respects `eqType` equality, with `eqType`'s ability to move casts about willy-nilly.
- It seems that `quantifyTyVars` is duplicating some logic from `simplifyInfer`, in that it removes covars. This should really be done in `kindGeneralize`, because `simplifyInfer`*uses*`quantifyTyVars`. This should be just about possible, but with some twists and turns:

  - H98 constructors are strange in that they have tyvars that aren't mentioned in the type. So be careful here and make sure the type is closed (w.r.t. user-written tyvars) before calling `kindGeneralize`.
  - `tcFamTyPats` needs a hard look
  - So does `tcRule`.
  - Could also separate out `kindGeneralizeKind` and `kindGeneralizeType`. The latter works only over closed types.
  - If we remove the "remove covars" call from `quantifyTyVars`, we should really put it in `decideQuantifiedTyVars`. Perhaps we *don't* need to remove covars in `kindGeneralize` because `solveEqualities` will fail if any covars are around. It is **OK** to remove a covar without removing its kind, because the covar will be solved in the residual implication constraint from `simplifyInfer`.
  - Example of why we need to exclude coercions during generalization:

```wiki
data X where
  MkX :: Proxy a -> Proxy b -> (Refl :: a :~: b) -> X
```

- Take a look at `tidyToIfaceType`: I don't think it needs to tidy the env.
- Simplify `mkCastTy`, keeping the `isReflexiveCo` check. Document the reflexivity invariant. (Do we need to differentiate between `tcView`-reflexivity and `coreView`-reflexivity?) ([\#13650](https://gitlab.haskell.org//ghc/ghc/issues/13650))

  - Bah. This is wrong. I had thought that the reflexivity invariant would mean that `splitTyConApp` could never be thrown off the scent by a coercion. But this is wrong. Consider `T a b c` and `(T a b |> (co -> <Type>)) (c |> sym co)`. These types have the same kind (`Type`) and are `eqType`. (That is, they are the same if we ignore coercions.) Yet the first is a `TyConApp` and the second is an `AppTy`. No reflexive coercions here! The current elaborate `mkCastTy` doesn't even handle this situation. But it makes me realize that the reflexivity invariant isn't enough. We need to teach `splitTyConApp` how to deal with this scenario. Ugh.
  - Some invariants to make sure of: No nested `CastTy`s. No `AppTy (TyConApp ... |> co) ty`. No reflexive coercions.
  - Implement KPush in `splitTyConApp`.
- Remove `quantifyTyVars` call from `simplifyInfer`. Instead call `skolemiseUnboundMetaTyVars` from `simplifyInfer` directly.
- Stable topological sort may not be well specified. But we can always write a deterministic algorithm. Perhaps that should be in the manual.
- Can remove `closeOverKinds` in most places. Otherwise, just gather the kinds of user-written tyvars (e.g. fundep RHS)
- Re-do the fix for [\#13233](https://gitlab.haskell.org//ghc/ghc/issues/13233). There are two separate problems:

  1. How to ascertain whether or not a primop is saturated during desugaring (or, possibly, earlier). On a call, we thought that we could do this in the desugarer by decomposing nested `HsApp`s, using a little stack data type to denote all the different ways a function could be applied (`HsApp`, `HsWrap` with the right wrapper, sections, tuple-sections, `HsTypeApp`, maybe more) uncovering what the function was underneath, and then checking how many parameters are supplied. But now, I think it's much better to do this in the type-checker, especially because the type-checker already decomposes nested `HsApp`s. (See `TcExpr.tcApp`.) When it discovers what the function is, it can check whether the function is a `hasNoBinding` primop. If so, it can eta-expand as necessary (but only if necessary) and use a new piece of `HsSyn` to denote a saturated primop. (It will be a new invariant that no unsaturated primop passes the type-checker.) This seems better than redoing the stack type in the desugarer. The original problem in [\#13233](https://gitlab.haskell.org//ghc/ghc/issues/13233) was around levity polymorphism. If we make this change in the type checker, then the existing levity polymorphism checks should just work. We'll have to be careful to make the `HsSyn` structure printable in the way a user expects, so that the levity-polymorphism error message doesn't talk about an argument the user didn't write.
  1. How to make sure that saturated primops stay that way in Core. This would be a new check in Lint as well as new checks on any code that does eta-contraction. It has been suggested that levity-polymorphic primops desugar to a family of levity-monomorphic primops. This surely would work, but there doesn't seem to be benefit over a plan simply to keep primops eta-expanded always. Then, there's no worry about eta-contracting levity-polymorphic arguments.
- Make better use of the `uo_thing` field, including refactoring `noThing` away and improving term-level error messages.
- Take full advantage of `TcTyCon`, getting rid of the dreaded type-checking knot.
- Document why we're not worried about casts in class wanteds. (Short story: any cast should be available for rewriting, and so it will rewrite the kinds.)
- Sort out `matchTypeable` (see email) [\#13333](https://gitlab.haskell.org//ghc/ghc/issues/13333)
- Fix equality printing: 

  - Remove IfaceEqualityTyCon in favor of a new IfaceEquality constructor of IfaceType, which would be the conversion of a TyConApp
  - Make explicit-kinds print the kinds (duh) and equality-rels control the equality relation (duh)
  - Print \~ for homo in practice; print `~~` for hetero in practice (unless equality-rels)
- Merge fsk and fmv treatment, by returning the list of created fsks from `solveSimpleGivens` (which would now be fmvs) and fill them in after solving the wanteds. This eliminates problems around the fact that zonking in the flattener might zonk fsks back to type family applications and that fsks might lurk in residual constraints.

## Completed tasks

- Sort out `mkCastTy` (see email)

**Iceland_jack**: By `[]` as a data family do you mean:

```
datafamily[](a ::TYPE(rep ::RuntimeRep))::Typedatainstance[](a ::Type)=[]| a :[a]datainstance[](a ::TYPEIntRep)=INil|ICons a [a]...
```


I invite you to look at [ this gist](https://gist.github.com/Icelandjack/1824f4544c86b4ab497282783f94c360) posted on [\#12369](https://gitlab.haskell.org//ghc/ghc/issues/12369) and [\#13341](https://gitlab.haskell.org//ghc/ghc/issues/13341).
