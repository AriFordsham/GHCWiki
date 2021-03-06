# Refactoring GHC Types


This page is to document several ideas for how to refactor the way types are stored and processed within GHC. The general goal here is to clean up code, but it is also possible we'll be able to improve error messages through this work.


This page was started in June 2018.


See Trac #15479

## Desugaring types separately



Currently (June 2018), GHC sports these two functions:


```
tc_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType -- in TcHsType.hs
tcExpr :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTcId)   -- in TcExpr.hs
```


The first takes a Haskell type (`HsType GhcRn`) and its expected kind and outputs a `TcType`: a Core type (possibly with `TcTyVar`s in it -- that's the `Tc` prefix). In contrast, the second takes a Haskell expression (`HsExpr GhcRn`) and its expected type and outputs a `HsExpr GhcTcId`: a Haskell expression using `TcId`s as its binders. Despite performing parallel operations, these two functions have wildly different output forms: one is a Core thing and one is a Haskell thing. Accordingly, expressions must be desugared in a later pass to Core expressions, while types are essentially desugared while being checked.


The first goal in this refactor is to align these functions. Desugaring separately has several advantages:

- Type checking is complicated. Desugaring can be, too. Better to separate them.
- We report type errors during type checking (and sometimes afterwards). These messages can use the code the user has written only if we don't desugar simultaneously. For example, if we declare `x :: Int `Either` Bool`, currently GHC reports `x :: Either Int Bool` in error messages, having forgotten about the user's syntax. Perhaps if we save the user's syntax, we can also be less worried about expanding type synonyms as we solve.
- As we will see later, doing this sets the stage for removing the "knot" in type checking.


We thus want


```
tc_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM (HsType GhcTc)
```


Note the new return type.

**SLPJ**: I hypothesise that `tc_hs_type` would always immediately be followed by `dsHsType`.   For example, given a type signature `f :: Int -> Int`, I want to add `f` to the type environment with its `Type` (emphatically not its `HsType`). 


Is there ever a case where we do not want to immediately desugar?  **End SLPJ**.

**RAE**: I would argue that during type-checking, we want to add `f :: Int -> Int` into the environment using a `HsType GhcTc`, not a `Type`. Then, if we ever have to print `f`'s type, we can print it correctly. For example, suppose the user wrote `f :: (->) Int Int`. Let's print that! **End RAE**



Problematically, we need to be able to compare types for equality. **SLPJ**: where, precisely? **End SLPJ** **RAE:** In `unifyType`, for example. **End RAE**  (We don't do this for terms, at least outside the simplifier.) So storing type-checked types in `HsType` can be troublesome -- we don't want to have to compute equality over `HsType`s. So we'll need to make liberal use of the extension fields to store the `Type` equivalent of each node in the `HsType`. With this, getting from an `HsType GhcTc` to a `TcType` is just a field access.



This plan will require changing zonking to work over `HsType GhcTc` and will require adding rules in the desugarer to desugar `HsType`s. Of course, desugaring is dead simple: just retrieve the core type from the extension field.

**SLPJ:** I think there is a much less ambitious version that goes like this:
 

1. `tc_hs_type` returns a `HsType GhcTc`, which may contain kind-unification variables.
1. Then solve constraints and zonk
1. Then desugar to produce a `Type`.

**RAE:** This means that we're not adding `HsType`s to the env't. I suppose this would be an intermediate step toward the overall goal of using `HsType`s throughout the typechecker. It would enable the following "removing the second pass" step with less up-front work.
**End RAE**

## Removing the second pass in TcTyClsDecls


All type-level declarations (`data`, `class`, etc) are handled in the TcTyClsDecls module. This module takes two full passes over type declarations: the first accumulates kind constraints (so that, say, a datatype constructor type can inform the kind of a datatype parameter), and then the second effectively desugars. Regrettably, the second pass repeats much of the work of the first pass, as the first pass only gathers constraints -- it doesn't store any information in the AST. So, after all the kind constraints are gathered (and solved), we throw away the result and start again.


Instead, we can imagine that the first pass produces `TyClDecl GhcTc` that saves what information it needs so that the second pass is really just desugaring. Currently, the only way of saving information between passes is to store it in the `TcTyCon` produced as a "partially formed" `TyCon`. This is a poor fit -- instead, let's just have `TyClDecl GhcTc`.

**SLPJ** I believe we would still need `TcTyCon`. Consider

```wiki
data T a = MKT (T a) Int
```


When looking at T's RHS we must have a kind for `T` in the type envt.  The `TcTyCon` precisely carries that kind.


Moreover in terms we have `AbsBinds`, which allow us to traverse the RHS of the function defn with `f :: alpha` in the type envt, and later generalise it.  We have no equivalent for types.  So we are more or less forced to take two passes.  Perhaps we count the second as "desugaring"; fair enough.   **End SLPJ**

**RAE:** Yes, we may still need `TcTyCon`, but with a few changes. The kind in a `TcTyCon` would be a `HsKind GhcTc`, so that we can print it the way the user wrote it. It would also not need to have `tcTyConScopedTyVars` or `tcTyConUserTyVars`, both of which really should end up in `TyClDecl GhcTc`.


And, yes, I count the second pass as desugaring, and we should aim for it never to fail. **End RAE**


One wonderful benefit of all this is that we would no longer need to use knots in type-checking. Instead, the knot would just be in desugaring, which is vastly simpler than type checking.

## Separating `TcType` from `Type`


Separately from the refactoring above, it might also be nice to make `TcType` and `Type` to be distinct. The former can store unification variables, while the latter never does. If we separate the types, we can ensure this statically. In addition, GHC currently is confused about the difference between proper quantified tyvars and type-checking skolems. Pulling `TcType` away from `Type` cleans this up. Desugaring would become converting from the former to the latter.
