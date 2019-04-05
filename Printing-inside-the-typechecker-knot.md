As explained in [this wiki page](commentary/compiler/tying-the-knot), reckless use of `pprTrace` statements in code that lives in the typechecker knot can cause GHC to loop forever. This can make debugging code that lives in the knot extremely painful. This page serves to document some useful advice for when you really, _really_ need to `pprTrace` inside the knot. See also [this `ghc-devs` mailing list discussion](https://mail.haskell.org/pipermail/ghc-devs/2019-April/017447.html), which sparked a conversation that led to this wiki page.

## Zonks, scoob

As a concrete example, consider the [following code](ttps://gitlab.haskell.org/ghc/ghc/blob/51fd357119b357c52e990ccce9059c423cc49406/compiler/typecheck/TcTyClsDecls.hs#L2312-2318) from `TcTyClsDecls.tcConDecl`:

```hs
       ; (ze, tkvs)     <- zonkTyBndrs tkvs
       ; (ze, user_tvs) <- zonkTyBndrsX ze user_tvs
       ; arg_tys <- zonkTcTypesToTypesX ze arg_tys
       ; ctxt    <- zonkTcTypesToTypesX ze ctxt
       ; res_ty  <- zonkTcTypeToTypeX   ze res_ty

       ; let (univ_tvs, ex_tvs, tkvs', user_tvs', eq_preds, arg_subst)
               = rejigConRes tmpl_bndrs res_tmpl tkvs user_tvs res_ty
```

It's often useful to print out what `univ_tvs`, `ex_tvs`, etc. are, but unfortunately, these values are deeply entwined with the typechecker knot, so doing so will make GHC spin forever. Luckily, there is a way to work around this: use different zonking functions! Paraphrasing Richard's advice from [here](https://phabricator.haskell.org/D4974#137224), the steps are as follows:

1. First, zonk everything from functions from `TcMType` (e.g., `zonkTcType`), not `TcHsSyn` (e.g., `zonkTcTypeToType`).
2. Pass the results of those zonks into `rejigConRes`.
3. Call `pprTrace` on the results of `rejigConRes`.
4. Finally, zonk the results of `rejigConRes` using function from `TcHsSyn`.

This double-zonking is redundant, so it's probably not wise to actually use this trick for anything besides debugging, but it shouldn't cause other problems.

## The zonking conversion guide

Step (1)—using zonking functions from `TcMType` instead of `TcHsSyn`—is not always clear. For example, what is the `TcMType` equivalent of `TcHsSyn.zonkTcTypeToTypeX :: ZonkEnv -> TcType -> TcM Type`? From a quick search of `TcMType`, there is no function which a type signature even close to that—in fact, no function in `TcMType` ever mentions `ZonkEnv`!

Fear not, because you don't actually _need_ the `ZonkEnv` in step (1)—it's just an optimization (see [here](https://mail.haskell.org/pipermail/ghc-devs/2019-April/017454.html) for a more detailed explanation). You can get by with using the `ZonkEnv`-less function `TcMType.zonkTcType :: TcType -> TcM TcType`.

Here is a table of corresponding functions from `TcMType` and `TcHsSyn`:

| `TcMType` | `TcHsSyn` |
| ------ | ------ |
| `zonkTcType` | `zonkTcTypeToType` |
| `zonkTyCoVarKind` | `zonkTyBndr`* |
| `zonkTyCoVarKinds`* | `zonkTyBndrs` |
| `zonkTcTyVar` | `zonkTyVarOcc` |
| `zonkTcTyVars` | `zonkTyVarOccs`* |

* These functions don't actually exist yet, but it would not be difficult to create them.