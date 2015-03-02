
\# Allow rules to examine context


As of GHC 7.10 rewrite rules, even built-in ones, cannot inspect the context of the term they are considering in deciding whether they will rewrite it. Context can be an important hint in determining whether a given term will benefit from a rewrite.


Consider, for example, the `litEq` built-in rule (see Bug [\#9661](https://gitlab.haskell.org//ghc/ghc/issues/9661) for further motivation). This rewrites expressions such as `n ==# 3` into case analyses of the form,

```
case n of3->True_->False
```


While this is usually a good thing, when applied indiscriminantly it will interfere with the user's attempt at using unboxed booleans. For instance,
the user might write,

```
f::Int->Stringf(I# n)=case isTrue# pred ofTrue->"That's Numberwang!"_->"Oh dear."where
    pred =(n ==#3#)`orI#`(n ==#42#)`orI#`(n ==#78#)`orI#`(n ==#90#)
```


Unfortunately in this case `litEq` will rewrite the user's carefully written unboxed expression as a case expression,

```
f::Int->Stringf=\ ds_dPI ->case ds_dPI of_{I# n_an9 ->case n_an9 of_{
      __DEFAULT -> lvl_r3yJ;3-> lvl1_r3yK;42-> lvl1_r3yK;78-> lvl1_r3yK;90-> lvl1_r3yK
    }}
```


which produces the same branch-y assembler as the user was likely trying to avoid in the first place.


For this reason, we'd like to ensure that `litEq` (and similar built-in rewrite rules) does not rewrite unless the term is directly scrutinized by a case expression.


\#\# Implementation


Built-in rewrite rules are currently encoded as a \[\[`RuleFun`\],

```
typeRuleFun=DynFlags->InScopeEnv-- ^ The scope within which the call is embedded->Id-- ^ The name of the called function->[CoreExpr]-- ^ The arguments of the call->MaybeCoreExpr-- ^ The resulting rewrite if appropriate
```


The simplifier currently encodes the context surrounding the term being simplified in a zipper-like fashion in the `SimplCont` type. We want to allow the `RuleFun` access to the `SimplCont` so that it can account for context when deciding whether to rewrite,

```
typeRuleFun=DynFlags->InScopeEnv-- ^ The scope within which the call is embedded->SimplCont-- ^ The context surrounding the call->Id-- ^ The name of the called function->[CoreExpr]-- ^ The arguments of the call->MaybeCoreExpr-- ^ The resulting rewrite if appropriate
```