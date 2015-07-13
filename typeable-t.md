# Type Indexed Type Representations Proposal

## Overview


Proposed API for type indexed type representations, and basic `Dynamic` functionality, without using pattern synonyms.
We consider 3 modules `TyConT`, `Typeable` and `Dynamic`, built up in that order.
We consider two varients, one for ghc as of 2015-07-10 with kind-homogenous equalities `a:~:b` only, and one for type hetrogenous type equalities `a:~~:b`.
The TCB consists of `TyConT` and `Typeable` in the homogenous case, and just `TyConT` in the hetrogenous case.

## Homogenous Case

`TyConT`:

```
dataTyConT(a::k)-- abstracteqTyConT::TyConT(a :: k1)->TyConT(b :: k2)->BooleqTyConTHom::TyConT(a :: k)->TyConT(b :: k)->(a :~: b)-- this needs unsafeCoerce-- compiler support for generating (e.g.)tyConTBool::TyConTBooltyConTArr::TyConT(->)
```


Note `eqTyConT` is not hugely useful as (if it returns True) we know that types and kinds are the same, but GHC doesn't, so unsafeCoerce is often needed.

`Typeable`:

```
dataTypeRetT(a::k)--abstractclassTypeable(a :: k)where
  typeRepT ::TypeRepT a
instance(Typeable c,Typeable a)=>Typeable(c a)typeOf::Typeable a => a ->TypeRepT a -- for convenienceeqTT::TypeRepT(a::k1)->TypeRepT(b::k2)->BooleqTTHom::TypeRepT(a::k)->TypeRepT(b::k)->Maybe(a :~: b)dataGetApp(a::k)whereGA::TypeRepT a ->TypeRepT b ->GetApp(a b)getApp::TypeRepT(a::k)->Maybe(GetApp a)-- no unsafeCoerce neededdataG1 c a whereG1::TypeRepT a ->G1 c (c a)get1::TypeRepT(c :: k1->k)->TypeRepT(a::k)->Maybe(G1 c a)-- uses an unsafeCoercedataG2 c a whereG1::TypeRepT a ->TypeRepT b ->G2 c (c a b)get2::TypeRepT(c :: k2->k1->k)->TypeRepT(a::k)->Maybe(G1 c a)--uses an unsafeCoerce--TODO: how many of these should we provide?getFn::TypeRepT(a ::*)->Maybe(G2(->) a)-- convenience, specialise get2, don't need unsafeCoerce-- compiler support for generating (e.g.)instanceTypeableBoolinstanceTypeable(->)
```


Similar notes to `eqTyConT` apply to `eqTT`.

`Dynamic`

```
dataDynamicwhereDyn::TypeRepT a -> a ->DynamicmkDyn::Typeable a => a ->Dynamic-- for conveniencedynApply::Dynamic->Dynamic->MaybeDynamic-- type-safely apply a dynamic function to a dynamic argument
```

## Hetrogenous Case


The only changes are that `eqTyConT` and `eqTT` now return `a:~~:b`, and are more useful (don't force us to use `unsafeCoerce`), `get1` and `get2` don't need `unsafeCoerce`, and we can generalise `getFn` to be poly-kinded.
The `get*` could now be implemented outside of the TCB, but we should keep them around for compatibility and convenience.
