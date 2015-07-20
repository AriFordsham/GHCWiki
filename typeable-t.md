# Type Indexed Type Representations Proposal

## Overview


Proposed API for type indexed type representations, and basic `Dynamic` functionality, without using pattern synonyms.
We consider 3 modules `TyConT`, `Typeable` and `Dynamic`, built up in that order.
We consider two varients, one for ghc as of 2015-07-10 with kind-homogenous equalities `a:~:b` only, and one for kind-hetrogenous type equalities `a:~~:b`.
The TCB consists of `TyConT` and `Typeable` in the homogenous case, and just `TyConT` in the hetrogenous case.

## Homogenous Case

`TyConT`:

```
dataTyConT(a::k)-- abstracteqTyConT::TyConT(a :: k1)->TyConT(b :: k2)->BooleqTyConTHom::TyConT(a :: k)->TyConT(b :: k)->Maybe(a :~: b)-- eqTyConTHom needs unsafeCoerce its implementation-- compiler support for generating (e.g.)tyConTBool::TyConTBooltyConTArr::TyConT(->)
```


The type `(:~:)` comes from `Data.Type.Equality`.


Note `eqTyConT` is not hugely useful as (if it returns True) we know that types and kinds are the same, but GHC doesn't, so unsafeCoerce is often needed.

`Typeable`:

```
dataTypeRepT(a::k)-- Type-indexed type representation; abstracttyConT::TyConT a ->TypeRepT a
appT::TypeRepT a ->TypeRepT b ->TypeRepT(a b)classTypeable(a :: k)where
  typeRepT ::TypeRepT a

withTypeRepT::TypeRepT a ->(Typeable a => b)-> b
-- c.f. Trac #2439typeOf::Typeable a => a ->TypeRepT a -- for convenienceeqTT::TypeRepT(a::k1)->TypeRepT(b::k2)->BooleqTTHom::TypeRepT(a::k)->TypeRepT(b::k)->Maybe(a :~: b)getConT::TypeRepT a ->Maybe(TyConT a)dataGetApp(a::k)whereGA::TypeRepT a ->TypeRepT b ->GetApp(a b)getAppT::TypeRepT(a::k)->Maybe(GetApp a)-- no unsafeCoerce neededdataG1 c a whereG1::TypeRepT a ->G1 c (c a)getT1::TypeRepT(c :: k1->k)->TypeRepT(a::k)->Maybe(G1 c a)-- Implementation uses an unsafeCoercedataG2 c a whereG2::TypeRepT a ->TypeRepT b ->G2 c (c a b)getT2::TypeRepT(c :: k2->k1->k)->TypeRepT(a::k)->Maybe(G1 c a)-- Implementation uses an unsafeCoerce--TODO: how many of these should we provide?getFnT::TypeRepT(a ::*)->Maybe(G2(->) a
getFnT= getT2 (tyConT tyConTFun)-- convenience, specialise get2, don't need unsafeCoerce-- GHC has magic built-in support for Typeable instances-- but the effect is similar to declarations like these:instance(Typeable c,Typeable a)=>Typeable(c a)instanceTypeableBoolinstanceTypeable(->)
```


Similar notes to `eqTyConT` apply to `eqTT`.

`Dynamic`

```
dataDynamicwhereDyn::TypeRepT a -> a ->DynamicmkDyn::Typeable a => a ->Dynamic-- for conveniencegetDyn::TypeRepT a
        ->Dynamic-- ^ the dynamically-typed object->Maybe a      -- ^ returns: @'Just' a@, if the dynamically-typed-- object has the correct type (and @a@ is its value), -- or 'Nothing' otherwise.dynApply::Dynamic->Dynamic->MaybeDynamic-- type-safely apply a dynamic function to a dynamic argument
```

## Hetrogenous Case


In this case, where we have a kind-hetrogenous `:~~:`, life becomes simpler: we now never need unsafeCoerce in `getT1` and the like, so we can now just export `getAppT` and leave the rest to the users.
 
The changes are that `eqTyConT` and `eqTT` now return `a:~~:b`, and are more useful (don't force us to use `unsafeCoerce`), `getT1` and `getT2` don't need `unsafeCoerce`, and we can generalise `getFnT` to be poly-kinded.
We obviously may want to provide (and deprecate) `getT1`, `eqTyConTHom` etc. for compatibility, and maybe provide `getFnT` for convenience, if it turns out to be used a lot etc.


(I am not yet sure whether it would be useful to keep the homogenous equality functions around --- potentially enforcing kind homogeneity could be useful)

`TyConT`:

```
dataTyConT(a::k)-- abstracteqTyConT::TyConT(a :: k1)->TyConT(b :: k2)->Maybe(a :~~: b)-- compiler support for generating (e.g.)tyConTBool::TyConTBooltyConTArr::TyConT(->)
```

`Typeable`:

```
dataTypeRepT(a::k)-- Type-indexed type representation; abstracttyConT::TyConT a ->TypeRepT a
appT::TypeRepT a ->TypeRepT b ->TypeRepT(a b)classTypeable(a :: k)where
  typeRepT ::TypeRepT a

withTypeRepT::TypeRepT a ->(Typeable a => b)-> b
-- c.f. Trac #2439typeOf::Typeable a => a ->TypeRepT a -- for convenienceeqTT::TypeRepT(a::k1)->TypeRepT(b::k2)->Maybe(a:~~:b)getConT::TypeRepT a ->Maybe(TyConT a)dataGetApp(a::k)whereGA::TypeRepT a ->TypeRepT b ->GetApp(a b)getAppT::TypeRepT(a::k)->Maybe(GetApp a)-- no unsafeCoerce needed-- GHC has magic built-in support for Typeable instances-- but the effect is similar to declarations like these:instance(Typeable c,Typeable a)=>Typeable(c a)instanceTypeableBoolinstanceTypeable(->)
```

`Dynamic`

```
dataDynamicwhereDyn::TypeRepT a -> a ->DynamicmkDyn::Typeable a => a ->Dynamic-- for conveniencegetDyn::TypeRepT a
        ->Dynamic-- ^ the dynamically-typed object->Maybe a      -- ^ returns: @'Just' a@, if the dynamically-typed-- object has the correct type (and @a@ is its value), -- or 'Nothing' otherwise.dynApply::Dynamic->Dynamic->MaybeDynamic-- type-safely apply a dynamic function to a dynamic argument
```