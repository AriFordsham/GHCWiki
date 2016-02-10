# A plan for type-indexed type representations

## `Data.Typeable`


The user-visible interface of `Data.Typeable` will look like this,

```
-- The user-facing interfacemoduleData.TypeablewhereclassTypeable(a :: k)-- This is how we get the representation for a typetypeRep:: forall (a :: k).Typeable a =>TypeRep a 

-- This is merely a record of some metadata about a type constructor.-- One of these is produced for every type defined in a module during its-- compilation.---- This should also carry a fingerprint; to address #7897 this fingerprint-- should hash not only the name of the tycon, but also the structure of its-- data constructorsdataTyContyConPackage::TyCon->StringtyConModule::TyCon->StringtyConName::TyCon->String-- A runtime type representation with O(1) access to a fingerprint.dataTypeRep(a :: k)instanceShow(TypeRep a)-- Since TypeRep is indexed by its type and must be a singleton we can trivially-- provide theseinstanceEq(TTypeRep a)where(==)__=TrueinstanceOrd(TTypeRep a)where compare __=EQ-- While TypeRep is abstract, we can pattern match against it:patternTRApp:: forall k2 (fun :: k2).()=> forall k1 (a :: k1 -> k2)(b :: k1).(fun ~ a b)=>TypeRep a ->TypeRep b ->TypeRep fun

patternTRCon:: forall k (a :: k).TyCon->TypeRep a

-- decompose functionspatternTRFun:: forall fun.()=> forall arg res.(fun ~(arg -> res))=>TypeRep arg
              ->TypeRep res
              ->TypeRep fun

-- We can also request the kind of a typetTypeRepKind::TypeRep(a :: k)->TypeRep k

-- and compare typeseqTypeRep:: forall k (a :: k)(b :: k).TypeRep a ->TypeRep b ->Maybe(a :~: b)eqTypeRep':: forall k1 k2 (a :: k1)(b :: k2).TypeRep a ->TypeRep b ->Maybe(a :~~: b)-- it can also be useful to quantify over the type such that we can, e.g.,-- index a map on a typedataTypeRepXwhereTypeRepX::TypeRep a ->TypeRepX-- these have some useful instancesinstanceEqTypeRepXinstanceOrdTypeRepXinstanceShowTypeRepX-- A `TypeRep a` gives rise to a `Typeable a` instance without loss of-- confluence.withTypeable::TypeRep a ->(Typeable a => b)-> b
withTypeable= undefined

-- We can also allow the user to build up his own applicationsmkTrApp:: forall k1 k2 (a :: k1 -> k2)(b :: k1).TTypeRep(a :: k1 -> k2)->TTypeRep(b :: k1)->TTypeRep(a b)-- However, we can't (easily) allow instantiation of TyCons since we have-- no way of producing the kind of the resulting type...--mkTrCon :: forall k (a :: k). TyCon -> [TypeRep] -> TTypeRep a
```

## The representation serialization problem


Serialization of type representations is a bit tricky in this new world,

```
```

## `Data.Dynamic`

`Dynamic` doesn't really change,

```
moduleData.Dynamicwhere-- Dynamic itself no longer needs to be abstractdataDynamicwhereDynamic::TypeRep a -> a ->Dynamic-- ConstructiontoDynR::TypeRep a -> a ->DynamictoDyn::Typeable a => a ->Dynamic-- EliminationfromDynamicR::TypeRep a ->Dynamic->Maybe a
fromDynamic::Typeable a =>Dynamic->Maybe a

-- fromDynR::TypeRep a ->Dynamic-> a -> a
fromDyn::Typeable a =>Dynamic-> a -> a

-- ApplicationdynApp::Dynamic->Dynamic->Dynamic-- Existing function; calls error on failure-- I think this should be deprecateddynApply::Dynamic->Dynamic->MaybeDynamic
```


Ben Pierce also
[ https://ghc.haskell.org/trac/ghc/wiki/TypeableT\#Data.Dynamic\|suggested](https://ghc.haskell.org/trac/ghc/wiki/TypeableT#Data.Dynamic|suggested) this
variant of `Dynamic`, which models a value of dynamic type "inside" of a known
functor. He p

```
dataSDynamic s whereSDynamic::TypeRep a -> s a ->SDynamic s

toSDynR::TypeRep a -> s a ->SDynamic s
toSDyn::Typeable a => s a ->SDynamic s
fromSDynamicR::TypeRep a ->SDynamic s ->Maybe(s a)fromSDynamic::Typeable a =>SDynamic s ->Maybe(s a)fromSDynR::TypeRep a ->SDynamic s -> s a -> s a
fromSDyn::Typeable a =>SDynamic s -> s a -> s a
```