# A plan for type-indexed type representations


This is Ben Gamari's plan for moving ahead with the type-indexed `Typeable`
scheme, described most recently in
[ A reflection on types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic).

## `Data.Typeable`


The user-visible interface of `Data.Typeable` will look like this,

```
-- The user-facing interfacemoduleData.TypeablewhereclassTypeable(a :: k)-- This is how we get the representation for a typetypeRep:: forall (a :: k).Typeable a =>TypeRep a 

-- This is merely a record of some metadata about a type constructor.-- One of these is produced for every type defined in a module during its-- compilation.---- This should also carry a fingerprint; to address #7897 this fingerprint-- should hash not only the name of the tycon, but also the structure of its-- data constructorsdataTyContyConPackage::TyCon->StringtyConModule::TyCon->StringtyConName::TyCon->String-- A runtime type representation with O(1) access to a fingerprint.dataTypeRep(a :: k)instanceShow(TypeRep a)-- Since TypeRep is indexed by its type and must be a singleton we can trivially-- provide theseinstanceEq(TTypeRep a)where(==)__=TrueinstanceOrd(TTypeRep a)where compare __=EQ-- While TypeRep is abstract, we can pattern match against it:patternTRApp:: forall k2 (fun :: k2).()=> forall k1 (a :: k1 -> k2)(b :: k1).(fun ~ a b)=>TypeRep a ->TypeRep b ->TypeRep fun

-- Open question: Should this pattern include the kind of the constructor?-- It seems you often need it.patternTRCon:: forall k (a :: k).TyCon->TypeRep a

-- decompose functionspatternTRFun:: forall fun.()=> forall arg res.(fun ~(arg -> res))=>TypeRep arg
              ->TypeRep res
              ->TypeRep fun

-- We can also request the kind of a typetTypeRepKind::TypeRep(a :: k)->TypeRep k

-- and compare typeseqTypeRep:: forall k (a :: k)(b :: k).TypeRep a ->TypeRep b ->Maybe(a :~: b)eqTypeRep':: forall k1 k2 (a :: k1)(b :: k2).TypeRep a ->TypeRep b ->Maybe(a :~~: b)-- it can also be useful to quantify over the type such that we can, e.g.,-- index a map on a typedataTypeRepXwhereTypeRepX::TypeRep a ->TypeRepX-- these have some useful instancesinstanceEqTypeRepXinstanceOrdTypeRepXinstanceShowTypeRepX-- A `TypeRep a` gives rise to a `Typeable a` instance without loss of-- confluence.withTypeable::TypeRep a ->(Typeable a => b)-> b
withTypeable= undefined

-- We can also allow the user to build up his own applicationsmkTrApp:: forall k1 k2 (a :: k1 -> k2)(b :: k1).TTypeRep(a :: k1 -> k2)->TTypeRep(b :: k1)->TTypeRep(a b)-- However, we can't (easily) allow instantiation of TyCons since we have-- no way of producing the kind of the resulting type...--mkTrCon :: forall k (a :: k). TyCon -> [TypeRep] -> TTypeRep a
```

## The representation serialization problem


Serialization of type representations is a bit tricky in this new world. Let's
say that we want to serialize (say, using the `binary` package), for instance, a
type-indexed map,

```
dataTMap a
lookup::TypeRepX->TMap a ->Maybe a
insert::TypeRepX-> a ->TMap a ->TMap a

-- we want to support these operations...getTMap::Binary a =>Get(TMap a)putTMap::Binary a =>TMap a ->Put
```


This is the sort of usage we might see in, for instance, the `Shake` build system.

### Serializing `TypeRep`


Of course in order to provide `getTMap` and `putTMap` we need to be able to both
serialize and deserialize `TypeRepX`s. Serialization poses no particular issue.
For instance, we might write,

```
instanceBinaryTyConputTypeRep::TypeRep a ->PutputTypeRep tr@(TRCon tc)=do put 0
                              put tc
                              putTypeRep (typeRepKind tr)putTypeRep(TRApp f x)=do put 1
                              putTypeRep f
                              putTypeRep x

putTypeRepX::TypeRepX->PutputTypeRepX(TypeRepX rep)= putTypeRep rep
```


That was easy.


Now let's try deserialization.

### Deserialization


We first need to consider which of these two types, `TypeRep` and `TypeRepX`, we want to imp


First, we need to define how deserialization should behave. For instance, defining

```
getTypeRep::Get(TypeRep a)
```


is a non-starter as we have no way to verify that the representation that we
read plausibly represents the type `a` that the user requests.


For this we need more information: a `Typeable` dictionary,

```
getTypeRep::TypeRep a ->Get(TypeRep a)getTypeRep ty =do
    tag <- get ::GetWord8case tag of0|TRCon con <- ty  ->do
            con' <- get
            when (con' /= con)$ fail "Binary: Mismatched type constructors"
            getTypeRep (typeRepKind ty)1|TRApp rep_f rep_x <- ty  ->do
            getTypeRep rep_f
            getTypeRep rep_x
    pure ty
```


Note how here we aren't even constructing a new representation; we are merely
verifying that what we deserialize matches the representation that we expect.
Of course, the fact that we must know the type we are trying to deserialize
means that `getTypeRep` isn't terribly useful on its own; it certainly won't
help us to deserialize a `TMap`.


Then what of `TypeRepX`? We clearly can't use the `getTypeRep` defined above
since it requires that we already know which type we expect.
Furthermore, any attempt

```
getTypeRepX::GetTypeRepgetTypeRepX ty =do
    rep <- getTypeRep'
    pure $TypeRepX(rep ::TypeRep a)where
    getTypeRep' ::Get(TypeRep a)
    getTypeRep' =do
        tag <- get ::GetWord8case tag of0|TRCon con <- ty  ->do
                con' <- get
                rep_k <- getTypeRepX
                getTypeRep rep_k
            1|TRApp rep_f rep_x <- ty  ->do
                getTypeRepX rep_f
                getTypeRepX rep_x
        pure ty
      where rep_k = typeRepKind ty
```

### Through static data?


On might have the idea that the solution here may be to avoid encoding
representations at all: instead use GHC's existing support for static data, e.g.
add `TypeRep a` entries to the static pointer table for every known type. But of
course, this is unrealistic: we have no way of enumerating the types that must
be considered and even if we did, there would be very many of them.

### Through an un-indexed representation?


The "give up" approach here is to simply project the type-indexed `TypeRep` onto
something that is totally untyped,

```
dataTypeRepXX=TypeConXXTyCon|TypeAppXXTypeRepXXTypeRepXXtoTypeRep::TypeRepXX->Maybe(TypeRep a)toTypeRepX::TypeRepXX->TypeRepXfromTypeRep::TypeRep a ->TypeRepXXfromTypeRepX::TypeRepX->TypeRepXX
```

`TypeRepXX` is now just plain old data, requiring nothing special for
serialization and deserialization. However, this gives us an awkward third
variety of type representation

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
[ suggested](https://ghc.haskell.org/trac/ghc/wiki/TypeableT#Data.Dynamic) this
variant of `Dynamic`, which models a value of dynamic type "inside" of a known
functor. He p

```
dataSDynamic s whereSDynamic::TypeRep a -> s a ->SDynamic s

toSDynR::TypeRep a -> s a ->SDynamic s
toSDyn::Typeable a => s a ->SDynamic s
fromSDynamicR::TypeRep a ->SDynamic s ->Maybe(s a)fromSDynamic::Typeable a =>SDynamic s ->Maybe(s a)fromSDynR::TypeRep a ->SDynamic s -> s a -> s a
fromSDyn::Typeable a =>SDynamic s -> s a -> s a
```