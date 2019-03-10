# A plan for type-indexed type representations


This is Ben Gamari's plan for moving ahead with the type-indexed `Typeable`
scheme, described most recently in
[ A reflection on types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/).

## Status


A branch with the current state of things can be found [ here](https://github.com/ghc/ghc/compare/master...bgamari:wip/ttypeable). There is also an intermittently-updated Phabricator differential, [ Phab:D2010](https://phabricator.haskell.org/D2010). It's still rather preliminary however it does give you a working stage2 compiler with functional type-indexed type representations and what I believe is a pretty reasonable set of interfaces, described below.


There are a number of tasks outstanding. These involve only Ben,

- Fix representation pretty-printer to correctly handle tuples, lists, arrows, and precedence
- Examine performance changes (even some improvements, perplexingly)
- Look at testsuite failures involving stack overflows: `T10294, plugins01, T5550, annrun01, ann01, annth_make`
- Evaluate whether we want to try harder to preserve the re-exports that have been dropped from `Data.Dynamic`.
- Some of the new naming choices should be revisited (e.g. `TRFun`)
- Typeable fingerprints need to be made more robust ([\#7897](https://gitlab.haskell.org//ghc/ghc/issues/7897))
- Performance:

  - Carefully check `Data.Typeable` for deviations from the previous interface and fill in gaps where possible
  - Examine whether the primitive `TypeRep`s (e.g. `trTYPE`, `trArrow`) should be `INLINEABLE`; we perform a number of runtime checks during serialization/deserialization so being able to inline these fingerprints may be helpful.
  - Perhaps generated `TyCon`s should also be inlined (or generate rules to inline just fingerprints)
  - Perhaps it would be worth offering unsafe deserialization interface without runtime checks?


These are issues that need to be addressed elsewhere in the compiler,

- [\#11736](https://gitlab.haskell.org//ghc/ghc/issues/11736): Core Lint rejects unsaturated applications of unlifted types; it's not clear whether this is actually a safe thing to do
- [\#11714](https://gitlab.haskell.org//ghc/ghc/issues/11714): kind of  `(->)` is overly-restrictive consequently `T11120` testcase fails
- [\#11715](https://gitlab.haskell.org//ghc/ghc/issues/11715): `TypeOf` fails due to the fact that `Constraint` and `*` are indistinguishable in Core

## `Type.Reflection`


The new type-indexed typeable machinery will be exposed via a new module
(`Type.Reflection` is chosen here, although this name is still up in the air;
`Reflection` in particular has an unfortunate conflict with Edward Kmett's `reflection`
library). The user-visible interface of `Type.Reflection` will look like this,

```
-- The user-facing interfacemoduleType.ReflectionwhereclassTypeable(a :: k)-- This is how we get the representation for a typetypeRep:: forall (a :: k).Typeable a =>TypeRep a 

-- This is merely a record of some metadata about a type constructor.-- One of these is produced for every type defined in a module during its-- compilation.---- This should also carry a fingerprint; to address #7897 this fingerprint-- should hash not only the name of the tycon, but also the structure of its-- data constructorsdataTyContyConPackage::TyCon->StringtyConModule::TyCon->StringtyConName::TyCon->String-- A runtime type representation with O(1) access to a fingerprint.dataTypeRep(a :: k)instanceShow(TypeRep a)-- Since TypeRep is indexed by its type and must be a singleton we can trivially-- provide theseinstanceEq(TypeRep a)where(==)__=TrueinstanceOrd(TypeRep a)where compare __=EQ-- While TypeRep is abstract, we can pattern match against it.-- This can be a bi-directional pattern (using mkTrApp for construction).patternTRApp:: forall k2 (fun :: k2).()=> forall k1 (a :: k1 -> k2)(b :: k1).(fun ~ a b)=>TypeRep a ->TypeRep b ->TypeRep fun

-- Open question: Should this pattern include the kind of the constructor?-- In practice you often need it when you need the TyConpatternTRCon:: forall k (a :: k).TyCon->TypeRep a

-- Decompose functions, also bidirectionalpatternTRFun:: forall fun.()=> forall arg res.(fun ~(arg -> res))=>TypeRep arg
              ->TypeRep res
              ->TypeRep fun

-- We can also request the kind of a typetypeRepKind::TypeRep(a :: k)->TypeRep k

-- and compare typeseqTypeRep:: forall k (a :: k)(b :: k).TypeRep a ->TypeRep b ->Maybe(a :~: b)eqTypeRep':: forall k1 k2 (a :: k1)(b :: k2).TypeRep a ->TypeRep b ->Maybe(a :~~: b)-- it can also be useful to quantify over the type such that we can, e.g.,-- index a map on a typedataTypeRepXwhereTypeRepX:: forall a.TypeRep a ->TypeRepX-- these have some useful instancesinstanceEqTypeRepXinstanceOrdTypeRepXinstanceShowTypeRepX-- A `TypeRep a` gives rise to a `Typeable a` instance without loss of-- confluence.withTypeable::TypeRep a ->(Typeable a => b)-> b
withTypeable= undefined

-- We can also allow the user to build up his own applicationsmkTrApp:: forall k1 k2 (a :: k1 -> k2)(b :: k1).TypeRep(a :: k1 -> k2)->TypeRep(b :: k1)->TypeRep(a b)-- However, we can't (easily) allow instantiation of TyCons since we have-- no way of producing the kind of the resulting type...--mkTrCon :: forall k (a :: k). TyCon -> [TypeRepX] -> TypeRep a
```

## Preserving compatibility with `Data.Typeable`


Note how above we placed the new type-indexed Typeable machinery in an entirely new
module. The goal of this is to preserve compatibility with the old
`Data.Typeable`. Notice how the old `Data.Typeable.TypeRep` is essentially
`TypeRepX` under the new scheme. This gives us a very nice compatibility story
(thanks due to Richard Eisenberg for first proposing this),

```
moduleData.Typeable(-- We can use the same Typeable classI.Typeable,I.TyCon,I.tyConPackage,I.tyConModule,I.tyConName,(:~:)(Refl),moduleData.Typeable)whereimportType.Reflectionas I
importData.Type.Equality-- Merely expose TypeRepX opaquely under the old nametypeTypeRep=I.TypeRepXtypeOf:: forall a.Typeable a => a ->TypeReptypeOf_=I.typeRepX (Proxy::Proxy a)typeRep:: forall proxy a.Typeable a => proxy a ->TypeReptypeRep=I.typeRepX

cast:: forall a b.(Typeable a,Typeable b)=> a ->Maybe b
cast x
  |JustHRefl<- ta `I.eqTypeRep` tb =Just x
  | otherwise                         =Nothingwhere
    ta =I.typeRep ::I.TypeRep a
    tb =I.typeRep ::I.TypeRep b

eqT:: forall a b.(Typeable a,Typeable b)=>Maybe(a :~: b)eqT|JustHRefl<- ta `I.eqTypeRep` tb =JustRefl| otherwise                         =Nothingwhere
    ta =I.typeRep ::I.TypeRep a
    tb =I.typeRep ::I.TypeRep b

funResultTy::TypeRep->TypeRep->MaybeTypeRepfunResultTy(I.TypeRepX f)(I.TypeRepX x)|JustHRefl<-(I.typeRep ::I.TypeRepType)`I.eqTypeRep`I.typeRepKind f
  ,I.TRFun arg res <- f
  ,JustHRefl<- arg `I.eqTypeRep` x
  =Just(I.TypeRepX res)| otherwise
  =NothingtypeRepTyCon::TypeRep->TyCon-- the old typeOfN exports from the pre-PolyKinds days can-- also be trivially provided.
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


First, we need to define how deserialization should behave. For instance, defining

```
getTypeRep::Get(TypeRep a)
```


is a non-starter as we have no way to verify that the representation that we
deserialize plausibly represents the type `a` that the user requests.


Instead, let's first consider `TypeRepX` (thanks to Adam Gundry for his guidance),

```
getTypeRepX::GetTypeRepXgetTypeRepX=do
    tag <- get ::GetWord8case tag of0->do con <- get ::GetTyConTypeRepX rep_k <- getTypeRepX
                case rep_k `eqTypeRep`(typeRep ::TypeRepType)ofJustHRefl-> pure $TypeRepX$ mkTrCon con rep_k
                    Nothing-> fail "getTypeRepX: Kind mismatch"1->doTypeRepX f <- getTypeRepX
                TypeRepX x <- getTypeRepX
                case typeRepKind f ofTRFun arg _|JustHRefl<- arg `eqTypeRep` x ->
                      pure $TypeRepX$ mkTrApp f x
                    _-> fail "getTypeRepX: Kind mismatch"_-> fail "getTypeRepX: Invalid TypeRepX"
```


Note how we need to invoke type equality here to ensure,

- in the case of a tycon: that the tycon's kind is `Type` (as all kinds must be in the `TypeInType` scheme)
- in the case of applications `f x`:

  - that the type `f` is indeed an arrow
  - that the type `f` is applied at the type `x` that it expects


Given this we can easily implement `TypeRep a` given a representation of the expected `a`,

```
getTypeRep::Typeable a =>Get(TypeRep a)getTypeRep=doTypeRepX rep <- getTypeRepX
   case rep `eqTypeRep`(typeRep ::TypeRep a)ofJustHRefl-> pure rep
       Nothing-> fail "Binary: Type mismatch"
```

### Alternative: Through static data?


One might have the idea that the solution here may be to avoid encoding
representations at all: instead use GHC's existing support for static data, e.g.
add `TypeRep a` entries to the static pointer table for every known type. One will quickly realize, however, that
this is unrealistic: we have no way of enumerating the types that must
be considered and even if we did, there would be very many of them.

## Alternative: Type-indexed `TyCon`s?


Under the above proposal `TyCon` is merely a record of static metadata; it has no
type information and consequently the user is quite limited in what they can do with it.
Another point in the design space would be to add a type index to `TyCon`,

```
-- metadata describing a tycondataTyConMeta=TyConMeta{ tyConPackage ::String, tyConModule  ::String, tyConName    ::String}newtypeTyCon(a :: k)=TyConTyConMetapatternTRCon::TyCon a ->TypeRep a

-- which allows us to providemkTyCon::TyCon a ->TypeRep a
```


While this is something that we could do, I have yet to see a compelling reason
why we **should** do it. The only way you can produce a `TyCon` is from a `TypeRep`,
so ultimately you should be able to accomplish everything you can with type-index
`TyCon`s by just not destructuring the `TypeRep` from which it arose.

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

## Implementation notes


The implementation of the above plan shares a great deal with the previous
`Typeable` implementation. As we did before, we split the generation of
`Typeable` representations into two halves,

1. At time of type definition: When compiling a type definition we emit a
  `TyCon` binding describing its type constructor

1. When solving for a `Typeable` constraint: We produce a dictionary referring
  to `TyCon` binding associated with the type for which `Typeable` is needed.


Step (1) is essentially unchanged from the previous implementation. There's
nothing particularly interesting here other than some tiresome details regarding
generating representations for primitive types, which doesn't really change (see
`Note [The Grand Typeable Story]` in `TcTypeable` for details).


Step (2), however, changes slightly in that it needs to produce evidence for the
kind of type.

`Typeable` evidence is merely a `TypeRep`,

```
dataTypeRep(a :: k)whereTrTyCon::!Fingerprint->!TyCon->TypeRep k ->TypeRep(a :: k)TrApp:: forall k1 k2 (a :: k1 -> k2)(b :: k1).!Fingerprint->TypeRep a
            ->TypeRep b
            ->TypeRep(a b)
```


Note that a `Fingerprint` is included in each node for O(1) comparison.

### Dealing with recursive kinds


The fact that we now have the ability to reflect on kinds poses an interesting
challenge (especially in the `TypeInType` world) as we now have to worry about 
recursive kind relationships during evidence generation. Thankfully there are
only a few types which we need to worry about,

```
TYPE::RuntimeRep->TYPE'PtrRepLiftedRuntimeRep::TYPE'PtrRepLifted'PtrRepLifted::RuntimeRep(->)::TYPE'PtrRepLifted->TYPE'PtrRepLifted->Type'PtrRepLiftedTYPE'PtrRepLifted::TYPE'PtrRepLifted
```


While in principle we could generate knot-tied dictionaries for these in the
typechecker, this would be quite tiresome; moreover these types are ubiquitous
and it would be wasteful to replicate representations for them.
Instead, the implementation manually defines representations for these types in
`Data.Typeable.Internal` and using these definitions instead of generated bindings.

### Alternative: Richer `TypeRep`


Another approach would be to to encode these special cases in the `TypeRep`
type itself

```
dataTypeRep(a :: k)whereTrTyCon::!Fingerprint->!TyCon->TypeRep k ->TypeRep(a :: k)TrApp:: forall k1 k2 (a :: k1 -> k2)(b :: k1).!Fingerprint->TypeRep a
            ->TypeRep b
            ->TypeRep(a b)TrArrow:: forall k1 k2.!Fingerprint->TypeRep k1
            ->TypeRep k2
            ->TypeRep((->):: k1 -> k2 ->*)TrTYPE::TypeRepTYPETrType::TypeRep(TYPE'PtrRepLifted)TrRuntimeRep::TypeRepRuntimeRepTr'PtrRepLifted::TypeRep'PtrRepLifted
```


(although `TrArrow` won't quite work yet due to [\#11714](https://gitlab.haskell.org//ghc/ghc/issues/11714))


With this we can easily write `typeRepKind`,

```
typeRepKind:: forall k (a :: k).TypeRep a ->TypeRep k
-- these cases are unchanged...typeRepKind(TrTyCon__ k)= k
typeRepKind(TrApp_ f _)=case typeRepKind f ofTRFun _arg res -> res

-- these are new...typeRepKind(TrArrow x y)= mkTrApp (mkTrApp (TrArrowTrTypeTrType) x) y
typeRepKindTrTYPE= mkTrApp TrRuntimeRepTrTypetypeRepKindTrType=TrTypetypeRepKindTrRuntimeRep=TrTypetypeRepKindTr'PtrRepLifted=TrRuntimeRep
```


Although providing pattern synonyms to allow decomposition of, e.g.,
`TYPE 'PtrTypeLifted` becomes a bit trickier,

```
-- Just as above, we can decompose applications but we-- now need to define it in terms of the splitApp helper,patternTRApp:: forall k2 (t :: k2).()=> forall k1 (a :: k1 -> k2)(b :: k1).(t ~ a b)=>TypeRep a ->TypeRep b ->TypeRep t
patternTRApp x y =(splitApp ->Just(App x y))dataAppResult(t :: k)whereApp::TypeRep a ->TypeRep b ->AppResult(a b)splitApp::TypeRep a ->Maybe(AppResult a)splitApp(TrTyCon___)=NothingsplitApp(TrApp_ f x)=Just$App f x
splitApp(TrArrow_ x y)=Just$App(mkTrApp (TrArrow(typeRepKind x)(typeRepKind y)) x) y
splitAppTrTYPE=NothingsplitAppTrType=Just$AppTrTYPETr'PtrRepLiftedsplitAppTrRuntimeRep=NothingsplitAppTr'PtrRepLifted=Nothing
```


Pretty much everything else follows from this. For instance `TRFun`,

```
patternTRFun:: forall fun.()=> forall arg res.(fun ~(arg -> res))=>TypeRep arg
              ->TypeRep res
              ->TypeRep fun
patternTRFun arg res <-TRApp_(TRApp_(TrArrow__) arg) res whereTRFun arg res = mkTrApp (mkTrApp trArrow arg) res
```


This approach trades some boilerplate complexity for a more complex
representation type. I'm not yet certain whether it would be an improvement over
the current state of affairs. It has the disadvantage that the implementation
needs to take care to normalize representations that it builds (e.g. prefer
`TrType` to `TrApp TrTYPE Tr'PtrRepLifted`). That being said, it may be a bit
more efficient for the compiler to produce dictionaries in this form.
