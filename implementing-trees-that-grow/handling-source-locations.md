# Handling of Source Locations in Trees that Grow

## Problem


The current design of [ TTG HsSyn AST](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance) in GHC stores source locations for terms of a datatype `Exp` in a separate wrapper datatype `LExp` which is mutually recursive with `Exp` such that every recursive reference to `Exp` is done **indirectly**, via a reference to the wrapper datatype `LExp` (see the example code below). We refer to this style of storing source locations as the ping-pong style.


Besides the indirection and the resulting complications of the ping-pong style, there are two key problems with it: 

1. It bakes-in the source locations in the base TTG AST, forcing all instances to store source locations, even if they don't need them.
  For example, TH AST does not carry source locations. 

1. It results in a form of conceptual redundancy: source locations are tree decorations and they belong in the extension points.
  (see [ TTG Guidance](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance))

## Solutions


The key solution is to move source locations to the extension points, remove the indirection (e.g., the wrapper datatype `LExp`) altogether, and update the related code (e.g., functions over `Exp`) accordingly. 


We assume that open extension typefamily instances for GHC-specific decorations are nested, such that they call a closed typefamily to choose the extension based on the index (e.g., see `XApp` calling `XAppGHC` in the code below).
 
There are a couple of ways to implement such a solution:

1. We put the source locations in the new constructor extension, similar in spirit to the current `Located`.
1. We put the source locations in the new field extensions and use a typeclass to set/get the locations.


In the implementation, we have settled on the solution A, as it avoids the clutter. 


Notes:

- The API Annotations are similar to the `SrcSpan`, in that they are additional decorations, and also currently appear wherever there is a `SrcSpan`.
  The API Annotations can be accommodated via a straightforward extension of the type class approach, by defining

  ```
  dataExtra=ExtraSrcSpan[(SrcSpan,AnnKeywordId)]classHasExtra a where
                    getSpan :: a ->SrcSpan
                    setSpan :: a ->SrcSpan-> a
                  
                    getApiAnns :: a ->[(SrcSpan,AnnKeywordId)]
                    setApiAnns :: a ->[(SrcSpan,AnnKeywordId)]-> a
  ```

- We also currently have sections of AST without source locations, such as those generated when converting TH AST to hsSyn AST, or for GHC derived code.

>
> We can perhaps deal with these by either defining an additional pass, so

```
dataPass=Parsed|Renamed|Typechecked|Generatedderiving(Data)
```

>
> or by making the extra information status dependent on an additional parameter, so

```
dataGhcPass(l ::Location)(c ::Pass)derivinginstanceEq(GhcPass c)derivinginstance(Typeable l,Typeable c)=>Data(GhcPass l c)dataPass=Parsed|Renamed|Typecheckedderiving(Data)dataLocation=Located|UnLocated
```

>
> Thanks to Zubin Duggal for bringing the unlocated problem up on IRC.

- The setter/getter functions can be generalised to set/get anything:

  ```
  classHas b a where
    get :: a -> b
    set :: a -> b -> a
  ```

## An example to illustrate


To explain the design choices, we use a simple language of expressions.
Here are the base definitions in [TTG style](implementing-trees-that-grow/trees-that-grow-guidance):

```
{-# OPTIONS_GHC -Wall #-}{-# LANGUAGE TypeFamilies #-}moduleTTGwhere-- ------------------------------------------------ AST Base-- ----------------------------------------------dataExp x
  =Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(Exp x)|App(XApp x)(Exp x)(Exp x)|Par(XPar x)(Exp x)|New(XNew x)-- The extension constructortypefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x
typefamilyXId  x
```


with some basic GHC-specific types defined as

```
{-# OPTIONS_GHC -Wall -fno-warn-unticked-promoted-constructors #-}{-# LANGUAGE TypeFamilies , DataKinds #-}moduleBasicGHCTypeswhereimportData.Void-- ------------------------------------------------ GHC-Specific Declarations-- ----------------------------------------------dataPhase=Ps|Rn|TcdataGHC(p ::Phase)dataRdrName-- = the definition of RdrNamedataName-- = the definition of NamedataId-- = the definition of IddataSrcSpan-- = the definition of SrcSpandataType-- = the definition of SrcSpandataUnboundVar-- = the definition of UnboundVardataLocated a =LSrcSpan a

getLoc::Located a ->SrcSpangetLoc(L sp _)= sp

setLoc::Located a ->SrcSpan->Located a
setLoc(L_ x) sp' =L sp' x

noLoc::SrcSpannoLoc= undefined -- or be an empty SrcSpantypefamilyXAppGHC(p ::Phase)whereXAppGHCPs=()XAppGHCRn=()XAppGHCTc=TypetypefamilyXNewGHC(p ::Phase)whereXNewGHCPs=VoidXNewGHC_=UnboundVartypefamilyXIdGHC(p ::Phase)whereXIdGHCPs=RdrNameXIdGHCRn=NameXIdGHCTc=Id
```


Notice that the payload of the `Var` constructor is of type `XId x`. For
GHC, `x` will be instantiated to `GHC p`, and `XId` has a `type instance` that
delegates to `XIdGHC p`.  The latter can be defined by a nice *closed* type
family.

### Ping-pong style


Here is a representation of lambda expressions in the ping-pong style.
Unfortunately, this forces us to redefine the base TTG data type,
forcing it into ping-pong style, which is why we don't like it for the reasons mentioned above.

```
{-# OPTIONS_GHC -Wall #-}{-# LANGUAGE TypeFamilies #-}moduleOriginalwhereimportBasicGHCTypes-- ------------------------------------------------ AST Base-- ----------------------------------------------typeLExp x =Located(Exp x)dataExp x -- Notice the alternation between LExp and Exp=Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(LExp x)|App(XApp x)(LExp x)(LExp x)|Par(XPar x)(LExp x)|New(XNew x)-- The extension constructortypefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x
typefamilyXId  x

-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------typeinstanceXVar(GHC_)=()typeinstanceXAbs(GHC_)=()typeinstanceXApp(GHC p)=XAppGHC p
typeinstanceXPar(GHC_)=()typeinstanceXNew(GHC p)=XNewGHC p
typeinstanceXId(GHC p)=XIdGHC  p

-- ------------------------------------------------ Example Function-- ----------------------------------------------par::LExp(GHC x)->LExp(GHC x)par l@(L sp (App{}))=L sp (Par() l)par l                = l
```

### Solution A - Example Code


In the code below, as compared to the ping-pong style above, we have the following key changes:

- `LExp` is replaced with `Exp`
- a new constructor extension is introduced to wrap `Exp` with a `SrcSpan`
- a pattern synonym `LL` is introduced using the new constructor

```
{-# OPTIONS_GHC -Wall #-}{-# LANGUAGE TypeFamilies, PatternSynonyms #-}moduleSolutionBwhereimportBasicGHCTypesimportTTG-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------typeinstanceXVar(GHC_)=()typeinstanceXAbs(GHC_)=()typeinstanceXApp(GHC p)=XAppGHC p
typeinstanceXPar(GHC_)=()typeinstanceXNew(GHC p)=Either(Located(Exp(GHC p)))(XNewGHC p)typeinstanceXId(GHC p)=XIdGHC  p

-- NB: if GHC later wants to add extension fields to (say)-- XAbs, we can just redefine XAbs (GHC p) to be more like-- the XApp case-- ------------------------------------------------ LL Pattern Synonym-- ----------------------------------------------patternLL::SrcSpan->Exp(GHC p)->Exp(GHC p)patternLL sp m =New(Left(L sp m))-- ------------------------------------------------ Example Function-- ----------------------------------------------par::Exp(GHC p)->Exp(GHC p)par l@(LL sp (App{}))=LL sp (Par() l)par l                 = l
```

### Solution B - Example Code


In the code below, as compared to the ping-pong style above, we have the following key changes:

- `LExp` is replaced with `Exp`
- field extensions are set to have a `SrcSpan` paired (via `Located`)
  with a closed type family specialised for GHC phases
- a setter/getter function pair is introduced by a typeclass
- a pattern synonym `LL` is introduced using the setter/getter function pair

```
{-# OPTIONS_GHC -Wall #-}{-# LANGUAGE TypeFamilies, PatternSynonyms, ViewPatterns, FlexibleInstances #-}moduleSolutionAwhereimportData.VoidimportBasicGHCTypesimportTTG-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------typeinstanceXVar(GHC p)=Located()typeinstanceXAbs(GHC p)=Located()typeinstanceXApp(GHC p)=Located(XAppGHC p)typeinstanceXPar(GHC p)=Located()typeinstanceXNew(GHC p)=Located(XNewGHC p)typeinstanceXId(GHC p)=XIdGHC  p

-- NB: if GHC later wants to add extension fields to (say)-- XAbs, we can just redefine XAbs (GHC p) to be more like-- the XApp case-- ------------------------------------------------ HasSpan Typeclass and LL Pattern Synonym-- ----------------------------------------------classHasSpan a where
  getSpan :: a ->SrcSpan
  setSpan :: a ->SrcSpan-> a

instanceHasSpanSrcSpanwhere
  getSpan   = id
  setSpan _= id

instanceHasSpanVoidwhere
  getSpan x   = absurd x
  setSpan x _= absurd x

instanceHasSpan(Located a)where
  getSpan = getLoc
  setSpan = setLoc

instanceHasSpan(Exp(GHC p))where{- or,
type ForallX (p :: * -> Constraint) x
  = ( p (XVar x) , p (XAbs x) , p (XApp x) , p (XPar x)
    , p (XNew x) )

instance ForallX HasSpan x => HasSpan (Exp x) where
-}
  getSpan (Var ex _)= getSpan ex
  getSpan (Abs ex __)= getSpan ex
  getSpan (App ex __)= getSpan ex
  getSpan (Par ex _)= getSpan ex
  getSpan (New ex)= getSpan ex

  setSpan (Var ex x)   sp =Var(setSpan ex sp) x
  setSpan (Abs ex x n) sp =Abs(setSpan ex sp) x n
  setSpan (App ex l m) sp =App(setSpan ex sp) l m
  setSpan (Par ex m)   sp =Par(setSpan ex sp) m
  setSpan (New ex)     sp =New(setSpan ex sp)getSpan'::HasSpan a => a ->Located a
getSpan' m =L(getSpan m) m

patternLL::HasSpan a =>SrcSpan-> a -> a
patternLL s m <-(getSpan' ->L s m)whereLL s m =  setSpan m s

-- ------------------------------------------------ Example Function-- ----------------------------------------------par::Exp(GHC p)->Exp(GHC p)par l@(LL sp (App{}))=LL sp (Par(L noLoc ()) l)par l                 = l
```

## Pros & Cons

### Solution A


Pros:

- It makes it easy to omit locations altogether (see the notes about "Generated" code).
  This is a Good Thing.
- It makes it easy to store fewer locations (e.g. one location for `(f x y z)`, 
  rather than one for `(f x y z)`, one for `(f x y)`, and one for `(f x)`).
- It's easy to add the current location to the monad

> `f (XNew loc e) = setLoc loc $ f e`

>
> Simple, elegant!


Cons:

- At the binding site of a variable we know that we \*always\* have a location, and we can put that in its Name.  
  If locations were more optional, that would not be so true.

### Solution B


Pros:

- TODO


Cons:

- An instance of `HasSpan` should be defined per datatype
- Handling of the source locations should be done once per constructor
- When constructing/generating terms the first field of the constructors should explicitly mention the source location
  (see the `par` function in the Solution A's code, where the first field of `Par` should have a `SrcSpan`, even though a dummy one.)
