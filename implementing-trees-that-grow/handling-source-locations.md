# Handling of Source Locations in Trees that Grow

## Problem


The current design of [ TTG HsSyn AST](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance) in GHC stores source locations for terms of a datatype `Exp` in a separate wrapper datatype `LExp` which is mutually recursive with `Exp` such that every recursive reference to `Exp` is done **indirectly**, via a reference to the wrapper datatype `LExp` (see the example code below). We refer to this style of storing source locations as the ping-pong style.


Besides the indirection and the resulting complications of the ping-pong style, there are two key problems with it: 

1. It bakes-in the source locations in the base TTG AST, forcing all instances to store source locations, even if they don't need them.
  For example, TH AST does not carry source locations. 

1. It results in a form of conceptual redundancy: source locations are tree decorations and they belong in the extension points.
  (see [ TTG Guidance](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance))

### Example


For example, here is a simple [ TTG](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance) representation of lambda expressions in the ping-pong style.

```
{-# OPTIONS_GHC -Wall
                -fno-warn-unticked-promoted-constructors
#-}{-# LANGUAGE TypeFamilies
           , DataKinds
#-}moduleOriginalwhereimportData.VoiddataRdrName-- = the definition of RdrNamedataName-- = the definition of NamedataId-- = the definition of IddataSrcSpan-- = the definition of SrcSpandataType-- = the definition of SrcSpandataUnboundVar-- = the definition of UnboundVarnoLoc::SrcSpannoLoc= undefined -- or be an empty SrcSpan-- ------------------------------------------------ AST Base-- ----------------------------------------------dataLocated a =LSrcSpan a

typeLExp x =Located(Exp x)dataExp x
  =Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(LExp x)|App(XApp x)(LExp x)(LExp x)|Par(XPar x)(LExp x)|New(XNew x)typefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x

typefamilyXId  x

-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------dataPhase=Ps|Rn|TcdataGHC(p ::Phase)typeinstanceXVar(GHC_)=()typeinstanceXAbs(GHC_)=()typeinstanceXApp(GHCPs)=()typeinstanceXApp(GHCRn)=()typeinstanceXApp(GHCTc)=TypetypeinstanceXPar(GHC_)=()typeinstanceXNew(GHCPs)=VoidtypeinstanceXNew(GHCRn)=UnboundVartypeinstanceXNew(GHCTc)=UnboundVartypeinstanceXId(GHCPs)=RdrNametypeinstanceXId(GHCRn)=NametypeinstanceXId(GHCTc)=IdtypeExpPs=Exp(GHCPs)typeExpRn=Exp(GHCRn)typeExpTc=Exp(GHCTc)typeLExpPs=LExp(GHCPs)typeLExpRn=LExp(GHCRn)typeLExpTc=LExp(GHCTc)-- ------------------------------------------------ Example Function-- ----------------------------------------------par::LExp(GHC x)->LExp(GHC x)par l@(L sp (App{}))=L sp (Par() l)par l                = l
```

## Solutions


The key solution is to move source locations to the extension points, remove the indirection (e.g., the wrapper datatype `LExp`) altogether, and update the related code (e.g., functions over `Exp`) accordingly. 
There are a couple of ways to implement such a solution:

1. Using a typeclass to set/get source locations
  **SLPJ**: Explain `ForallX`.  I hate it because it implies that we'll pass massive dictionaries around; and what happens if we have lots of different data types, not just one?

1. We can nest extension typefamilies to be able to say that all constructors have the same uniform decorations (e.g., `SrcSpan`) beside their specific ones. This is just for convenience as `ForallX*` constraint quantifications can simulate the same (see the code for solution A).

> **SLPJ** It's more than just convenience; it's much more elegant than passing these huge dictionaries.  Show the code; something like
>
> ```
> typeinstanceXVar(Ghc p)=Located(XVarGhc p)typefamilyXVarGhc p whereXVarGhcPs=()XVarGhcRn=...XVarGhcTc=..
> ```
>
>
> It's quite nice that we get a *closed* type family for the GHC extensions. Now a typical function might look like
>
> ```
> getLoc::Located x ->SrcSpan-- As nowrnExpr(Var exts id)= setSrcSpan (getLoc exts)$do{...}
> ```
>
>
> ...etc...

1. We can extend (using TTG) each datatype to add a wrapper \*constructor\*, similar in spirit to the current `Located`.

1. The API Annotations are similar to the `SrcSpan`, in that they are additional decorations, and also currently appear wherever there is a `SrcSpan`.

1. We also currently have sections of AST without source locations, such as those generated when converting TH AST to hsSyn AST, or for GHC derived code.

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

1. TODO (add your suggestions)

### Solution A - Example Code


In the code below, as compared to the original one above, we have the following key changes:

- `LExp` is replaced with `Exp`
- field extensions are set to have a `SrcSpan` (instead of `()`)
- a setter/getter typeclass `HasSpan` (and instances) is introduced
- a pattern synonym for `L` is introduced using the typeclass

```
{-# OPTIONS_GHC -Wall
                -fno-warn-unticked-promoted-constructors
#-}{-# LANGUAGE TypeFamilies
           , DataKinds
           , ConstraintKinds
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , PatternSynonyms
           , ViewPatterns
#-}moduleSolutionAwhereimportGHC.Exts(Constraint)importData.VoiddataRdrName-- = the definition of RdrNamedataName-- = the definition of NamedataId-- = the definition of IddataSrcSpan-- = the definition of SrcSpandataType-- = the definition of SrcSpandataUnboundVar-- = the definition of UnboundVarnoLoc::SrcSpannoLoc= undefined -- or be an empty SrcSpan-- ------------------------------------------------ AST Base-- ----------------------------------------------dataExp x
  =Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(Exp x)|App(XApp x)(Exp x)(Exp x)|Par(XPar x)(Exp x)|New(XNew x)typefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x

typefamilyXId  x

-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------dataPhase=Ps|Rn|TcdataGHC(p ::Phase)typeinstanceXVar(GHC_)=SrcSpantypeinstanceXAbs(GHC_)=SrcSpantypeinstanceXApp(GHCPs)=SrcSpantypeinstanceXApp(GHCRn)=SrcSpantypeinstanceXApp(GHCTc)=(SrcSpan,Type)typeinstanceXPar(GHC_)=SrcSpantypeinstanceXNew(GHCPs)=VoidtypeinstanceXNew(GHCRn)=(SrcSpan,UnboundVar)typeinstanceXNew(GHCTc)=(SrcSpan,UnboundVar)typeinstanceXId(GHCPs)=RdrNametypeinstanceXId(GHCRn)=NametypeinstanceXId(GHCTc)=IdtypeExpPs=Exp(GHCPs)typeExpRn=Exp(GHCRn)typeExpTc=Exp(GHCTc)-- ------------------------------------------------ HasSpan Typeclass and L Pattern Synonym-- ----------------------------------------------classHasSpan a where
  getSpan :: a ->SrcSpan
  setSpan :: a ->SrcSpan-> a

instanceHasSpanSrcSpanwhere
  getSpan   = id
  setSpan _= id

instanceHasSpanVoidwhere
  getSpan x   = absurd x
  setSpan x _= absurd x

typeForallX(p ::*->Constraint) x
  =( p (XVar x), p (XAbs x), p (XApp x), p (XPar x), p (XNew x))instanceForallXHasSpan x =>HasSpan(Exp x)where
  getSpan (Var ex _)= getSpan ex
  getSpan (Abs ex __)= getSpan ex
  getSpan (App ex __)= getSpan ex
  getSpan (Par ex _)= getSpan ex
  getSpan (New ex)= getSpan ex

  setSpan (Var ex x)   sp =Var(setSpan ex sp) x
  setSpan (Abs ex x n) sp =Abs(setSpan ex sp) x n
  setSpan (App ex l m) sp =App(setSpan ex sp) l m
  setSpan (Par ex m)   sp =Par(setSpan ex sp) m
  setSpan (New ex)     sp =New(setSpan ex sp)getSpan'::HasSpan a => a ->(SrcSpan, a)getSpan' m =(getSpan m , m)patternL::HasSpan a =>SrcSpan-> a -> a
patternL s m <-(getSpan' ->(s , m))whereL s m =  setSpan m s

-- ------------------------------------------------ Example Function-- ----------------------------------------------par::ForallXHasSpan(GHC p)=>Exp(GHC p)->Exp(GHC p)par l@(L sp (App{}))=L sp (Par noLoc l)par l                = l
```

### Solution B - Example Code


In the code below, as compared to the original one above, we have the following key changes:

- `LExp` is replaced with `Exp`
- field extensions are set to have a `SrcSpan` paired with a closed type family specialised for GHC phases
- a setter/getter function pair is introduced
- a pattern synonym for `L` is introduced using the setter/getter function

```
{-# OPTIONS_GHC -Wall
                -fno-warn-unticked-promoted-constructors
#-}{-# LANGUAGE TypeFamilies
           , ConstraintKinds
           , ViewPatterns
           , PatternSynonyms
           , DataKinds
           , FlexibleInstances
#-}moduleSolutionBwhereimportData.VoiddataRdrName-- = the definition of RdrNamedataName-- = the definition of NamedataId-- = the definition of IddataSrcSpan-- = the definition of SrcSpandataType-- = the definition of SrcSpandataUnboundVar-- = the definition of UnboundVarnoLoc::SrcSpannoLoc= undefined -- or be an empty SrcSpan-- ------------------------------------------------ AST Base-- ----------------------------------------------dataExp x
  =Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(Exp x)|App(XApp x)(Exp x)(Exp x)|Par(XPar x)(Exp x)|New(XNew x)typefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x

typefamilyXId  x

-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------dataPhase=Ps|Rn|TcdataGHC(p ::Phase)typeinstanceXVar(GHC p)=(SrcSpan,XVarGHC p)typeinstanceXAbs(GHC p)=(SrcSpan,XAbsGHC p)typeinstanceXApp(GHC p)=(SrcSpan,XAppGHC p)typeinstanceXPar(GHC p)=(SrcSpan,XParGHC p)typeinstanceXNew(GHC p)=(SrcSpan,XNewGHC p)typeinstanceXId(GHC p)=XIdGHC p


typefamilyXVarGHC(p ::Phase)whereXVarGHC_=()typefamilyXAbsGHC(p ::Phase)whereXAbsGHC_=()typefamilyXAppGHC(p ::Phase)whereXAppGHCPs=()XAppGHCRn=()XAppGHCTc=TypetypefamilyXParGHC(p ::Phase)whereXParGHC_=()typefamilyXNewGHC(p ::Phase)whereXNewGHCPs=VoidXNewGHC_=UnboundVartypefamilyXIdGHC(p ::Phase)whereXIdGHCPs=RdrNameXIdGHCRn=NameXIdGHCTc=IdtypeExpPs=Exp(GHCPs)typeExpRn=Exp(GHCRn)typeExpTc=Exp(GHCTc)-- ------------------------------------------------ getter/setter of Span--  (similar to methods of HasSpan)-- ----------------------------------------------classHasSpan a where
  getSpan :: a ->SrcSpan
  setSpan :: a ->SrcSpan-> a

instanceHasSpanSrcSpanwhere
  getSpan   = id
  setSpan _= id

instanceHasSpanVoidwhere
  getSpan x   = absurd x
  setSpan x _= absurd x

instanceHasSpan(Exp(GHC p))where
  getSpan (Var ex _)= fst ex
  getSpan (Abs ex __)= fst ex
  getSpan (App ex __)= fst ex
  getSpan (Par ex _)= fst ex
  getSpan (New ex)= fst ex

  setSpan (Var ex x)   sp =Var(setFst ex sp) x
  setSpan (Abs ex x n) sp =Abs(setFst ex sp) x n
  setSpan (App ex l m) sp =App(setFst ex sp) l m
  setSpan (Par ex m)   sp =Par(setFst ex sp) m
  setSpan (New ex)     sp =New(setFst ex sp)setFst::(a , b)-> a ->(a , b)setFst(_, b) a' =(a' , b)getSpan'::HasSpan a => a ->(SrcSpan, a)getSpan' m =(getSpan m , m)patternL::HasSpan a =>SrcSpan-> a -> a
patternL s m <-(getSpan' ->(s , m))whereL s m =  setSpan m s

-- ------------------------------------------------ Example Function-- ----------------------------------------------par::Exp(GHC p)->Exp(GHC p)par l@(L sp (App{}))=L sp (Par(noLoc,()) l)par l                = l
```

### Solution C - Example Code


In the code below, as compared to the original one above, we have the following key changes:

- `LExp` is replaced with `Exp`
- a new constructor extension is introduced to wrap `ExpPs` with a `SrcSpan`
- a pattern synonym for `L` is introduced

```
{-# OPTIONS_GHC -Wall
                -fno-warn-unticked-promoted-constructors
#-}{-# LANGUAGE TypeFamilies
           , DataKinds
           , ConstraintKinds
           , PatternSynonyms
#-}moduleSolutionCwhereimportData.VoiddataRdrName-- = the definition of RdrNamedataName-- = the definition of NamedataId-- = the definition of IddataSrcSpan-- = the definition of SrcSpandataType-- = the definition of SrcSpandataUnboundVar-- = the definition of UnboundVarnoLoc::SrcSpannoLoc= undefined -- or be an empty SrcSpan-- ------------------------------------------------ AST Base-- ----------------------------------------------dataExp x
  =Var(XVar x)(XId x)|Abs(XAbs x)(XId x)(Exp x)|App(XApp x)(Exp x)(Exp x)|Par(XPar x)(Exp x)|New(XNew x)typefamilyXVar x
typefamilyXAbs x
typefamilyXApp x
typefamilyXPar x
typefamilyXNew x

typefamilyXId  x

-- ------------------------------------------------ GHC-Specific Decorations-- ----------------------------------------------dataPhase=Ps|Rn|TcdataGHC(p ::Phase)typeinstanceXVar(GHC_)=()typeinstanceXAbs(GHC_)=()typeinstanceXApp(GHCPs)=()typeinstanceXApp(GHCRn)=()typeinstanceXApp(GHCTc)=TypetypeinstanceXPar(GHC_)=()typeinstanceXNew(GHC p)=Either(SrcSpan,Exp(GHC p))(XNewGHC p)typefamilyXNewGHC(p ::Phase)whereXNewGHCPs=VoidXNewGHCRn=UnboundVarXNewGHCTc=UnboundVartypeinstanceXId(GHCPs)=RdrNametypeinstanceXId(GHCRn)=NametypeinstanceXId(GHCTc)=IdtypeExpPs=Exp(GHCPs)typeExpRn=Exp(GHCRn)typeExpTc=Exp(GHCTc)-- ------------------------------------------------ L Pattern Synonym-- ----------------------------------------------patternL::SrcSpan->Exp(GHC p)->Exp(GHC p)patternL sp m =New(Left(sp , m))-- ------------------------------------------------ Example Function-- ----------------------------------------------par::Exp(GHC p)->Exp(GHC p)par l@(L sp (App{}))=L sp (Par() l)par l                = l
```