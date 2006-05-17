
The Marvellous Module Structure of GHC


GHC is built out of about 245 Haskell modules. It can be quite tricky to figure out what the module dependency graph looks like. It can be important, too, because loops in the module dependency graph need to be broken carefully using .hi-boot interface files.


This section of the commentary documents the subtlest part of the module dependency graph, namely the part near the bottom.

- The list is given in compilation order: that is, module near the top are more primitive, and are compiled earlier.
- Each module is listed together with its most critical dependencies in parentheses; that is, the dependencies that prevent it being compiled earlier.
- Modules in the same bullet don't depend on each other.
- Loops are documented by a dependency such as "loop Type.Type". This means tha the module imports Type.Type, but module Type has not yet been compiled, so the import comes from Type.hi-boot. 


Compilation order is as follows:

- First comes a layer of modules that have few interdependencies, and which implement very basic data types:

> > ** Util
> > ** OccName
> > ** Pretty
> > ** Outputable
> > ** StringBuffer
> > ** ListSetOps
> > ** Maybes
> > ** etc 

- Now comes the main subtle layer, involving types, classes, type constructors identifiers, expressions, rules, and their operations.

> > ** Name
> > **
> >
> > >
> > > PrimRep

> > ** PrelNames
> > **

> >
> > o Var (Name, loop IdInfo.IdInfo, loop Type.Type, loop Type.Kind)

> >
> > o VarEnv, VarSet, ThinAir

> >
> > o Class (loop TyCon.TyCon, loop Type.Type)

> >
> > o TyCon (loop Type.Type, loop Type.Kind, loop DataCon.DataCon, loop Generics.GenInfo)

> >
> > o TypeRep (loop DataCon.DataCon, loop Subst.substTyWith)

> >
> > o Type (loop PprType.pprType, loop Subst.substTyWith)

> >
> > o FieldLabel(Type)
> >
> > >
> > > TysPrim(Type)

> >
> > o Literal (TysPrim, PprType)
> >
> > >
> > > DataCon (loop PprType, loop Subst.substTyWith, FieldLabel.FieldLabel)

> >
> > o TysWiredIn (loop MkId.mkDataConIds)

> >
> > o TcType( lots of TysWiredIn stuff)

> >
> > o PprType( lots of TcType stuff )

> >
> > o PrimOp (PprType, TysWiredIn)

> >
> > o CoreSyn \[does not import Id\]

> >
> > o IdInfo (CoreSyn.Unfolding, CoreSyn.CoreRules)

> >
> > o Id (lots from IdInfo)

> >
> > o CoreFVs
> >
> > >
> > > PprCore

> >
> > o CoreUtils (PprCore.pprCoreExpr, CoreFVs.exprFreeVars, CoreSyn.isEvaldUnfolding CoreSyn.maybeUnfoldingTemplate)

> >
> > o CoreLint( CoreUtils )
> >
> > >
> > > OccurAnal (CoreUtils.exprIsTrivial)
> > > CoreTidy (CoreUtils.exprArity )

> >
> > o CoreUnfold (OccurAnal.occurAnalyseGlobalExpr)

> >
> > o Subst (CoreUnfold.Unfolding, CoreFVs)
> >
> > >
> > > Generics (CoreUnfold.mkTopUnfolding)
> > > Rules (CoreUnfold.Unfolding, PprCore.pprTidyIdRules)

> >
> > o MkId (CoreUnfold.mkUnfolding, Subst, Rules.addRule)

> >
> > o PrelInfo (MkId)
> >
> > >
> > > HscTypes( Rules.RuleBase ) 

- That is the end of the infrastructure. Now we get the main layer of mdoules that perform useful work.

> >
> > o CoreTidy (HscTypes.PersistentCompilerState) 


HsSyn stuff

- HsPat.hs-boot
- HsExpr.hs-boot (loop HsPat.LPat)
- HsTypes (loop HsExpr.HsSplice)
- HsBinds (HsTypes.LHsType, loop HsPat.LPat, HsExpr.pprFunBind and others) HsLit (HsTypes.SyntaxName)
- HsPat (HsBinds, HsLit) HsDecls (HsBinds)
- HsExpr (HsDecls, HsPat) 


Library stuff: base package

- GHC.Base
- Data.Tuple (GHC.Base), GHC.Ptr (GHC.Base)
- GHC.Enum (Data.Tuple)
- GHC.Show (GHC.Enum)
- GHC.Num (GHC.Show)
- GHC.ST (GHC.Num), GHC.Real (GHC.Num)
- GHC.Arr (GHC.ST) GHC.STRef (GHC.ST)
- GHC.IOBase (GHC.Arr)
- Data.Bits (GHC.Real)
- Data.HashTable (Data.Bits, Control.Monad)
- Data.Typeable (GHC.IOBase, Data.HashTable)
- GHC.Weak (Data.Typeable, GHC.IOBase) 
