Motivation
==========

This is an ongoing effort to make GHC more modular.

* Make the pipeline more reusable.
    * For historical reasons, the current codebase assumes a straight pipeline from Haskell files to Cmm.
    * Clash and IHC forks after Core, GHCJS forks after Stg, Asterius forks after Cmm, etc. We could imagine other languages targeting Core, Stg or Cmm. They often can't use GHC-API from a stock GHC and rely on a forked GHC instead.
    * I would like to have an interactive frontend that can explore different optimizations order, etc. Currently GHC-API doesn't make it simple/possible to do.
    * A Unison-like frontend for GHC would be very cool but it would break many assumptions about file organization, packages, etc.

* Test independent parts of the compiler with "modern" tools (QuickCheck, etc.).
    * Compare IR expressions instead of IR dumps: faster, avoid fragile output canonicalization
    * Make the testsuite faster: don't execute the whole pipeline
    * Catch more bugs/regressions maybe
    * Provide Criterion reports (checked in CI?)

* Make GHC multi-sessions, multi-targets, multi-threaded...
    * It will be easier with constrained side-effects and a more modular code-base

Principles
==========

1. Make the different stages of the pipeline usable independently from each other
2. Avoid shared mutable state and uncontrolled side-effects
3. Clearly separate GHC-the-program from GHC-API

Tasks
=====

Make the pipeline apparent in the module hierarchy (#13009)
-----------------------------------------------------------

We would like to introduce the following module prefixes:

![ghc_modules.svg](uploads/db1b2a8fa23301b897a3e6f16dcefbab/ghc_modules.svg)

The boxes are the GHC.XXX top-level module prefixes. For example, the box `Hs` stands for a module `GHC.Hs` and a collection of sub-module (e.g. `GHC.Hs.Pat`, `GHC.Hs.Expr` etc)

The arrows indicate allowed dependencies between groups of modules.

Principles:
* Consider top-level modules GHC.XXX as if they were from independent packages
* Put together modules defining, transforming and analyzing each IR: GHC.{Hs,Core,Stg,Cmm,Iface,Llvm,etc.}
* Put each IR-to-IR compiler under top-level "GHC.Ir1ToIr2" (e.g. GHC.StgToCmm)

Goals:
* Group all the GHC modules under the GHC prefix (common convention for Haskell packages, least surprise)
* Ensure that each IR can be used independently from the others
* Improve the generated Haddocks (table of contents reflecting the hierarchy)

### Proposed renaming

* `GHC.Hs` (done; can still be modified):

```
GHC.Hs.Binds
GHC.Hs.Decls
GHC.Hs.Doc
GHC.Hs.Dump
GHC.Hs.Expr
GHC.Hs.Extension
GHC.Hs
GHC.Hs.ImpExp
GHC.Hs.Instances
GHC.Hs.Lit
GHC.Hs.Pat
GHC.Hs.PlaceHolder
GHC.Hs.Types
GHC.Hs.Utils
```

* `GHC.HsToCore` (done; can still be modified):
```
GHC.HsToCore.PmCheck
GHC.HsToCore.PmCheck.Oracle
GHC.HsToCore.PmCheck.Ppr
GHC.HsToCore.PmCheck.Types
```

* `GHC.Platform` (done; can still be modified):
```
GHC.Platform.ARM64
GHC.Platform.ARM
GHC.Platform.NoRegs
GHC.Platform.PPC
GHC.Platform.Regs
GHC.Platform.SPARC
GHC.Platform.X86_64
GHC.Platform.X86
```
* `GHC.StgToCmm` (done; can still be modified):
```
GHC.StgToCmm.ArgRep
GHC.StgToCmm.Bind
GHC.StgToCmm.CgUtils
GHC.StgToCmm.Closure
GHC.StgToCmm.DataCon
GHC.StgToCmm.Env
GHC.StgToCmm.Expr
GHC.StgToCmm.ExtCode
GHC.StgToCmm.Foreign
GHC.StgToCmm.Heap
GHC.StgToCmm.Hpc
GHC.StgToCmm
GHC.StgToCmm/Layout
GHC.StgToCmm/Monad
GHC.StgToCmm/Prim
GHC.StgToCmm/Prof
GHC.StgToCmm/Ticky
GHC.StgToCmm/Utils
```

* `GHC.ThToHs` (done; can still be modified):
```
GHC.ThToHs
```

* Renamer !1899:


```
GHC.Rename.Binds <= rename/RnBinds.hs
GHC.Rename.Env <= rename/RnEnv.hs
GHC.Rename.Expr <= rename/RnExpr.hs
GHC.Rename.Fixity <= rename/RnFixity.hs
GHC.Rename.Doc <= rename/RnHsDoc.hs
GHC.Rename.Names <= rename/RnNames.hs
GHC.Rename.Pat <= rename/RnPat.hs
GHC.Rename.Source <= rename/RnSource.hs
GHC.Rename.Splice <= rename/RnSplice.hs
GHC.Rename.Types <= rename/RnTypes.hs
GHC.Rename.Unbound <= rename/RnUnbound.hs
GHC.Rename.Utils <= rename/RnUtils.hs
```

* `GHC.Core` !1773:

```
GHC.Core.Arity <= coreSyn/CoreArity.hs
GHC.Core.FVs <= coreSyn/CoreFVs.hs
GHC.Core.Lint <= coreSyn/CoreLint.hs
GHC.Core.Map <= coreSyn/CoreMap.hs
GHC.Core.SimpleOpt <= coreSyn/CoreOpt.hs
GHC.Core.Seq <= coreSyn/CoreSeq.hs
GHC.Core.Stats <= coreSyn/CoreStats.hs
GHC.Core.Subst <= coreSyn/CoreSubst.hs
GHC.Core.Syntax <= coreSyn/CoreSyn.hs
GHC.Core.Tidy <= coreSyn/CoreTidy.hs
GHC.Core.Unfold <= coreSyn/CoreUnfold.hs
GHC.Core.Utils <= coreSyn/CoreUtils.hs
GHC.Core.Make <= coreSyn/MkCore.hs
GHC.Core.Pretty <= coreSyn/PprCore.hs
GHC.Core.OccurAnal <= simplCore/OccurAnal.hs
GHC.Core.Rules <= specialise/Rules.hs

GHC.Core.Class <= types/Class.hs
GHC.Core.CoAxiom <= types/CoAxiom.hs
GHC.Core.Coercion <= types/Coercion.hs
GHC.Core.FamInstEnv <= types/FamInstEnv.hs
GHC.Core.InstEnv <= types/InstEnv.hs
GHC.Core.Kind <= types/Kind.hs
GHC.Core.OptCoercion <= types/OptCoercion.hs
GHC.Core.TyCoFVs <= types/TyCoFVs.hs
GHC.Core.TyCon <= types/TyCon.hs
GHC.Core.TyCoPpr <= types/TyCoPpr.hs
GHC.Core.TyCoRep <= types/TyCoRep.hs
GHC.Core.TyCoSubst <= types/TyCoSubst.hs
GHC.Core.TyCoTidy <= types/TyCoTidy.hs
GHC.Core.Type <= types/Type.hs
GHC.Core.Unify <= types/Unify.hs
GHC.Core.DataCon <= basicTypes/DataCon.hs
```

* `GHC.CoreToCore` !1773
```
GHC.CoreToCore.CallArity <= simplCore/CallArity.hs
GHC.CoreToCore.Monad <= simplCore/CoreMonad.hs
GHC.CoreToCore.CSE <= simplCore/CSE.hs
GHC.CoreToCore.Exitify <= simplCore/Exitify.hs
GHC.CoreToCore.FloatIn <= simplCore/FloatIn.hs
GHC.CoreToCore.FloatOut <= simplCore/FloatOut.hs
GHC.CoreToCore.LiberateCase <= simplCore/LiberateCase.hs
GHC.CoreToCore.StaticArgs <= simplCore/SAT.hs
GHC.CoreToCore.SetLevels <= simplCore/SetLevels.hs
GHC.CoreToCore.Simplify.Driver <= simplCore/SimplCore.hs
GHC.CoreToCore.Simplify.Env <= simplCore/SimplEnv.hs
GHC.CoreToCore.Simplify <= simplCore/Simplify.hs
GHC.CoreToCore.Simplify.Monad <= simplCore/SimplMonad.hs
GHC.CoreToCore.Simplify.Utils <= simplCore/SimplUtils.hs
GHC.CoreToCore.SpecConstr <= specialise/SpecConstr.hs
GHC.CoreToCore.Specialise <= specialise/Specialise.hs
GHC.CoreToCore.DmdAnal <= stranal/DmdAnal.hs
GHC.CoreToCore.WorkWrap <= stranal/WorkWrap.hs
GHC.CoreToCore.WorkWrapLib <= stranal/WwLib.hs
GHC.CoreToCore.ConstantFold <= prelude/PrelRules.hs
```

* `GHC.BasicTypes`:
```
GHC.BasicTypes.Avail <= basicTypes/Avail.hs
GHC.BasicTypes <= basicTypes/BasicTypes.hs
GHC.BasicTypes.Demand <= basicTypes/Demand.hs
GHC.BasicTypes.FieldLabal <= basicTypes/FieldLabel.hs
GHC.BasicTypes.Id <= basicTypes/Id.hs
GHC.BasicTypes.IdInfo <= basicTypes/IdInfo.hs
GHC.BasicTypes.Lexeme <= basicTypes/Lexeme.hs
GHC.BasicTypes.Literal <= basicTypes/Literal.hs
GHC.BasicTypes.MkId <= basicTypes/MkId.hs
GHC.BasicTypes.Module <= basicTypes/Module.hs
GHC.BasicTypes.NameCache <= basicTypes/NameCache.hs
GHC.BasicTypes.NameEnv <= basicTypes/NameEnv.hs
GHC.BasicTypes.Name <= basicTypes/Name.hs
GHC.BasicTypes.NameSet <= basicTypes/NameSet.hs
GHC.BasicTypes.OccName <= basicTypes/OccName.hs
GHC.BasicTypes.PatSyn <= basicTypes/PatSyn.hs
GHC.BasicTypes.RdrName <= basicTypes/RdrName.hs
GHC.BasicTypes.SrcLoc <= basicTypes/SrcLoc.hs
GHC.BasicTypes.UniqSupply <= basicTypes/UniqSupply.hs
GHC.BasicTypes.Unique <= basicTypes/Unique.hs
GHC.BasicTypes.VarEnv <= basicTypes/VarEnv.hs
GHC.BasicTypes.Var <= basicTypes/Var.hs
GHC.BasicTypes.VarSet <= basicTypes/VarSet.hs
GHC.BasicTypes.NameShape <= backpack/NameShape.hs
```


* `GHC.Cmm`

```
GHC.Cmm.BlockId <= cmm/BlockId.hs
GHC.Cmm.CLabel <= cmm/CLabel.hs
GHC.Cmm.BuildInfoTables <= cmm/CmmBuildInfoTables.hs
GHC.Cmm.CallConv <= cmm/CmmCallConv.hs
GHC.Cmm.CommonBlockElim <= cmm/CmmCommonBlockElim.hs
GHC.Cmm.ContFlowOpt <= cmm/CmmContFlowOpt.hs
GHC.Cmm.Expr <= cmm/CmmExpr.hs
GHC.Cmm <= cmm/Cmm.hs
GHC.Cmm.Switch.Implement <= cmm/CmmImplementSwitchPlans.hs
GHC.Cmm.Info <= cmm/CmmInfo.hs
GHC.Cmm.LayoutStack <= cmm/CmmLayoutStack.hs
GHC.Cmm.Lint <= cmm/CmmLint.hs
GHC.Cmm.Liveness <= cmm/CmmLive.hs
GHC.Cmm.MachOp <= cmm/CmmMachOp.hs
GHC.Cmm.Monad <= cmm/CmmMonad.hs
GHC.Cmm.Node <= cmm/CmmNode.hs
GHC.Cmm.Opt <= cmm/CmmOpt.hs
GHC.Cmm.Pipeline <= cmm/CmmPipeline.hs
GHC.Cmm.ProcPoint <= cmm/CmmProcPoint.hs
GHC.Cmm.Sink <= cmm/CmmSink.hs
GHC.Cmm.Switch <= cmm/CmmSwitch.hs
GHC.Cmm.Type <= cmm/CmmType.hs
GHC.Cmm.Utils <= cmm/CmmUtils.hs
GHC.Cmm.DebugBlock <= cmm/Debug.hs
GHC.Cmm.Dataflow.Block <= cmm/Hoopl/Block.hs
GHC.Cmm.Dataflow.Collections <= cmm/Hoopl/Collections.hs
GHC.Cmm.Dataflow <= cmm/Hoopl/Dataflow.hs
GHC.Cmm.Dataflow.Graph <= cmm/Hoopl/Graph.hs
GHC.Cmm.Dataflow.Label <= cmm/Hoopl/Label.hs
GHC.Cmm.Graph <= cmm/MkGraph.hs
GHC.Cmm.Pretty.Decl <= cmm/PprCmmDecl.hs
GHC.Cmm.Pretty.Expr <= cmm/PprCmmExpr.hs
GHC.Cmm.Pretty <= cmm/PprCmm.hs

GHC.CmmToC <= cmm/PprC.hs
```

* `GHC.Tc`

```
GHC.Tc.Deriv <= typecheck/TcDeriv.hs
GHC.Tc.Deriv.Constraints <= typecheck/TcDerivInfer.hs
GHC.Tc.Deriv.Utils <= typecheck/TcDerivUtils.hs
GHC.Tc.Deriv.BasicClasses <= typecheck/TcGenDeriv.hs
GHC.Tc.Deriv.Functor <= typecheck/TcGenFunctor.hs
GHC.Tc.Deriv.Generics <= typecheck/TcGenGenerics.hs
GHC.Tc.Deriv.Typeable <= typecheck/TcTypeable.hs

GHC.Tc.Solver.Interact <= typecheck/TcInteract.hs
GHC.Tc.Solver.Monad <= typecheck/TcSMonad.hs
GHC.Tc.Solver.Canonical <= typecheck/TcCanonical.hs
GHC.Tc.Solver.Flatten <= typecheck/TcFlatten.hs

GHC.Tc.Utils <= typecheck/TcRnTypes.hs
GHC.Tc.Utils.Monadic <= typecheck/TcMType.hs
GHC.Tc.Utils.CoreType <= typecheck/TcType.hs

GHC.Tc.ClsInst <= typecheck/ClsInst.hs
GHC.Tc.FamInst <= typecheck/FamInst.hs
GHC.Tc.FunDeps <= typecheck/FunDeps.hs
GHC.Tc.Inst <= typecheck/Inst.hs
GHC.Tc.Annotations <= typecheck/TcAnnotations.hs
GHC.Tc.Arrows <= typecheck/TcArrows.hs
GHC.Tc.Backpack <= typecheck/TcBackpack.hs
GHC.Tc.Binds <= typecheck/TcBinds.hs
GHC.Tc.ClassDcl <= typecheck/TcClassDcl.hs
GHC.Tc.Defaults <= typecheck/TcDefaults.hs
GHC.Tc.Env <= typecheck/TcEnv.hs
GHC.Tc.Errors <= typecheck/TcErrors.hs
GHC.Tc.Evidence <= typecheck/TcEvidence.hs
GHC.Tc.EvTerm <= typecheck/TcEvTerm.hs
GHC.Tc.Expr <= typecheck/TcExpr.hs
GHC.Tc.Foreign <= typecheck/TcForeign.hs
GHC.Tc.Hole.Errors <= typecheck/TcHoleErrors.hs
GHC.Tc.Hole.FitTypes <= typecheck/TcHoleFitTypes.hs
GHC.Tc.Syntax <= typecheck/TcHsSyn.hs
GHC.Tc.Type <= typecheck/TcHsType.hs
GHC.Tc.InstDcls <= typecheck/TcInstDcls.hs
GHC.Tc.Match <= typecheck/TcMatches.hs
GHC.Tc.Pat <= typecheck/TcPat.hs
GHC.Tc.PatSyn <= typecheck/TcPatSyn.hs
GHC.Tc.Module <= typecheck/TcRnDriver.hs
GHC.Tc.Export <= typecheck/TcRnExports.hs
GHC.Tc.Monad <= typecheck/TcRnMonad.hs
GHC.Tc.Rule <= typecheck/TcRules.hs
GHC.Tc.Sigs <= typecheck/TcSigs.hs
GHC.Tc.Simplify <= typecheck/TcSimplify.hs
GHC.Tc.Splice <= typecheck/TcSplice.hs
GHC.Tc.TyClsDecls <= typecheck/TcTyClsDecls.hs
GHC.Tc.TyDecls <= typecheck/TcTyDecls.hs
GHC.Tc.Unify <= typecheck/TcUnify.hs
GHC.Tc.Validity <= typecheck/TcValidity.hs
```

* `GHC.HsToCore`

```
GHC.HsToCore <= deSugar/Desugar.hs
GHC.HsToCore.Coverage <= deSugar/Coverage.hs
GHC.HsToCore.Arrows <= deSugar/DsArrows.hs
GHC.HsToCore.Binds <= deSugar/DsBinds.hs
GHC.HsToCore.ForeignCall <= deSugar/DsCCall.hs
GHC.HsToCore.Expr <= deSugar/DsExpr.hs
GHC.HsToCore.ForeignCallDecl <= deSugar/DsForeign.hs
GHC.HsToCore.GuardedRHSs <= deSugar/DsGRHSs.hs
GHC.HsToCore.ListComp <= deSugar/DsListComp.hs
GHC.HsToCore.Splice <= deSugar/DsMeta.hs
GHC.HsToCore.Monad <= deSugar/DsMonad.hs
GHC.HsToCore.Usage <= deSugar/DsUsage.hs
GHC.HsToCore.Utils <= deSugar/DsUtils.hs
GHC.HsToCore.Docs <= deSugar/ExtractDocs.hs
GHC.HsToCore.Match <= deSugar/Match.hs
GHC.HsToCore.Match.Constructor <= deSugar/MatchCon.hs
GHC.HsToCore.Match.Literal <= deSugar/MatchLit.hs
```

* Stg

```
GHC.Stg.Pipeline <= simplStg/SimplStg.hs
GHC.Stg.CSE <= simplStg/StgCse.hs
GHC.Stg.LiftLams.Analysis <= simplStg/StgLiftLams/Analysis.hs
GHC.Stg.LiftLams <= simplStg/StgLiftLams.hs
GHC.Stg.LiftLams.Monad <= simplStg/StgLiftLams/LiftM.hs
GHC.Stg.LiftLams.Trans <= simplStg/StgLiftLams/Transformation.hs
GHC.Stg.Stats <= simplStg/StgStats.hs
GHC.Stg.Unarise <= simplStg/UnariseStg.hs
GHC.Stg.FVs <= stgSyn/StgFVs.hs
GHC.Stg.Lint <= stgSyn/StgLint.hs
GHC.Stg.Subst <= stgSyn/StgSubst.hs
GHC.Stg.Syntax <= stgSyn/StgSyn.hs

GHC.CoreToStg <= stgSyn/CoreToStg.hs
```

* Builtin

```
GHC.Builtin.Uniques <= prelude/KnownUniques.hs
GHC.Builtin.Utils <= prelude/PrelInfo.hs
GHC.Builtin.Names <= prelude/PrelNames.hs
GHC.Builtin.Names.TH <= prelude/THNames.hs
GHC.Builtin.PrimOps <= prelude/PrimOp.hs
GHC.Builtin.PrimTypes <= prelude/TysPrim.hs
GHC.Builtin.Types <= prelude/TysWiredIn.hs
GHC.Builtin.Types.Nats <= typecheck/TcTypeNats.hs
```

* `GHC.Interface`

```
GHC.Interface.Binary <= iface/BinIface.hs
GHC.Interface.BuildTyCl <= iface/BuildTyCl.hs
GHC.Interface.Env <= iface/IfaceEnv.hs
GHC.Interface.Syntax <= iface/IfaceSyn.hs
GHC.Interface.Type <= iface/IfaceType.hs
GHC.Interface.Load <= iface/LoadIface.hs
GHC.Interface.Utils <= iface/MkIface.hs
GHC.Interface.Tc <= iface/TcIface.hs
GHC.Interface.Rename <= backpack/RnModIface.hs
GHC.Interface.Tidy <= main/TidyPgm.hs

GHC.CoreToInterface <= iface/ToIface.hs
```
* `GHC.ByteCode`:

```
GHC.ByteCode.Asm <= ghci/ByteCodeAsm.hs
GHC.ByteCode.Instr <= ghci/ByteCodeInstr.hs
GHC.ByteCode.InfoTable <= ghci/ByteCodeItbls.hs
GHC.ByteCode.Linker <= ghci/ByteCodeLink.hs
GHC.ByteCode.Types <= ghci/ByteCodeTypes.hs

GHC.CoreToByteCode <= ghci/ByteCodeGen.hs
```

* `GHC.Llvm`:

```
GHC.Llvm.MetaData <= llvmGen/Llvm/MetaData.hs
GHC.Llvm.Pretty <= llvmGen/Llvm/PpLlvm.hs
GHC.Llvm.Types <= llvmGen/Llvm/Types.hs
GHC.Llvm.Syntax <= llvmGen/Llvm/AbsSyn.hs
GHC.Llvm <= llvmGen/Llvm.hs

GHC.CmmToLlvm <= llvmGen/LlvmCodeGen.hs
GHC.CmmToLlvm.Mangler <= llvmGen/LlvmMangler.hs
GHC.CmmToLlvm.Base <= llvmGen/LlvmCodeGen/Base.hs
GHC.CmmToLlvm.CodeGen <= llvmGen/LlvmCodeGen/CodeGen.hs
GHC.CmmToLlvm.Data <= llvmGen/LlvmCodeGen/Data.hs
GHC.CmmToLlvm.Pretty <= llvmGen/LlvmCodeGen/Ppr.hs
GHC.CmmToLlvm.Regs <= llvmGen/LlvmCodeGen/Regs.hs
```

* `GHC.Runtime`:

```
GHC.Runtime.ClosureInspect <= ghci/RtClosureInspect.hs
GHC.Runtime.Debugger <= ghci/Debugger.hs
GHC.Runtime.Interpreter <= ghci/GHCi.hs
GHC.Runtime.Linker <= ghci/Linker.hs
GHC.Runtime.Linker.Types <= ghci/LinkerTypes.hs
GHC.Runtime.DynamicLoading <= main/DynamicLoading.hs
GHC.Runtime.Eval <= main/InteractiveEval.hs
GHC.Runtime.Eval.Types <= main/InteractiveEvalTypes.hs
```

* `GHC.Driver`:

```
GHC.Driver.Backpack.Syntax <= backpack/BkpSyn.hs
GHC.Driver.Backpack <= backpack/DriverBkp.hs
GHC.Driver.CmdLine <= main/CmdLineParser.hs
GHC.Driver.CodeOutput <= main/CodeOutput.hs
GHC.Driver.Finder <= main/Finder.hs
GHC.Driver.MakeDepend <= main/DriverMkDepend.hs
GHC.Driver.Phases <= main/DriverPhases.hs
GHC.Driver.Pipeline <= main/DriverPipeline.hs
GHC.Driver.PipelineMonad <= main/PipelineMonad.hs
GHC.Driver.Main <= main/HscMain.hs
GHC.Driver.Make <= main/GhcMake.hs
```

* `GHC.CmmToAsm`:

```
GHC.CmmToAsm.CodeGen <= nativeGen/AsmCodeGen.hs
GHC.CmmToAsm.Blocklayout <= nativeGen/BlockLayout.hs
GHC.CmmToAsm.CFG <= nativeGen/CFG.hs
GHC.CmmToAsm.Prim <= nativeGen/CPrim.hs
GHC.CmmToAsm.Dwarf.Constants <= nativeGen/Dwarf/Constants.hs
GHC.CmmToAsm.Dwarf <= nativeGen/Dwarf.hs
GHC.CmmToAsm.Dwarf.Types <= nativeGen/Dwarf/Types.hs
GHC.CmmToAsm.Format <= nativeGen/Format.hs
GHC.CmmToAsm.Instr <= nativeGen/Instruction.hs
GHC.CmmToAsm.Monad <= nativeGen/NCGMonad.hs
GHC.CmmToAsm.PIC <= nativeGen/PIC.hs

GHC.CmmToAsm.Pretty <= nativeGen/PprBase.hs

GHC.CmmToAsm.Reg.Graph.ArchBase <= nativeGen/RegAlloc/Graph/ArchBase.hs
GHC.CmmToAsm.Reg.Graph.ArchX86 <= nativeGen/RegAlloc/Graph/ArchX86.hs
GHC.CmmToAsm.Reg.Graph.Coalesce <= nativeGen/RegAlloc/Graph/Coalesce.hs
GHC.CmmToAsm.Reg.Graph <= nativeGen/RegAlloc/Graph/Main.hs
GHC.CmmToAsm.Reg.Graph.SpillClean <= nativeGen/RegAlloc/Graph/SpillClean.hs
GHC.CmmToAsm.Reg.Graph.SpillCost <= nativeGen/RegAlloc/Graph/SpillCost.hs
GHC.CmmToAsm.Reg.Graph.Spill <= nativeGen/RegAlloc/Graph/Spill.hs
GHC.CmmToAsm.Reg.Graph.Stats <= nativeGen/RegAlloc/Graph/Stats.hs
GHC.CmmToAsm.Reg.Graph.TrivColorable <= nativeGen/RegAlloc/Graph/TrivColorable.hs

GHC.CmmToAsm.Reg.Linear.Base <= nativeGen/RegAlloc/Linear/Base.hs
GHC.CmmToAsm.Reg.Linear.JoinToTargets <= nativeGen/RegAlloc/Linear/JoinToTargets.hs
GHC.CmmToAsm.Reg.Linear <= nativeGen/RegAlloc/Linear/Main.hs
GHC.CmmToAsm.Reg.Linear.StackMap <= nativeGen/RegAlloc/Linear/StackMap.hs
GHC.CmmToAsm.Reg.Linear.State <= nativeGen/RegAlloc/Linear/State.hs
GHC.CmmToAsm.Reg.Linear.Stats <= nativeGen/RegAlloc/Linear/Stats.hs
GHC.CmmToAsm.Reg.Linear.FreeRegs <= nativeGen/RegAlloc/Linear/FreeRegs.hs
GHC.CmmToAsm.Reg.Linear.FreeRegs.PPC <= nativeGen/RegAlloc/Linear/PPC/FreeRegs.hs
GHC.CmmToAsm.Reg.Linear.FreeRegs.SPARC <= nativeGen/RegAlloc/Linear/SPARC/FreeRegs.hs
GHC.CmmToAsm.Reg.Linear.FreeRegs.X86_64 <= nativeGen/RegAlloc/Linear/X86_64/FreeRegs.hs
GHC.CmmToAsm.Reg.Linear.FreeRegs.X86 <= nativeGen/RegAlloc/Linear/X86/FreeRegs.hs

GHC.CmmToAsm.Reg <= nativeGen/Reg.hs
GHC.CmmToAsm.Reg.Liveness <= nativeGen/RegAlloc/Liveness.hs
GHC.CmmToAsm.Reg.Class <= nativeGen/RegClass.hs
GHC.CmmToAsm.Reg.Target <= nativeGen/TargetReg.hs

GHC.CmmToAsm.PPC.CodeGen <= nativeGen/PPC/CodeGen.hs
GHC.CmmToAsm.PPC.Cond <= nativeGen/PPC/Cond.hs
GHC.CmmToAsm.PPC.Instr <= nativeGen/PPC/Instr.hs
GHC.CmmToAsm.PPC.Pretty <= nativeGen/PPC/Ppr.hs
GHC.CmmToAsm.PPC.RegInfo <= nativeGen/PPC/RegInfo.hs
GHC.CmmToAsm.PPC.Regs <= nativeGen/PPC/Regs.hs

GHC.CmmToAsm.SPARC.AddrMode <= nativeGen/SPARC/AddrMode.hs
GHC.CmmToAsm.SPARC.Base <= nativeGen/SPARC/Base.hs
GHC.CmmToAsm.SPARC.CodeGen.Amode <= nativeGen/SPARC/CodeGen/Amode.hs
GHC.CmmToAsm.SPARC.CodeGen.Base <= nativeGen/SPARC/CodeGen/Base.hs
GHC.CmmToAsm.SPARC.CodeGen.Cond <= nativeGen/SPARC/CodeGen/CondCode.hs
GHC.CmmToAsm.SPARC.CodeGen.Expand <= nativeGen/SPARC/CodeGen/Expand.hs
GHC.CmmToAsm.SPARC.CodeGen.Gen32 <= nativeGen/SPARC/CodeGen/Gen32.hs
GHC.CmmToAsm.SPARC.CodeGen.Gen64 <= nativeGen/SPARC/CodeGen/Gen64.hs
GHC.CmmToAsm.SPARC.CodeGen <= nativeGen/SPARC/CodeGen.hs
GHC.CmmToAsm.SPARC.CodeGen.Sanity <= nativeGen/SPARC/CodeGen/Sanity.hs
GHC.CmmToAsm.SPARC.Cond <= nativeGen/SPARC/Cond.hs
GHC.CmmToAsm.SPARC.Imm <= nativeGen/SPARC/Imm.hs
GHC.CmmToAsm.SPARC.Instr <= nativeGen/SPARC/Instr.hs
GHC.CmmToAsm.SPARC.Pretty <= nativeGen/SPARC/Ppr.hs
GHC.CmmToAsm.SPARC.Regs <= nativeGen/SPARC/Regs.hs
GHC.CmmToAsm.SPARC.ShortcutJump <= nativeGen/SPARC/ShortcutJump.hs
GHC.CmmToAsm.SPARC.Stack <= nativeGen/SPARC/Stack.hs

GHC.CmmToAsm.X86 <= nativeGen/X86/CodeGen.hs
GHC.CmmToAsm.X86.Cond <= nativeGen/X86/Cond.hs
GHC.CmmToAsm.X86.Inst <= nativeGen/X86/Instr.hs
GHC.CmmToAsm.X86.Pretty <= nativeGen/X86/Ppr.hs
GHC.CmmToAsm.X86.RegInfo <= nativeGen/X86/RegInfo.hs
GHC.CmmToAsm.X86.Regs <= nativeGen/X86/Regs.hs
```

* `GHC.Data`:

```
GHC.Data.Bitmap <= cmm/Bitmap.hs
GHC.Data.Bag <= utils/Bag.hs
GHC.Data.BooleanFormula <= utils/BooleanFormula.hs
GHC.Data.EnumSet <= utils/EnumSet.hs
GHC.Data.FastMutInt <= utils/FastMutInt.hs
GHC.Data.FastStringEnv <= utils/FastStringEnv.hs
GHC.Data.FastString <= utils/FastString.hs
GHC.Data.Graph.Base <= utils/GraphBase.hs
GHC.Data.Graph.Color <= utils/GraphColor.hs
GHC.Data.Graph.Directed <= utils/Digraph.hs
GHC.Data.Graph.Ops <= utils/GraphOps.hs
GHC.Data.Graph.Pretty <= utils/GraphPpr.hs
GHC.Data.Graph.UnVar <= utils/UnVarGraph.hs
GHC.Data.List.SetOps <= utils/ListSetOps.hs
GHC.Data.Maybe <= utils/Maybes.hs
GHC.Data.FiniteMap <= utils/FiniteMap.hs
GHC.Data.OrdList <= utils/OrdList.hs
GHC.Data.Pair <= utils/Pair.hs
GHC.Data.Stream <= utils/Stream.hs
GHC.Data.StringBuffer <= utils/StringBuffer.hs
GHC.Data.TrieMap <= utils/TrieMap.hs

GHC.Data.Char.Encoding <= utils/Encoding.hs
GHC.Data.Char.Class <= parser/Ctype.hs

GHC.Data.Unique.DFM <= utils/UniqDFM.hs
GHC.Data.Unique.DSet <= utils/UniqDSet.hs
GHC.Data.Unique.FM <= utils/UniqFM.hs
GHC.Data.Unique.Map <= utils/UniqMap.hs
GHC.Data.Unique.Set <= utils/UniqSet.hs
```
* `GHC.Utils`:

```
GHC.Utils.Asm <= utils/AsmUtils.hs
GHC.Utils.Binary <= utils/Binary.hs
GHC.Utils.BufWrite <= utils/BufWrite.hs
GHC.Utils.Exception <= utils/Exception.hs
GHC.Utils.IO.Unsafe <= utils/FastFunctions.hs
GHC.Utils.Prelude <= utils/GhcPrelude.hs
GHC.Utils.Monad.IOEnv <= utils/IOEnv.hs
GHC.Utils.Json <= utils/Json.hs
GHC.Utils.Monad.Utils <= utils/MonadUtils.hs
GHC.Utils.Outputable <= utils/Outputable.hs
GHC.Utils.Panic <= utils/Panic.hs
GHC.Utils.PlainPanic <= utils/PlainPanic.hs
GHC.Utils.Pretty.Colour <= utils/PprColour.hs
GHC.Utils.pretty <= utils/Pretty.hs
GHC.Utils.Monad.State <= utils/State.hs
GHC.Utils.Misc <= utils/Util.hs
```

* `GHC.SysTools`:

```
GHC.SysTools.BaseDir <= main/SysTools/BaseDir.hs
GHC.SysTools.ExtraObj <= main/SysTools/ExtraObj.hs
GHC.SysTools <= main/SysTools.hs
GHC.SysTools.Info <= main/SysTools/Info.hs
GHC.SysTools.Process <= main/SysTools/Process.hs
GHC.SysTools.Settings <= main/SysTools/Settings.hs
GHC.SysTools.Tasks <= main/SysTools/Tasks.hs
GHC.SysTools.Terminal <= main/SysTools/Terminal.hs
```

* TODO:

```
GHC.RTS.Storage <= cmm/SMRep.hs
GHC.CoreToStg.Prep <= coreSyn/CorePrep.hs
GHC.BasicTypes.ConLike <= basicTypes/ConLike.hs
GHC.Plugin.TypeChecker <= typecheck/TcPluginM.hs
GHC.Config.Flags.FingerPrint <= utils/Fingerprint.hs
? <= iface/FlagChecker.hs
? <= iface/BinFingerprint.hs
? <= hieFile/HieAst.hs
? <= hieFile/HieBin.hs
? <= hieFile/HieDebug.hs
? <= hieFile/HieTypes.hs
? <= hieFile/HieUtils.hs
? <= main/Annotations.hs
? <= main/Ar.hs
? <= main/CliOption.hs
? <= main/Constants.hs
? <= main/DynFlags.hs
? <= main/Elf.hs
? <= main/ErrUtils.hs
? <= main/FileCleanup.hs
? <= main/FileSettings.hs
? <= main/GHC.hs
? <= main/GhcMonad.hs
? <= main/GhcNameVersion.hs
? <= main/GhcPlugins.hs
? <= main/HeaderInfo.hs
? <= main/Hooks.hs
? <= main/HscStats.hs
? <= main/HscTypes.hs
? <= main/PackageConfig.hs
? <= main/Packages.hs
? <= main/PlatformConstants.hs
? <= main/Plugins.hs
? <= main/PprTyThing.hs
? <= main/Settings.hs
? <= main/StaticPtrTable.hs
? <= main/ToolSettings.hs
? <= parser/ApiAnnotation.hs
? <= parser/HaddockUtils.hs
? <= parser/RdrHsSyn.hs
? <= prelude/ForeignCall.hs
? <= profiling/CostCentre.hs
? <= profiling/CostCentreState.hs
? <= profiling/ProfInit.hs
? <= simplStg/RepType.hs
? <= utils/FV.hs
```


Reduce the dependencies on DynFlags
-----------------------------------

DynFlags is a huge (mutable) datatype that is passed to many functions in the compiler. We can make the transition smoother by defining the following kind of classes and instances:

```
-- in GHC.StgToCmm.Options
class HasOptions a where
   stgToCmmXXX :: Bool
   ...

data Options = Options
   { _stgToCmmXXX :: Bool
   , ...
   }

stgToCmm :: HasOptions a => a -> Stg -> Cmm

-- in GHC.Driver.Options
instance StgToCmm.HasOptions DynFlags where
   ...

```


Make the compiler as pure as possible
-------------------------------------

* IR-to-IR should callback into the driving code to perform IO (e.g. when they need an interface to be loaded). They can return a continuation to resume their work.
* Don't assume that a filesystem is present
    * Abstract file loading (i.e. make the Finder configurable, like Java's class loaders).
    * Abstract error reporting and logging (i.e. pluggable Logger)
    * It will make life easier for IDEs and other frontends.
* Support multi-sessions (#10827)
