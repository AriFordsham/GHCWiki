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

Proposed hierarchy:
```
-- IRs
GHC.Hs
GHC.Core
GHC.Stg
GHC.Cmm
GHC.Iface
GHC.Bytecode
GHC.Llvm

-- Compilers, fron one IR to another
GHC.Rename      -- Note that both the renamer and the typechecker work on the Hs representation
GHC.Typecheck   -- but with a different type index (GhcPs, GhcRn, GhcTc)
GHC.HsToCore
GHC.CoreToStg
GHC.StgToCmm
GHC.CmmToAsm
GHC.CmmToC
GHC.CmmToLlvm
GHC.CoreToIface
GHC.CoreToBytecode

GHC.Driver: pipeline driver (Backpack, Finder, Main, Make, MakeDepend, etc.)
GHC.Program: command-line parsing, GHCi UI, etc.

-- shared stuff
GHC.Builtins: primops, etc.
GHC.Data: data structures (Bag, Graph, FiniteMap, EnumSet, etc.)
GHC.Common (Id, Name, etc.)
GHC.{Runtime/Interactive}: interactive evaluation stuff (Debugger, Eval, etc.)
GHC.Plugin
GHC.Utils: SysTools, IO stuff, Outputable, etc.
GHC.Platform: platform description (register mapping, word-size, etc.)
GHC.Config: Constants, DynFlags, etc.
```

### Actual renaming

* Done (can still be modified):

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
GHC.HsToCore.PmCheck
GHC.HsToCore.PmCheck.Oracle
GHC.HsToCore.PmCheck.Ppr
GHC.HsToCore.PmCheck.Types
GHC.Hs.Types
GHC.Hs.Utils
GHC.Platform.ARM64
GHC.Platform.ARM
GHC.Platform.NoRegs
GHC.Platform.PPC
GHC.Platform.Regs
GHC.Platform.SPARC
GHC.Platform.X86_64
GHC.Platform.X86
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

* Core !1773:

```
GHC.Core.Arity <= coreSyn/CoreArity.hs
GHC.Core.FVs <= coreSyn/CoreFVs.hs
GHC.Core.Lint <= coreSyn/CoreLint.hs
GHC.Core.Map <= coreSyn/CoreMap.hs
GHC.Core.SimpleOpt <= coreSyn/CoreOpt.hs
GHC.Core.Seq <= coreSyn/CoreSeq.hs
GHC.Core.Stats <= coreSyn/CoreStats.hs
GHC.Core.Subst <= coreSyn/CoreSubst.hs
GHC.Core <= coreSyn/CoreSyn.hs
GHC.Core.Tidy <= coreSyn/CoreTidy.hs
GHC.Core.Unfold <= coreSyn/CoreUnfold.hs
GHC.Core.Utils <= coreSyn/CoreUtils.hs
GHC.Core.Make <= coreSyn/MkCore.hs
GHC.Core.Pretty <= coreSyn/PprCore.hs
GHC.Core.OccurAnal <= simplCore/OccurAnal.hs
GHC.Core.Rules <= specialise/Rules.hs

GHC.CoreToCore.CallArity <= simplCore/CallArity.hs
GHC.CoreToCore.Monad <= simplCore/CoreMonad.hs
GHC.CoreToCore.CSE <= simplCore/CSE.hs
GHC.CoreToCore.Exitify <= simplCore/Exitify.hs
GHC.CoreToCore.FloatIn <= simplCore/FloatIn.hs
GHC.CoreToCore.FloatOut <= simplCore/FloatOut.hs
GHC.CoreToCore.LiberateCase <= simplCore/LiberateCase.hs
GHC.CoreToCore.StaticArgs <= simplCore/SAT.hs
GHC.CoreToCore.SetLevels <= simplCore/SetLevels.hs
GHC.CoreToCore.Simplifier <= simplCore/SimplCore.hs
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
GHC.Core.ConLike <= basicTypes/ConLike.hs
GHC.Core.DataCon <= basicTypes/DataCon.hs
```

Cmm:

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

* TODO:

```
GHC.Data.Bitmap <= cmm/Bitmap.hs
GHC.RTS.Storage <= cmm/SMRep.hs
GHC.CoreToStg.Prep <= coreSyn/CorePrep.hs
? <= backpack/BkpSyn.hs
? <= backpack/DriverBkp.hs
? <= backpack/NameShape.hs
? <= backpack/RnModIface.hs
? <= basicTypes/Avail.hs
? <= basicTypes/BasicTypes.hs
? <= basicTypes/Demand.hs
? <= basicTypes/FieldLabel.hs
? <= basicTypes/Id.hs
? <= basicTypes/IdInfo.hs
? <= basicTypes/Lexeme.hs
? <= basicTypes/Literal.hs
? <= basicTypes/MkId.hs
? <= basicTypes/Module.hs
? <= basicTypes/NameCache.hs
? <= basicTypes/NameEnv.hs
? <= basicTypes/Name.hs
? <= basicTypes/NameSet.hs
? <= basicTypes/OccName.hs
? <= basicTypes/PatSyn.hs
? <= basicTypes/RdrName.hs
? <= basicTypes/SrcLoc.hs
? <= basicTypes/UniqSupply.hs
? <= basicTypes/Unique.hs
? <= basicTypes/VarEnv.hs
? <= basicTypes/Var.hs
? <= basicTypes/VarSet.hs
? <= deSugar/Coverage.hs
? <= deSugar/Desugar.hs
? <= deSugar/DsArrows.hs
? <= deSugar/DsBinds.hs
? <= deSugar/DsCCall.hs
? <= deSugar/DsExpr.hs
? <= deSugar/DsForeign.hs
? <= deSugar/DsGRHSs.hs
? <= deSugar/DsListComp.hs
? <= deSugar/DsMeta.hs
? <= deSugar/DsMonad.hs
? <= deSugar/DsUsage.hs
? <= deSugar/DsUtils.hs
? <= deSugar/ExtractDocs.hs
? <= deSugar/MatchCon.hs
? <= deSugar/Match.hs
? <= deSugar/MatchLit.hs
? <= ghci/ByteCodeAsm.hs
? <= ghci/ByteCodeGen.hs
? <= ghci/ByteCodeInstr.hs
? <= ghci/ByteCodeItbls.hs
? <= ghci/ByteCodeLink.hs
? <= ghci/ByteCodeTypes.hs
? <= ghci/Debugger.hs
? <= ghci/GHCi.hs
? <= ghci/Linker.hs
? <= ghci/LinkerTypes.hs
? <= ghci/RtClosureInspect.hs
? <= hieFile/HieAst.hs
? <= hieFile/HieBin.hs
? <= hieFile/HieDebug.hs
? <= hieFile/HieTypes.hs
? <= hieFile/HieUtils.hs
? <= iface/BinFingerprint.hs
? <= iface/BinIface.hs
? <= iface/BuildTyCl.hs
? <= iface/FlagChecker.hs
? <= iface/IfaceEnv.hs
? <= iface/IfaceSyn.hs
? <= iface/IfaceType.hs
? <= iface/LoadIface.hs
? <= iface/MkIface.hs
? <= iface/TcIface.hs
? <= iface/ToIface.hs
? <= llvmGen/Llvm/AbsSyn.hs
? <= llvmGen/LlvmCodeGen/Base.hs
? <= llvmGen/LlvmCodeGen/CodeGen.hs
? <= llvmGen/LlvmCodeGen/Data.hs
? <= llvmGen/LlvmCodeGen.hs
? <= llvmGen/LlvmCodeGen/Ppr.hs
? <= llvmGen/LlvmCodeGen/Regs.hs
? <= llvmGen/Llvm.hs
? <= llvmGen/LlvmMangler.hs
? <= llvmGen/Llvm/MetaData.hs
? <= llvmGen/Llvm/PpLlvm.hs
? <= llvmGen/Llvm/Types.hs
? <= main/Annotations.hs
? <= main/Ar.hs
? <= main/CliOption.hs
? <= main/CmdLineParser.hs
? <= main/CodeOutput.hs
? <= main/Constants.hs
? <= main/DriverMkDepend.hs
? <= main/DriverPhases.hs
? <= main/DriverPipeline.hs
? <= main/DynamicLoading.hs
? <= main/DynFlags.hs
? <= main/Elf.hs
? <= main/ErrUtils.hs
? <= main/FileCleanup.hs
? <= main/FileSettings.hs
? <= main/Finder.hs
? <= main/GHC.hs
? <= main/GhcMake.hs
? <= main/GhcMonad.hs
? <= main/GhcNameVersion.hs
? <= main/GhcPlugins.hs
? <= main/HeaderInfo.hs
? <= main/Hooks.hs
? <= main/HscMain.hs
? <= main/HscStats.hs
? <= main/HscTypes.hs
? <= main/InteractiveEval.hs
? <= main/InteractiveEvalTypes.hs
? <= main/PackageConfig.hs
? <= main/Packages.hs
? <= main/PipelineMonad.hs
? <= main/PlatformConstants.hs
? <= main/Plugins.hs
? <= main/PprTyThing.hs
? <= main/Settings.hs
? <= main/StaticPtrTable.hs
? <= main/SysTools/BaseDir.hs
? <= main/SysTools/ExtraObj.hs
? <= main/SysTools.hs
? <= main/SysTools/Info.hs
? <= main/SysTools/Process.hs
? <= main/SysTools/Settings.hs
? <= main/SysTools/Tasks.hs
? <= main/SysTools/Terminal.hs
? <= main/TidyPgm.hs
? <= main/ToolSettings.hs
? <= nativeGen/AsmCodeGen.hs
? <= nativeGen/BlockLayout.hs
? <= nativeGen/CFG.hs
? <= nativeGen/CPrim.hs
? <= nativeGen/Dwarf/Constants.hs
? <= nativeGen/Dwarf.hs
? <= nativeGen/Dwarf/Types.hs
? <= nativeGen/Format.hs
? <= nativeGen/Instruction.hs
? <= nativeGen/NCGMonad.hs
? <= nativeGen/PIC.hs
? <= nativeGen/PPC/CodeGen.hs
? <= nativeGen/PPC/Cond.hs
? <= nativeGen/PPC/Instr.hs
? <= nativeGen/PPC/Ppr.hs
? <= nativeGen/PPC/RegInfo.hs
? <= nativeGen/PPC/Regs.hs
? <= nativeGen/PprBase.hs
? <= nativeGen/RegAlloc/Graph/ArchBase.hs
? <= nativeGen/RegAlloc/Graph/ArchX86.hs
? <= nativeGen/RegAlloc/Graph/Coalesce.hs
? <= nativeGen/RegAlloc/Graph/Main.hs
? <= nativeGen/RegAlloc/Graph/SpillClean.hs
? <= nativeGen/RegAlloc/Graph/SpillCost.hs
? <= nativeGen/RegAlloc/Graph/Spill.hs
? <= nativeGen/RegAlloc/Graph/Stats.hs
? <= nativeGen/RegAlloc/Graph/TrivColorable.hs
? <= nativeGen/RegAlloc/Linear/Base.hs
? <= nativeGen/RegAlloc/Linear/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/JoinToTargets.hs
? <= nativeGen/RegAlloc/Linear/Main.hs
? <= nativeGen/RegAlloc/Linear/PPC/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/SPARC/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/StackMap.hs
? <= nativeGen/RegAlloc/Linear/State.hs
? <= nativeGen/RegAlloc/Linear/Stats.hs
? <= nativeGen/RegAlloc/Linear/X86_64/FreeRegs.hs
? <= nativeGen/RegAlloc/Linear/X86/FreeRegs.hs
? <= nativeGen/RegAlloc/Liveness.hs
? <= nativeGen/RegClass.hs
? <= nativeGen/Reg.hs
? <= nativeGen/SPARC/AddrMode.hs
? <= nativeGen/SPARC/Base.hs
? <= nativeGen/SPARC/CodeGen/Amode.hs
? <= nativeGen/SPARC/CodeGen/Base.hs
? <= nativeGen/SPARC/CodeGen/CondCode.hs
? <= nativeGen/SPARC/CodeGen/Expand.hs
? <= nativeGen/SPARC/CodeGen/Gen32.hs
? <= nativeGen/SPARC/CodeGen/Gen64.hs
? <= nativeGen/SPARC/CodeGen.hs
? <= nativeGen/SPARC/CodeGen/Sanity.hs
? <= nativeGen/SPARC/Cond.hs
? <= nativeGen/SPARC/Imm.hs
? <= nativeGen/SPARC/Instr.hs
? <= nativeGen/SPARC/Ppr.hs
? <= nativeGen/SPARC/Regs.hs
? <= nativeGen/SPARC/ShortcutJump.hs
? <= nativeGen/SPARC/Stack.hs
? <= nativeGen/TargetReg.hs
? <= nativeGen/X86/CodeGen.hs
? <= nativeGen/X86/Cond.hs
? <= nativeGen/X86/Instr.hs
? <= nativeGen/X86/Ppr.hs
? <= nativeGen/X86/RegInfo.hs
? <= nativeGen/X86/Regs.hs
? <= parser/ApiAnnotation.hs
? <= parser/Ctype.hs
? <= parser/HaddockUtils.hs
? <= parser/RdrHsSyn.hs
? <= prelude/ForeignCall.hs
? <= prelude/KnownUniques.hs
? <= prelude/PrelInfo.hs
? <= prelude/PrelNames.hs
? <= prelude/PrimOp.hs
? <= prelude/THNames.hs
? <= prelude/TysPrim.hs
? <= prelude/TysWiredIn.hs
? <= profiling/CostCentre.hs
? <= profiling/CostCentreState.hs
? <= profiling/ProfInit.hs
? <= simplStg/RepType.hs
? <= simplStg/SimplStg.hs
? <= simplStg/StgCse.hs
? <= simplStg/StgLiftLams/Analysis.hs
? <= simplStg/StgLiftLams.hs
? <= simplStg/StgLiftLams/LiftM.hs
? <= simplStg/StgLiftLams/Transformation.hs
? <= simplStg/StgStats.hs
? <= simplStg/UnariseStg.hs
? <= stgSyn/CoreToStg.hs
? <= stgSyn/StgFVs.hs
? <= stgSyn/StgLint.hs
? <= stgSyn/StgSubst.hs
? <= stgSyn/StgSyn.hs
? <= typecheck/ClsInst.hs
? <= typecheck/FamInst.hs
? <= typecheck/FunDeps.hs
? <= typecheck/Inst.hs
? <= typecheck/TcAnnotations.hs
? <= typecheck/TcArrows.hs
? <= typecheck/TcBackpack.hs
? <= typecheck/TcBinds.hs
? <= typecheck/TcCanonical.hs
? <= typecheck/TcClassDcl.hs
? <= typecheck/TcDefaults.hs
? <= typecheck/TcDeriv.hs
? <= typecheck/TcDerivInfer.hs
? <= typecheck/TcDerivUtils.hs
? <= typecheck/TcEnv.hs
? <= typecheck/TcErrors.hs
? <= typecheck/TcEvidence.hs
? <= typecheck/TcEvTerm.hs
? <= typecheck/TcExpr.hs
? <= typecheck/TcFlatten.hs
? <= typecheck/TcForeign.hs
? <= typecheck/TcGenDeriv.hs
? <= typecheck/TcGenFunctor.hs
? <= typecheck/TcGenGenerics.hs
? <= typecheck/TcHoleErrors.hs
? <= typecheck/TcHoleFitTypes.hs
? <= typecheck/TcHsSyn.hs
? <= typecheck/TcHsType.hs
? <= typecheck/TcInstDcls.hs
? <= typecheck/TcInteract.hs
? <= typecheck/TcMatches.hs
? <= typecheck/TcMType.hs
? <= typecheck/TcPat.hs
? <= typecheck/TcPatSyn.hs
? <= typecheck/TcPluginM.hs
? <= typecheck/TcRnDriver.hs
? <= typecheck/TcRnExports.hs
? <= typecheck/TcRnMonad.hs
? <= typecheck/TcRnTypes.hs
? <= typecheck/TcRules.hs
? <= typecheck/TcSigs.hs
? <= typecheck/TcSimplify.hs
? <= typecheck/TcSMonad.hs
? <= typecheck/TcSplice.hs
? <= typecheck/TcTyClsDecls.hs
? <= typecheck/TcTyDecls.hs
? <= typecheck/TcTypeable.hs
? <= typecheck/TcType.hs
? <= typecheck/TcTypeNats.hs
? <= typecheck/TcUnify.hs
? <= typecheck/TcValidity.hs
? <= utils/AsmUtils.hs
? <= utils/Bag.hs
? <= utils/Binary.hs
? <= utils/BooleanFormula.hs
? <= utils/BufWrite.hs
? <= utils/Digraph.hs
? <= utils/Encoding.hs
? <= utils/EnumSet.hs
? <= utils/Exception.hs
? <= utils/FastFunctions.hs
? <= utils/FastMutInt.hs
? <= utils/FastStringEnv.hs
? <= utils/FastString.hs
? <= utils/Fingerprint.hs
? <= utils/FiniteMap.hs
? <= utils/FV.hs
? <= utils/GhcPrelude.hs
? <= utils/GraphBase.hs
? <= utils/GraphColor.hs
? <= utils/GraphOps.hs
? <= utils/GraphPpr.hs
? <= utils/IOEnv.hs
? <= utils/Json.hs
? <= utils/ListSetOps.hs
? <= utils/Maybes.hs
? <= utils/MonadUtils.hs
? <= utils/OrdList.hs
? <= utils/Outputable.hs
? <= utils/Pair.hs
? <= utils/Panic.hs
? <= utils/PlainPanic.hs
? <= utils/PprColour.hs
? <= utils/Pretty.hs
? <= utils/State.hs
? <= utils/Stream.hs
? <= utils/StringBuffer.hs
? <= utils/TrieMap.hs
? <= utils/UniqDFM.hs
? <= utils/UniqDSet.hs
? <= utils/UniqFM.hs
? <= utils/UniqMap.hs
? <= utils/UniqSet.hs
? <= utils/UnVarGraph.hs
? <= utils/Util.hs
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
