
\[ Up: [Commentary](commentary) \]

# Compiling one module: HscMain


Here we are going to look at the compilation of a single module.
There is a picture that goes with this description, which appears at the bottom of this page, but you'll probably find it easier to open [this link](commentary/compiler/hsc-pipe) in another window, so you can see it at the same time as reading the text.


Look at the picture first.  The yellow boxes are compiler passes, while the blue stuff on the left gives the data type that moves from one phase to the next.  The entire pipeline for a single module is run by a module called HscMain (in GhcFile(compiler/main/HscMain)).  Here are the steps it goes through:

- The program is initially parsed into the `HsSyn` types (in the [compiler/hsSyn](/trac/ghc/browser/ghc/compiler/hsSyn) directory), a collection of data types that describe the full abstract syntax of Haskell.  `HsSyn` is a pretty big colleciton of types: there are 52 data types when I last counted.  Many are pretty trivial, but a few have a lot of constructors (`HsExpr` has 40).  `HsSyn` represents Haskell its full glory, complete with all syntactic sugar.

- `HsSyn` is parameterised over the types of the variables it contains.  The first three passes (the front end) of the compiler work like this:

  - The **parser** produces `HsSyn` parameterised by **[RdrName](commentary/compiler/rdr-name-type)**.  To a first approximation, a `RdrName` is just a string.
  - The **[renamer](commentary/compiler/renamer)** transforms this to `HsSyn` parameterised by **[Name](commentary/compiler/name-type)**.  To a first appoximation, a `Name` is a string plus a `Unique` (number) that uniquely identifies it.
  - The **typechecker** transforms this further, to `HsSyn` parameterised by **[Id](commentary/compiler/entity-types)**.  To a first approximation, an `Id` is a `Name` plus a type.

>
> In addition, the type-checker converts class declarations to `Class`es, and type declarations to `TyCon`s and `DataCon`s.  And of course, the type-checker deals in `Type`s and `TyVar`s. The [data types for these entities](commentary/compiler/entity-types) (`Type`, `TyCon`, `Class`, `Id`, `TyVar`) are pervasive throughout the rest of the compiler.

- The **desugarer** converts from the massive `HsSyn` type to [GHC's intermediate language, CoreSyn](commentary/compiler/core-syn-type).  This data type is relatively tiny: just eight constructors.

- The **SimplCore** pass ([simplCore/SimplCore.lhs](/trac/ghc/browser/ghc/simplCore/SimplCore.lhs)) is a bunch of Core-to-Core passes that optimise the program.  The main passes are:

  - The **Simplifier**, which applies lots of small, local optimisations to the program.  The simplifier is big and complicated, because it implements a *lot* of transformations; and tries to make them cascade nicely.
  - The **float-out** and **float-in** transformations, which move let-bindings outwards and inwards respectively.
  - The **strictness analyser**.  This actually comprises two passes: the **analayser** itself and the **worker/wrapper** transformation that uses the results of the analysis to transform the program.
  - The **liberate-case** transformation.
  - The **constructor-specialialisation** transformation.
  - The **common sub-expression eliminiation** (CSE) transformation.

- Then the **CoreTidy pass** gets the code into a form in which it can be imported into subsequent modules (when using `--make`) and/or put into an interface file.  There are good notes at the top of the file [compiler/main/TidyPgm.lhs](/trac/ghc/browser/ghc/compiler/main/TidyPgm.lhs); the main function is `tidyProgram`, for some reason documented as "Plan B".

- At this point, the data flow forks.  First, the tidied program is dumped into an interface file.  This part happens in two stages:

  - It is **converted to `IfaceSyn`** (defined in GhcFile(compiler/iface/IfaceSyn.lhs? and GhcFile(compiler/iface/IfaceType.lhs?).
  - The `IfaceSyn` is **serialised into a binary output file** ([iface/BinIface.lhs](/trac/ghc/browser/ghc/iface/BinIface.lhs)).

> >
> > The serialisation does (pretty much) nothing except serialise.  All the intelligence is in the Core-to-IfaceSyn conversion; or, rather, in the reverse of that step.

- The same, tidied Core program is now fed to the Back End.  First there is a two-stage conversion from `CoreSyn` to `StgSyn`.

  - The first step is called **CorePrep**, a Core-to-Core pass that puts the program into A-normal form (ANF).  In ANF, the argument of every application is a variable or literal; more complicated arguments are let-bound.  Actually CorePrep does quite a bit more: there is a detailed list at the top of the file [compiler/coreSyn/CorePrep.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CorePrep.lhs).
  - The second step, **CoreToStg**, moves to the `StgSyn` data type (the code is in \[GhcFile(stgSyn/CoreToStg.lhs)?\].  The output of CorePrep is carefully arranged to exactly match what `StgSyn` allows (notably ANF), so there is very little work to do. However, `StgSyn` is decorated with lots of redundant information (free variables, let-no-escape indicators), which is generated on-the-fly by `CoreToStg`.

- Next, the **code generator** converts the STG program to a `C--` program.  The code generator is a Big Mother, and lives in directory [compiler/codeGen](/trac/ghc/browser/ghc/compiler/codeGen)

- Now the path forks again:

  - If we are generating GHC's stylised C code, we can just pretty-print the `C--` code as stylised C ([compiler/cmm/PprC.hs)](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs))
  - If we are generating native code, we invoke the native code generator.  This is another Big Mother, and lives in [compiler/nativeGen](/trac/ghc/browser/ghc/compiler/nativeGen).

# The Diagram


This diagram is also located [here](commentary/compiler/hsc-pipe), so that you can open it in a separate window.

[](/trac/ghc/attachment/wiki/Commentary/Compiler/HscPipe/HscPipe.png)