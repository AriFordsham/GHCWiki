# Overview


GHC is structured into two parts:

- The `ghc` package (in subdirectory `compiler`), which implements almost all GHC's functionality. It is an ordinary Haskell library, and can be imported into a Haskell program by saying `import GHC`.
- The `ghc` binary (in subdirectory `ghc`) which imports the `ghc` package, and implements the I/O for the `ghci` interactive loop.


Here's an overview of the module structure of the top levels of GHC library.   (Note: more precisly, this is the plan. Currently the module `Make` below is glommed into the giant module `GHC`.)

```wiki
          |---------------------------------|
          |              GHC                |
          | The root module for the GHC API |
          | Very little code;               |
          | just simple wrappers            |
          |---------------------------------|
                     /                \
                    /                  \
                   /                    \
 |------------------------|    |------------------------|
 |    GHC.Driver.Make     |    |   GHC.Runtime.Eval     |
 | Implements --make      |    | Stuff to support the   |
 | Deals with compiling   |    | GHCi interactive envt  |
 |    multiple modules    |    |                        |
 |------------------------|    |------------------------|
           |                                |
           |                                |
           |      --------------------      |
- - - - - -| - - -| GHC.Driver.Monad |- - - | - - - - - - - -
           |      --------------------      |
           |                                |
           |                                |
 |-------------------------|                |
 |  GHC.Driver.Pipeline    |                |
 | Deals with compiling    |                |
 |  *a single module*      |                |
 | through all its stages  |                |
 | (cpp, unlit, compile,   |                |
 |  assemble, link etc)    |                |
 |-------------------------|                |
              \                             |
               \                            |
                \                           |  
         |----------------------------------------------|
         |               GHC.Driver.Main                |
         | Compiling a single module (or expression or  |
         | stmt) to bytecode, or to a M.hc or M.s file  |
         |----------------------------------------------|
              |      |       |         |       |
            Parse Rename Typecheck Optimise CodeGen
```


There are some important functions if you are tracing how things get from GHC to GHC.Driver.Main (formerly known as HscMain).

- `compileOne` is the compilation entry point for `--make` mode (it's invoked by `upsweep_mod` in GHC.Driver.Make).  It calls `hscIncrementalCompile`, and then fires up the `GHC.Driver.Pipeline` to finish up code generation.

- `runPhase` is the compilation entry point for `-c` mode. It successfully processes files until we have an `Hsc` input file, at which point it calls `hscIncrementalCompile`. The rest of the pipeline is handled automatically by the driver.

- `hscIncrementalCompile` is the primary entrypoint for `GHC.Driver.Main`.  It calls `hscIncrementalFrontend`, and if typechecking was necessary, it also runs the simplifier and desugarer, and writes out the interface file.

- `hscIncrementalFrontend` is the recompilation checker: it checks if we actually need to compile the file in question; if so it calls `genericHscFrontend` to actually parse and typecheck. (Note that this does NOT do any backend stuff: that will be handled by `hscIncrementalCompile`.)

# The driver pipeline


The driver pipeline consist of a couple of phases that call other programs and generate a series of intermediate files. Code responsible for managing the order of phases is in [compiler/GHC/Driver/Phases.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Phases.hs), while managing the driver pipeline as a whole is coded in [compiler/GHC/Driver/Pipeline.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Pipeline.hs). Note that driver pipeline is not the same thing as compilation pipeline: the latter is part of the former.


Let's take a look at the overall structure of the driver pipeline. When we compile `Foo.hs` or `Foo.lhs` ("lhs" extension means that Literate Haskell is being used) the following phases are being called (some of them depending on additional conditions like file extensions or enabled flags):

- Run the **unlit pre-processor**, `unlit`, to remove the literate markup, generating `Foo.lpp`.  The `unlit` processor is a C program kept in [utils/unlit](https://gitlab.haskell.org/ghc/ghc/blob/master/utils/unlit).

- Run the **C preprocessor**, `cpp`, (if `-cpp` is specified), generating `Foo.hspp`.

- Run **the compiler itself**. This does not start a separate process; it's just a call to a Haskell function.  This step always generates an ['interface file'](commentary/compiler/iface-files) `Foo.hi`, and depending on what flags you give, it also generates a compiled file. As GHC supports three backend code generators currently (a native code generator, a C code generator and an llvm code generator) the possible range of outputs depends on the backend used. All three support assembly output:

  - Object code: no flags required, file `Foo.o` (supported by all three backends)
  - Assembly code: flag `-S`, file `Foo.s` (supported by all three backends)
  - C code: flags `-C`, file `Foo.hc` (only supported by C backend)

- In the `-fvia-C` case: (This case is **outdated**.)

  - Run the **C compiler** on `Foo.hc`, to generate `Foo.s`.

- If `-split-objs` is in force, run the **splitter** on `Foo.s`.  This splits `Foo.s` into lots of small files.  The idea is that the static linker will thereby avoid linking dead code.

- Run the assembler on `Foo.s`, or if `-split-objs` is in force, on each individual assembly file.

# The compiler pipeline


The **compiler itself**, independent of the external tools, is also structured as a pipeline.  For details (and a diagram), see [Commentary/Compiler/HscMain](commentary/compiler/hsc-main)

# Video


Video of compilation pipeline explanation from 2006: [Compilation Pipeline](http://www.youtube.com/watch?v=dzSc8ACz_mw&list=PLBkRCigjPwyeCSD_DFxpd246YIF7_RDDI) and interface files (17'30")
