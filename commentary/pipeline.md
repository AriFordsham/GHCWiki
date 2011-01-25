
Video: [ Compilation Pipeline](http://video.google.com/videoplay?docid=-4326420154219711812) and interface files (17'30")

# Overview


GHC is structured into two parts:

- The `ghc` package (in subdirectory `compiler`), which implements almost all GHC's functionality. It is an ordinary Haskell library, and can be imported into a Haskell program by saying `import GHC`.
- The `ghc` binary (in subdirectory `ghc`) which implements imports the `ghc` package, and implements the I/O for the `ghci` interactive loop.


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
 |        GhcMake         |    |    InteractiveEval     |
 | Implements --make      |    | Stuff to support the   |
 | Deals with compiling   |    | GHCi interactive envt  |
 |    multiple modules    |    |                        |
 |------------------------|    |------------------------|
           |                                |
           |                                |
           |      --------------------      |
- - - - - -| - - -|     GhcMonad     |- - - | - - - - - - - -
           |      --------------------      |
           |                                |
           |                                |
 |-------------------------|                |
 |   DriverPipeline        |                |
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
         |                    HscMain                   |
         | Compiling a single module (or expression or  |
         | stmt) to bytecode, or to a M.hc or M.s file  |
         |----------------------------------------------|
              |      |       |         |       |
            Parse Rename Typecheck Optimise CodeGen
```

# The compilation pipeline


When GHC compiles a module, it calls other programs, and generates a series of intermediate files.  Here's a summary of the process.
(source reference: [compiler/main/DriverPipeline.hs](/trac/ghc/browser/ghc/compiler/main/DriverPipeline.hs))


We start with `Foo.hs` or `Foo.lhs`, the "l" specifing whether literate style is being used.

- Run the **unlit pre-processor**, `unlit`, to remove the literate markup, generating `Foo.lpp`.  The `unlit` processor is a C program kept in [utils/unlit](/trac/ghc/browser/ghc/utils/unlit).

- Run the **C preprocessor**, `cpp`, (if `-cpp` is specified), generating `Foo.hspp`.

- Run **the compiler itself**. This does not start a separate process; it's just a call to a Haskell function.  This step always generates an ['interface file'](commentary/compiler/iface-files)`Foo.hi`, and depending on what flags you give, it also generates a compiled file:

  - Assembly code: flag `-S`, file `Foo.s`
  - C code: flag `-fvia-C`, file `Foo.hc`
  - C-- mode: flag `-fcmm`, file `Foo.cmm`, believed not to work

- In the `-fvia-C` case:

  - Run the **C compiler** on `Foo.hc`, to generate `Foo.raw_s`.
  - Run the [Evil Mangler](commentary/evil-mangler), generating `Foo.s`

- If `-split-objs` is in force, run the **splitter** on `Foo.s`.  This splits `Foo.s` into lots of small files.  The idea is that the static linker will thereby avoid linking dead code.

- Run the assembler on `Foo.s`, or if `-split-objs` is in force, on each individual assembly file.

# The compiler pipeline


The **compiler itself**, independent of the external tools, is also structured as a pipeline.  For details (and a diagram), see [Commentary/Compiler/HscMain](commentary/compiler/hsc-main)