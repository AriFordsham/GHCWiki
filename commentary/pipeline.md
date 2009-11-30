
Video: [ Compilation Pipeline](http://video.google.com/videoplay?docid=-4326420154219711812) and interface files (17'30")

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