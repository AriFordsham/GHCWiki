
Video: [ Compilation Pipeline](http://video.google.com/videoplay?docid=-4326420154219711812) and interface files (17'30")

# The compilation pipeline


When GHC compiles a module, it calls other programs, and generates a series of intermediate files.  Here's a summary of the process.
(source reference: `ghc/compiler/main/DriverPipeline.hs`)


We start with `Foo.hs` or `Foo.lhs`, the "l" specifing whether literate style is being used.

- Run the **unlit pre-processor**, `unlit`, to remove the literate markup, generating `Foo.lpp`.  The `unlit` processor is a C program kept in `utils/unlit`.

- Run the **C preprocessor**, `cpp`, (if `-cpp` is specified), generating `Foo.cpp` or `Foo.hspp` respectively.

- Run **the compiler itself**. This does not start a separate process; it's just a call to a Haskell function.  This step always generates an ['interface file'](commentary/compiler/iface-files)`Foo.hi`, and depending on what flags you give, it also generates a compiled file:

  - Assembly code: flag `-S`, file `Foo.s`
  - C code: flag `-fviaC`, file `Foo.hc`
  - C-- mode: flag `-fcmm`, file `Foo.cmm`, believed not to work

- In the `-fviaC` case, run the **C compiler**, followed by the [Evil Mangler](commentary/evil-mangler), generating `Foo.s`

- If `-fsplit-objs` is in force, run the **splitter** on `Foo.s`.  This splits `Foo.s` into lots of small files, `Foo/Foo1.s`, `Foo/Foo2.s`, etc.  The idea is that the static linker will thereby avoid linking dead code.

- Run the assembler on `Foo.s` or, if `-fsplit-objs` in in force, on each individual assembly file.
