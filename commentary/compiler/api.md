# GHC Commentary: The GHC API


This section of the commentary describes everything between [HscMain](commentary/compiler/hsc-main) and the front-end; that is, the parts of GHC that coordinate the compilation of multiple modules.

## Orgainsation of the top of GHC

[](/trac/ghc/attachment/wiki/Commentary/Compiler/API/ghc-top.png)


The GHC API is the interface exported by [compiler/main/GHC.hs](/trac/ghc/browser/ghc/compiler/main/GHC.hs).  To compile a Haskell module that uses the GHC API, use the flag `-package ghc` (in GHC 6.6 and later).  GHC itself contains a few front-ends:

- The "one-shot" mode, where GHC compiles each file on the command line separately (eg. `ghc -c Foo.hs`).  This mode
  is implemented directly on top of [HscMain](commentary/compiler/hsc-main), since it compiles only one file at a
  time.  In fact, this is all that GHC consisted of prior to version 5.00 when GHCi and `--make` were introduced.

- GHCi, the interactive environment, is implemented in [compiler/ghci/InteractiveUI.hs](/trac/ghc/browser/ghc/compiler/ghci/InteractiveUI.hs) and sits squarely on top
  of the GHC API.

- `--make` is almost a trivial client of the GHC API, and is implemented in [compiler/main/Main.hs](/trac/ghc/browser/ghc/compiler/main/Main.hs).

- `-M`, the Makefile dependency generator, is also a client of the GHC API and is implemented in 
  [compiler/main/DriverMkDepend.hs](/trac/ghc/browser/ghc/compiler/main/DriverMkDepend.hs).


Note that since GHC is packaged as a single binary, all of these front-ends are present, and there is a single command-line interface implemented in [compiler/main/Main.hs](/trac/ghc/browser/ghc/compiler/main/Main.hs).

## The GHC API


The GHC API is rather stateful; the state of an interaction with GHC is stored in an abstract value of type `GHC.Session`.  The only fundamental reason for this choice is that the `Session` models the state of the RTS's linker, which must be single-threaded.


Although the GHC API apparently supports multiple clients, because each can be interacting with a different `Session`, in fact it only supports one client that is actually executing code, because the [RTS linker](commentary/rts/interpreter#) has a single global symbol table.


This part of the commentary is not a tutorial on *using* the GHC API: for that, see [ Using GHC as a Library](http://haskell.org/haskellwiki/GHC/As_a_library).  Here we are going to talk about the implementation.


A typical interaction with the GHC API goes something like the following:

- Create a new session: `newSession`
- Add some *targets*: `setTargets`, `addTarget`, `guessTarget`
- Perform [Dependency Analysis](#DependencyAnalysis): `depanal`
- Load (compile) the source files: `load`

### Targets


The targets specify the source files or modules at the top of the dependency tree.  For a Haskell program there is often just a single target `Main.hs`, but for a library the targets would consist of every visible module in the library.


The `Target` type is defined in [compiler/main/HscTypes.lhs](/trac/ghc/browser/ghc/compiler/main/HscTypes.lhs).  Note that a `Target` includes not just the file or module name, but also optionally the complete source text of the module as a `StringBuffer`: this is to support an interactive development environment where the source file is being edited, and the in-memory copy of the source file is to be used in preference to the version on disk.

### Dependency Analysis


The dependency analysis phase determines all the Haskell source files that are to be compiled or loaded in the current session, by traversing the transitive dependencies of the targets.  This process is called the *downsweep* because we are traversing the dependency tree downwards from the targets.  (The *upsweep*, where we compile all these files happens in the opposite direction of course).


The `downsweep` function takes the targets and returns a list of `ModSummary` consisting of all the modules to be compiled/loaded.

### The ModSummary type


A `ModSummary` (defined in [compiler/main/HscTypes.h](/trac/ghc/browser/ghc/compiler/main/HscTypes.h)) contains various information about a module:

- Its `Module`, which includes the package that it belongs to
- Its `ModLocation`, which lists the pathnames of all the files associated with the module
- The modules that it imports
- The time it was last modified
- ... some other things


We collect `ModSumary` information for all the modules we are interested in during the *downsweep*, below.  Extracting the information about the module name and the imports from a source file is the job of [compiler/main/HeaderInfo.hs](/trac/ghc/browser/ghc/compiler/main/HeaderInfo.hs) which partially parses the source file.


Converting a given module name into a `ModSummary` is done by `summariseModule` in [compiler/main/GHC.hs](/trac/ghc/browser/ghc/compiler/main/GHC.hs).  Similarly, if we have a filename rather than a module name, we generate a `ModSummary` using `summariseFile`.

### Loading (compiling) the Modules


When the dependency analysis is complete, we can load these modules by calling `GHC.load`.  The same interface is used regardless of whether we are loading modules into GHCi with the `:load` command, or compiling a program with `ghc --make`: we always end up calling `GHC.load`.


The process in principle is fairly simple:

- Visit each module in the dependency tree from the bottom up, invoking [HscMain](commentary/compiler/hsc-main)
  to compile it (the *upsweep*).
- Finally, link all the code together.  In GHCi this involves loading all the object code into memory and linking it
  with the [RTS linker](commentary/rts/interpreter#), and then linking all the byte-code together.  In
  `--make` mode this involves invoking the external linker to link the object code into a binary.


The process is made more tricky in practice for two reasons:

- We might not need to compile certain modules, if none of their dependencies have changed.  GHC's 
  recompilation checker? determines whether a module really needs
  to be compiled or not.
- In GHCi, we might just be reloading the program after making some changes, so we don't even want to re-link
  modules for which no dependencies have changed.

### Stable Modules


ToDo.
