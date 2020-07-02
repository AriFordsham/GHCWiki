# GHC Commentary: The GHC API


This section of the commentary describes everything between [HscMain](commentary/compiler/hsc-main) and the front-end; that is, the parts of GHC that coordinate the compilation of multiple modules.


The GHC API is rather stateful; the state of an interaction with GHC is stored in an abstract value of type `GHC.Session`.  The only fundamental reason for this choice is that the `Session` models the state of the RTS's linker, which must be single-threaded.


Although the GHC API apparently supports multiple clients, because each can be interacting with a different `Session`, in fact it only supports one client that is actually executing code, because the [RTS linker](commentary/rts/interpreter#linker) has a single global symbol table.


This part of the commentary is not a tutorial on *using* the GHC API: for that, see [Using GHC as a Library](http://haskell.org/haskellwiki/GHC/As_a_library).  Here we are going to talk about the implementation.


A typical interaction with the GHC API goes something like the following:

- You probably want to wrap the whole program in `defaultErrorHandler defaultFatalMessager defaultFlushOut` to get error messages
- Start a new GHC session: `runGhc`
- Set the flags: `getSessionDynFlags`, `setSessionDynFlags`.
- Add some *targets*: `setTargets`, `addTarget`, `guessTarget`
- Perform [Dependency Analysis](#dependency-analysis): `depanal`
- Load (compile) the source files: `load`


Warning:  Initializing GHC is tricky!  Here is a template that seems to initialize GHC and a session.  Derived from ghc's Main.main function.

```haskell
{-# LANGUAGE CPP #-}
import GHC
import GHC.Paths ( libdir )
#if __GLASGOW_HASKELL__ >= 811
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )
#else
import DynFlags ( defaultFatalMessager, defaultFlushOut )
#endif

main =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags { hscTarget = HscInterpreted
                                  , ghcLink   = LinkInMemory }
    -- your code here, for example:
    --  target <- guessTarget "test_main.hs" Nothing
    --  setTargets [target]
    --  load LoadAllTargets
```


You must pass the path to `package.conf` as an argument to `runGhc`.


The `hscTarget` field of `DynFlags` tells the compiler what kind of output to generate from compilation. The `ghcLink` fiel of `DynFlags` tells the compiler what to do with the resulting object files.

## Targets


The targets specify the source files or modules at the top of the dependency tree.  For a Haskell program there is often just a single target `Main.hs`, but for a library the targets would consist of every visible module in the library.


The `Target` type is defined in [compiler/GHC/Driver/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Types.hs).  Note that a `Target` includes not just the file or module name, but also optionally the complete source text of the module as a `StringBuffer`: this is to support an interactive development environment where the source file is being edited, and the in-memory copy of the source file is to be used in preference to the version on disk.

## Dependency Analysis


The dependency analysis phase determines all the Haskell source files that are to be compiled or loaded in the current session, by traversing the transitive dependencies of the targets.  This process is called the *downsweep* because we are traversing the dependency tree downwards from the targets.  (The *upsweep*, where we compile all these files happens in the opposite direction of course).


The `downsweep` function takes the targets and returns a list of `ModSummary` consisting of all the modules to be compiled/loaded.

## The ModSummary type


A `ModSummary` (defined in [compiler/GHC/Driver/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Types.hs)) contains various information about a module:

- Its `Module`, which includes the package that it belongs to
- Its `ModLocation`, which lists the pathnames of all the files associated with the module
- The modules that it imports
- The time it was last modified
- ... some other things


We collect `ModSumary` information for all the modules we are interested in during the *downsweep*, below.  Extracting the information about the module name and the imports from a source file is the job of [compiler/GHC/Parser/Header.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Parser/Header.hs) which partially parses the source file.


Converting a given module name into a `ModSummary` is done by `summariseModule` in [compiler/GHC.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC.hs).  Similarly, if we have a filename rather than a module name, we generate a `ModSummary` using `summariseFile`.

## Loading (compiling) the Modules


When the dependency analysis is complete, we can load these modules by calling `GHC.load`.  The same interface is used regardless of whether we are loading modules into GHCi with the `:load` command, or compiling a program with `ghc --make`: we always end up calling `GHC.load`.


The process in principle is fairly simple:

- Visit each module in the dependency tree from the bottom up, invoking [HscMain](commentary/compiler/hsc-main)
  to compile it (the *upsweep*).
- Finally, link all the code together.  In GHCi this involves loading all the object code into memory and linking it
  with the [RTS linker](commentary/rts/interpreter#linker), and then linking all the byte-code together.  In
  `--make` mode this involves invoking the external linker to link the object code into a binary.


The process is made more tricky in practice for two reasons:

- We might not need to compile certain modules, if none of their dependencies have changed.  GHC's 
  [recompilation checker](commentary/compiler/recompilation-avoidance) determines whether a module really needs
  to be compiled or not.
- In GHCi, we might just be reloading the program after making some changes, so we don't even want to re-link
  modules for which no dependencies have changed.

## Current maintained testsuites

The GHC API may differ depending on the GHC version.
Testsuites for GHC API, executable examples, are here: https://gitlab.haskell.org/ghc/ghc/-/tree/master/testsuite/tests/ghc-api .

