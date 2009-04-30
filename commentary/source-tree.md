# Layout of important files and directories


This page summarises the overall file and directory structure of GHC.  We include both source files and generated files; the latter are always identified "build-tree only".


Everything starts with the main GHC repository (see [Building/GettingTheSources](building/getting-the-sources)).   The biuld system calls that directory `$(TOP)`.  All the paths below are relative to `$(TOP)`.

## Files in `$(TOP)`

<table><tr><th>**`darcs-all`**</th>
<td>
This script allows you to get or pull all the additional repositories that you need to build GHC.  The command-line interface is documented in the file itself.
</td></tr></table>

<table><tr><th>**`packages`**</th>
<td>
Lists the packages that `darcs-all` should get or pull.  `packages` is looked at only by `darcs-all`.
</td></tr></table>

<table><tr><th>**`validate`**</th>
<td>Run `validate` (a shell script) before committing (see [TestingPatches](testing-patches)).  The script is documented in the file itself.
</td></tr></table>

<table><tr><th>**Documentation files**</th>
<td>`README`, `ANNOUNCE`, `HACKING`, `LICENSE`</td></tr></table>

<table><tr><th>**GNU autoconf machinery**</th>
<td>`aclocal.m4`, `config.guess`, `config.sub`, `configure.ac`, `install-sh`, `config.mk.in`</td></tr></table>

<table><tr><th>**`ghc.spec.in`**</th>
<td>the RPM spec file
</td></tr></table>

<table><tr><th>**`ghc.mk`**, **`Makefile`**</th>
<td>The top-level `Makefile`: see [GHC Build System Architecture](building/architecture). GHC requires
[ GNU make](http://www.gnu.org/software/make/).
</td></tr></table>

## `libraries/`


The `libraries/` directory contains all the packages that GHC needs to build.  It has one sub-directory for each package repository (e.g. `base`, `haskell98`, `random`). Usually each such repository builds just one package but sometimes more than one (e.g DPH).

**NOT RIGHT: PLEASE FIX**

- **`libraries/cabal-bin.hs`** is a little program we use for building the libraries. It's similar to cabal-install, but without the dependencies on `http` etc.
- **`libraries/ifBuildable/`** is a utility that we use in the build system. It allows the build to continue if an extralib is not buildable (e.g., if we are missing a C library that an extralib depends on then we can still build the compiler). We expect this to disappear soon, when extralibs are removed.
- **`libraries/bootstrapping/`**: In order to build `cabal-bin` we need to compile `cabal-bin.hs`, as well as a few libraries that we can't rely on the bootstrapping compiler having. We put the `.hi` and `.o` files that result from this in `bootstrapping/`.

## `compiler/`, `docs/`, `ghc/`


These directories contain the main GHC compiler and documentation.
The `compiler/` directory contains the ghc package, which is linked
into an executable in the `ghc/` directory.


There is [documentation of the intended module dependency structure](module-dependencies) of the `compiler/` directory.

- **`compiler/ghc.cabal`**: the Cabal file for GHC.  If you add a module to GHC's source code, you must add it in the `ghc.cabal` file too, else you'll get link errors. **LIES ALL LIES**

## `rts/`


Sources for the runtime system; see [Commentary/SourceTree/Rts](commentary/source-tree/rts).

## `includes/`


Header files for the runtime system; see [Commentary/SourceTree/Includes](commentary/source-tree/includes).

## `utils/`, `libffi/`


The `utils` directory contains support utilities that GHC uses.  Some of these are themselves separate repositories that `darcs-all` pulls; others are part of the main GHC repository. 


These utils may be built with the bootstrapping compiler, for use during the build, or with the stage2 compiler, for installing. Some of them are built with both; we can't install the utils built with the bootstrapping compiler as they may use different versions of C libraries. The reason we use stage2 rather than stage1 is that some utils, e.g. haddock, need the GHC API package.  The file `utils/Makefile` controls all this.

*Why isn't libffi in utils/?*

## `testsuite/`, `nofib/`


The `testsuite/` and `nofib/` directories contain apparatus for testing GHC.  Each is a separate repository, which can be gotten with `darcs-all`.

## `mk/`, `rules.mk`


The `mk/` and `rules.mk` directories contains all the build system Makefile boilerplate; see [GHC Build System Architecture](building/architecture).  Some particular files are interesting:

- **`mk/build.mk`**: contains Makefile settings that control your build. Details [here](building/hacking).  The file `mk/build.mk.sample` contains a starting point that you can copy to `mk/build.mk` if you want.
- **`mk/are-validating.mk`**: this file records the fact that you are doing [validation](testing-patches), by containing the single line `Validating=YES`.  That in turn means the the build system gets its settings from `mk/validate-settings.mk` instead of from `mk/build.mk`.  Remove the file to stop validating.
- **`mk/validate.mk`**: just like `build.mk`, but applies when validating.  Use this file to override the default settings for validation, which are in `mk/validate-settings.mk`.

## `inplace/`


The `inplace/` directory (build tree only) is where we "install" stage1 and stage2 compilers when they are built, and GHC's utility programs, entirely locally to the tree.  The layout is exactly the same as that of an installed GHC on the host platform.

- **`inplace/bin/`**: executables, including `ghc-stage1`, `ghc-stage2`, `hasktags`, `hsc2hs`, `haddock`, etc
- **`inplace/lib/`**: suppporting libraries for the above.

## `distrib/`


Micellaneous files for building distributions.

## Stuff that appears only in a build tree

- **`compiler/stage1/`, `ghc/stage2plus/`**
  These directories contain `ghc_boot_platform.h`, which contains various `#define`s needed when building GHC. These are different depending on whether we are building stage1 or a later stage. **WHERE ARE THESE FILES NOW?**

- **`.../dist*/`**
  In many directories, `dist*` subdirectories appear. These are where Cabal puts all of the files generated while building.  **IS THIS STILL TRUE?**