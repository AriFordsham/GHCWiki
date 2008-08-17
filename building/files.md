# Layout of important files and directories


Everything starts with the main GHC repository (see [Building/GettingTheSources](building/getting-the-sources)).   The buld system calls that directory `$(TOP)`.  All the paths below are relative to `$(TOP)`.

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
<td>Run `validate` before committing (see [TestingPatches](testing-patches)).  The script is documented in the file itself.
</td></tr></table>

## `libraries/`


The `libraries/` directory contains all the packages that GHC needs to build.  It has one sub-directory for each package repository (e.g. `base`, `haskell98`, `random`). Usually each such repository builds just one package but sometimes more than one (e.g DPH).

- **`libraries/cabal-bin.hs`** is a little program we use for building the libraries. It's similar to cabal-install, but without the dependencies on `http` etc.
- **`libraries/ifBuildable/`** is a utility that we use in the build system. It allows the build to continue if an extralib is not buildable (e.g., if we are missing a C library that an extralib depends on then we can still build the compiler). We expect this to disappear soon, when extralibs are removed.
- **`libraries/bootstrapping/`**: In order to build `cabal-bin` we need to compile `cabal-bin.hs`, as well as a few libraries that we can't rely on the bootstrapping compiler having. We put the `.hi` and `.o` files that result from this in `bootstrapping/`.

## `utils/`, `libffi/`


The `utils` directory contains support utilities that GHC uses.  Some of these are themselves separate repositories that `darcs-all` pulls; others are part of the main GHC repository.  Typically these utilities are built once and for all when your build tree is initialised.

*Why isn't libffi in utils/?*

## `compiler/`, `rts/`, `docs/`, `includes/`


These directories contain the main GHC compiler, runtime system, and documentation.

- **`compiler/ghc.cabal`**: the Cabal file for GHC.  If you add a module to GHC's source code, you must add it in the `ghc.cabal` file too, else you'll get link errors.

## `testsuite/`, `nofib/`


The `testsuite/` and `nofib/` directories contain apparatus for testing GHC.  Each is a separate repository, which can be gotten with `darcs-all`.

## Stuff that appears only in a build tree

<table><tr><th>**`ghc/`**</th>
<td>
This directory appears only in the build tree. It contains the `.hi` and `.o` files obtained by compiling the compiler.

- **`ghc/stage1-inplace/`, `ghc/stage2-inplace/`**
  Code for the stage1 and stage2 compiler.

</td></tr></table>

*There seems to be `ghc/dist-stage1` too... what's that?  Also there is `compiler/stage1`!
*