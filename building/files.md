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

<table><tr><th>**`ghc.spec`**</th>
<td>
What is this?
</td></tr></table>

## `libraries/`


The `libraries/` directory contains all the packages that GHC needs to build.  It has one sub-directory for each package repository (e.g. `base`, `haskell98`, `random`). Usually each such repository builds just one package but sometimes more than one (e.g DPH).

- **`libraries/cabal-bin.hs`** is .... (mention the executable too)
- **`libraries/ifBuildable/`** is ...?
- **`libraries/bootstrapping/`** is ...?

`runghc.wrapper`?

## `utils/`, `libffi/`


The `utils` directory contains support utilities that GHC uses.  Some of these are themselves separate repositories that `darcs-all` pulls; others are part of the main GHC repository.  Typically these utilities are built once and for all when your build tree is initialised.

*Why isn't libffi in utils/?*

## `compiler/`, `rts/`, `docs/`, `includes/`


These directories contain the main GHC compiler, runtime system, and documentation.

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