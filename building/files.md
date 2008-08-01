# Layout of important files and directories


Everything starts with the main GHC repository (see [Building/GettingTheSources](building/getting-the-sources)).   The buld system calls that directory `$(TOP)`.  All the paths below are relative to `$(TOP)`.

<table><tr><th>**`darcs-all`**</th>
<td>
This script allows you to get or pull all the additional repositories that you need to build GHC.  The command-line interface is documented in the file itself.
</td></tr></table>

<table><tr><th>**`packages`**</th>
<td>
Lists the packages that `darcs-all` should get or pull.  `packages` is looked at only by `darcs-all`.
</td></tr></table>

<table><tr><th>**`libraries/`**</th>
<td>
Sub-directory for all the packages that GHC needs.
</td></tr></table>

<table><tr><th>**`utils/`**</th>
<td>
Sub-directory for support utilities that GHC uses.  Some of these are themselves separate repositories that `darcs-all` pulls; others are part of the main GHC repository.  Typically these utilities are built once and for all when your build tree is initialised.
</td></tr></table>

<table><tr><th>**`compiler/`**</th>
<td>
The main GHC compiler.
</td></tr></table>

<table><tr><th>**`rts/`**</th>
<td>
The runtime system.
</td></tr></table>

<table><tr><th>**`mk/`**</th>
<td>
Makefile support for the build system.
</td></tr></table>

<table><tr><th>**`ghc/`**</th>
<td>
This directory appears only in the build tree. It contains the `.hi` and `.o` files obtained by compiling the compiler.

- **`ghc/stage1-inplace/`, `ghc/stage2-inplace/`**
  Code for the stage1 and stage2 compiler.

</td></tr></table>