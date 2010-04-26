# GHC Darcs Repositories


This page lists the active darcs repositories relating to GHC.  For instructions on actually getting a GHC source tree, see [Building/GettingTheSources](building/getting-the-sources).

## The HEAD


The Master List of repositories is in the file [ $(TOP)/packages](http://darcs.haskell.org/ghc/packages).  It is authoritative.  Everything else in this sub-section may be out of date, although it gives the right general idea.


These darcs repositories are the HEAD (main trunk) of GHC development:

<table><tr><th>[ http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc)</th>
<th>The main GHC repository
</th></tr>
<tr><th>[ http://darcs.haskell.org/testsuite](http://darcs.haskell.org/testsuite)</th>
<th>The test suite (requires python 2.4+) 
</th></tr>
<tr><th>[ http://darcs.haskell.org/nofib](http://darcs.haskell.org/nofib)</th>
<th>The benchmark suite
</th></tr></table>


Our repositories are all in the darcs "hashed" format, so you need Darcs verison 2 or later (preferably 2.4 or later) to get them.  Use the `--lazy` option when getting the repository remotely to dramatically speed up the transfer.


The following repositories are the "GHC boot" libraries, i.e. the set of libraries that are necessary to build GHC (i.e. they are used when building the stage 2 compiler). They populate the libraries/ directory of a GHC tree.

<table><tr><th>[ http://darcs.haskell.org/packages/array](http://darcs.haskell.org/packages/array)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/base](http://darcs.haskell.org/packages/base)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/bytestring](http://darcs.haskell.org/packages/bytestring)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Cabal](http://darcs.haskell.org/packages/Cabal)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/containers](http://darcs.haskell.org/packages/containers)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/directory](http://darcs.haskell.org/packages/directory)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/filepath](http://darcs.haskell.org/packages/filepath)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell98](http://darcs.haskell.org/packages/haskell98)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/old-locale](http://darcs.haskell.org/packages/old-locale)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/old-time](http://darcs.haskell.org/packages/old-time)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/packedstring](http://darcs.haskell.org/packages/packedstring)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/pretty](http://darcs.haskell.org/packages/pretty)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/process](http://darcs.haskell.org/packages/process)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/random](http://darcs.haskell.org/packages/random)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/readline](http://darcs.haskell.org/packages/readline)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/template-haskell](http://darcs.haskell.org/packages/template-haskell)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/unix](http://darcs.haskell.org/packages/unix)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Win32](http://darcs.haskell.org/packages/Win32)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/hpc](http://darcs.haskell.org/packages/hpc)</th></tr></table>


Additionally a GHC build needs these two tools:

<table><tr><th>[ http://darcs.haskell.org/hsc2hs](http://darcs.haskell.org/hsc2hs)</th></tr>
<tr><th>[ http://darcs.haskell.org/haddock2](http://darcs.haskell.org/haddock2)</th></tr></table>

## Branches


The following branches are active:

<table><tr><th>**6.12 Branch**</th>
<td>
Prepend `ghc-6.12` to the name of the repository to get the 6.12 branch.  For example,
the 6.12 ghc repository is at [ http://darcs.haskell.org/ghc-6.12/ghc](http://darcs.haskell.org/ghc-6.12/ghc). 
</td></tr></table>

<table><tr><th>**6.10 Branch**</th>
<td>
As for the 6.12 branch, but use 6.10 as the version number.

Note: only the `ghc` repository and the core libraries were branched for 6.10, the
extralibs packages were not. However, there are symlinks under `ghc-6.10`.
The `darcs-all` script knows
where to get everything, so you don't have to worry about this, just follow the
instructions in [Building/GettingTheSources](building/getting-the-sources).
</td></tr></table>