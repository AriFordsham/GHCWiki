# GHC Repositories


This page lists the active repositories relating to GHC.  For instructions on actually getting a GHC source tree, see [Building/GettingTheSources](building/getting-the-sources).


GHC's repos use Git.  For Darcs-related stuff see also [DarcsToGit](darcs-to-git) and [GitForDarcsUsers](git-for-darcs-users).

## The HEAD


The Master List of repositories is in the file [ $(TOP)/packages](http://darcs.haskell.org/ghc/packages).  It is authoritative.  Everything else in this sub-section may be out of date, although it gives the right general idea.


These repositories are the HEAD (main trunk) of GHC development:

<table><tr><th>[ http://darcs.haskell.org/ghc.git](http://darcs.haskell.org/ghc.git)</th>
<th>The main GHC repository
</th></tr>
<tr><th>[ http://darcs.haskell.org/testsuite.git](http://darcs.haskell.org/testsuite.git)</th>
<th>The test suite (requires python 2.4+) 
</th></tr>
<tr><th>[ http://darcs.haskell.org/nofib.git](http://darcs.haskell.org/nofib.git)</th>
<th>The benchmark suite
</th></tr></table>


Our repositories are either git repo, or darcs "hashed" format repos. You need Darcs version 2 or later (preferably 2.4 or later) to get the darcs repos. Use darcs's `--lazy` option when getting the repository remotely to dramatically speed up the transfer.


The following repositories are the "GHC boot" libraries, i.e. the set of libraries that are necessary to build GHC (i.e. they are used when building the stage 2 compiler). They populate the libraries/ directory of a GHC tree.

<table><tr><th>[ http://darcs.haskell.org/packages/array.git](http://darcs.haskell.org/packages/array.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/base.git](http://darcs.haskell.org/packages/base.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/bytestring.git](http://darcs.haskell.org/packages/bytestring.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Cabal.git](http://darcs.haskell.org/packages/Cabal.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/containers.git](http://darcs.haskell.org/packages/containers.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/directory.git](http://darcs.haskell.org/packages/directory.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/filepath.git](http://darcs.haskell.org/packages/filepath.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell98.git](http://darcs.haskell.org/packages/haskell98.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/old-locale.git](http://darcs.haskell.org/packages/old-locale.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/old-time.git](http://darcs.haskell.org/packages/old-time.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/packedstring.git](http://darcs.haskell.org/packages/packedstring.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/pretty.git](http://darcs.haskell.org/packages/pretty.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/process.git](http://darcs.haskell.org/packages/process.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/random.git](http://darcs.haskell.org/packages/random.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/readline.git](http://darcs.haskell.org/packages/readline.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/template-haskell.git](http://darcs.haskell.org/packages/template-haskell.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/unix.git](http://darcs.haskell.org/packages/unix.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Win32.git](http://darcs.haskell.org/packages/Win32.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/hpc.git](http://darcs.haskell.org/packages/hpc.git)</th></tr></table>


Additionally a GHC build needs these two tools:

<table><tr><th>[ http://darcs.haskell.org/hsc2hs.git](http://darcs.haskell.org/hsc2hs.git)</th></tr>
<tr><th>[ http://darcs.haskell.org/haddock2.git](http://darcs.haskell.org/haddock2.git)</th></tr></table>

## Branches


The following branches are active:

<table><tr><th>**7.2 Branch**</th>
<td>
\[TODO!\]
</td></tr></table>