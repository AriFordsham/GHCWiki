# GHC Darcs Repositories


This page lists the active darcs repositories relating to GHC.  For instructions on actually getting a GHC source tree, see [Building/GettingTheSources](building/getting-the-sources).

## The HEAD


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

**Warning.**  Both the `ghc` and `testsuite` repositories tickle a case-sensitivity bug in darcs, so on Windows (only) you can't do a full `darcs get` for `ghc` or `testsuite`.  Instead, either start with a tarball of the repository gotten from somewhere else, or do `darcs get --partial`.  (The repository itself can't be fixed to avoid the bug, without messing up its history.)


The following repositories are the "GHC boot" libraries, i.e. the set of libraries that are necessary to build GHC (i.e. they are used when building the stage 2 compiler). They populate the libraries/ directory of a GHC tree:

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


And the following repositories are the "extra" libraries; if in the libraries/ directory of a GHC tree then they will also be built:

<table><tr><th>[ http://darcs.haskell.org/packages/ALUT](http://darcs.haskell.org/packages/ALUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/GLUT](http://darcs.haskell.org/packages/GLUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HGL](http://darcs.haskell.org/packages/HGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HUnit](http://darcs.haskell.org/packages/HUnit)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/ObjectIO](http://darcs.haskell.org/packages/ObjectIO)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenAL](http://darcs.haskell.org/packages/OpenAL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenGL](http://darcs.haskell.org/packages/OpenGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/QuickCheck](http://darcs.haskell.org/packages/QuickCheck)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/X11](http://darcs.haskell.org/packages/X11)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/arrows](http://darcs.haskell.org/packages/arrows)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/cgi](http://darcs.haskell.org/packages/cgi)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/fgl](http://darcs.haskell.org/packages/fgl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell-src](http://darcs.haskell.org/packages/haskell-src)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/html](http://darcs.haskell.org/packages/html)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/mtl](http://darcs.haskell.org/packages/mtl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/network](http://darcs.haskell.org/packages/network)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/parsec](http://darcs.haskell.org/packages/parsec)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/parallel](http://darcs.haskell.org/packages/parallel)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-base](http://darcs.haskell.org/packages/regex-base)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-compat](http://darcs.haskell.org/packages/regex-compat)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-posix](http://darcs.haskell.org/packages/regex-posix)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/stm](http://darcs.haskell.org/packages/stm)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/time](http://darcs.haskell.org/packages/time)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/xhtml](http://darcs.haskell.org/packages/xhtml)</th></tr></table>

## Branches


The following branches are active:

<table><tr><th>**6.10 Branch**</th>
<td>
Prepend `ghc-6.10` to the name of the repository to get the 6.10 branch.  For example,
the 6.10 ghc repository is at [ http://darcs.haskell.org/ghc-6.10/ghc](http://darcs.haskell.org/ghc-6.10/ghc). 

Note: only the `ghc` repository and the core libraries were branched for 6.10, the
extralibs packages were not. However, there are symlinks under `ghc-6.10`.
The `darcs-all` script knows
where to get everything, so you don't have to worry about this, just follow the
instructions in [Building/GettingTheSources](building/getting-the-sources).
</td></tr></table>