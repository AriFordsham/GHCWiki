
This page needs updating due to upcomgin changes in the GHC 7.9.x development cycle

# GHC Repositories


This page lists the active repositories relating to GHC. For instructions on actually getting a GHC source tree, see [Getting The Sources](building/getting-the-sources).


For read-only browsing, you can use the:

- [Trac source browser](/trac/ghc/browser/)
- [ Gitweb browser @ git.haskell.org](http://git.haskell.org/)
- [ GitHub GHC mirror](http://github.com/ghc/ghc)


For info on the active branches of the main GHC repo, see

- [ActiveBranches](active-branches)


GHC's repos use git; see [Git Working Conventions](working-conventions/git). For darcs-related stuff see [Darcs To Git](darcs-to-git) and [Git For Darcs Users](git-for-darcs-users).

## Overview


A GHC source tree is made of a collection of repositories. The script [sync-all](building/sync-all) knows how to apply git commands to the whole collection of repositories at once, for example to pull changes from the upstream repositories.


Here is a list of the repositories that GHC uses.  The columns have the following meaning

- **Location in tree**: where in the source tree this repository sits.

- **Upstream repo?**: if "yes", this library is maintained by someone else, 
  and its master repo is somewhere else.  See [Repositories/Upstream](repositories/upstream).

- **Reqd to build?**: is "no" if this library is not required to build GHC. We have a few of these because we use them for tests and suchlike.

- **Installed?**: is "no" if the library is not installed in a GHC installation, and "extra" if it is only installed if `InstallExtraPackages` is `YES`. All others are installed with GHC. See the [libraries page](commentary/libraries) for more info.

- **GHC repo**: in every case there is a repo on `http://git.haskell.org/`, which contains the bits we use for building GHC every night. For libraries with upstream repos, this is just a lagging mirror of the master (see [Repositories/Upstream](repositories/upstream)).  The read-only HTTP URL for the repo is `http://git.haskell.org/<table-entry>`.  To get a read/write URL, replace HTTP prefix `http://git.haskell.org` with the SSH prefix `ssh://git@git.haskell.org`. 

**Note**: `http://darcs.haskell.org/libraries` is actually a [ symlink](http://en.wikipedia.org/wiki/Symbolic_link) to `http://darcs.haskell.org/packages`, therefore they are just synonyms. (For historical reasons.). Moreover, the  Git repositories hosted on [ http://git.haskell.org](http://git.haskell.org) have an 301-redirect installed on their old [ http://darcs.haskell.org](http://darcs.haskell.org) locations.

<table><tr><th>**Location in tree**</th>
<th>**Upstream repo?**</th>
<th>**Reqd to build?**</th>
<th>**Installed?**</th>
<th>**GHC repo http://git.haskell.org/...**</th></tr>
<tr><th>. (ghc itself)</th>
<th></th>
<th></th>
<th></th>
<th>ghc.git</th></tr>
<tr><th>ghc-tarballs</th>
<th></th>
<th></th>
<th> N/A </th>
<th>ghc-tarballs.git</th></tr>
<tr><th>utils/hsc2hs</th>
<th></th>
<th></th>
<th></th>
<th>hsc2hs.git</th></tr>
<tr><th>utils/haddock</th>
<th></th>
<th></th>
<th></th>
<th>haddock.git</th></tr>
<tr><th>testsuite</th>
<th></th>
<th></th>
<th> N/A </th>
<th>testsuite.git</th></tr>
<tr><th>nofib</th>
<th></th>
<th></th>
<th> N/A </th>
<th>nofib.git</th></tr>
<tr><th>libraries/array</th>
<th></th>
<th></th>
<th></th>
<th>packages/array.git</th></tr>
<tr><th>libraries/base</th>
<th></th>
<th></th>
<th></th>
<th>packages/base.git</th></tr>
<tr><th>libraries/binary</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/binary.git</th></tr>
<tr><th>libraries/bytestring</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/bytestring.git</th></tr>
<tr><th>libraries/Cabal</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/Cabal.git</th></tr>
<tr><th>libraries/containers</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/containers.git</th></tr>
<tr><th>libraries/deepseq</th>
<th></th>
<th></th>
<th></th>
<th>packages/deepseq.git</th></tr>
<tr><th>libraries/directory</th>
<th></th>
<th></th>
<th></th>
<th>packages/directory.git</th></tr>
<tr><th>libraries/extensible-exceptions</th>
<th></th>
<th></th>
<th></th>
<th>packages/extensible-exceptions.git</th></tr>
<tr><th>libraries/filepath</th>
<th></th>
<th></th>
<th></th>
<th>packages/filepath.git</th></tr>
<tr><th>libraries/ghc-prim</th>
<th></th>
<th></th>
<th></th>
<th>packages/ghc-prim.git</th></tr>
<tr><th>libraries/haskeline</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/haskeline.git</th></tr>
<tr><th>libraries/haskell98</th>
<th></th>
<th></th>
<th></th>
<th>packages/haskell98.git</th></tr>
<tr><th>libraries/haskell2010</th>
<th></th>
<th></th>
<th></th>
<th>packages/haskell2010.git</th></tr>
<tr><th>libraries/hoopl</th>
<th></th>
<th></th>
<th></th>
<th>packages/hoopl.git</th></tr>
<tr><th>libraries/hpc</th>
<th></th>
<th></th>
<th></th>
<th>packages/hpc.git</th></tr>
<tr><th>libraries/integer-gmp</th>
<th></th>
<th></th>
<th></th>
<th>packages/integer-gmp.git</th></tr>
<tr><th>libraries/integer-simple</th>
<th></th>
<th></th>
<th></th>
<th>packages/integer-simple.git</th></tr>
<tr><th>libraries/old-locale</th>
<th></th>
<th></th>
<th></th>
<th>packages/old-locale.git</th></tr>
<tr><th>libraries/old-time</th>
<th></th>
<th></th>
<th></th>
<th>packages/old-time.git</th></tr>
<tr><th>libraries/pretty</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/pretty.git</th></tr>
<tr><th>libraries/process</th>
<th></th>
<th></th>
<th></th>
<th>packages/process.git</th></tr>
<tr><th>libraries/template-haskell</th>
<th></th>
<th></th>
<th></th>
<th>packages/template-haskell.git</th></tr>
<tr><th>libraries/terminfo</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/terminfo.git</th></tr>
<tr><th>libraries/time</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/time.git</th></tr>
<tr><th>libraries/transformers</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/transformers.git</th></tr>
<tr><th>libraries/unix</th>
<th></th>
<th></th>
<th></th>
<th>packages/unix.git</th></tr>
<tr><th>libraries/utf8-string</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/utf8-string.git</th></tr>
<tr><th>libraries/Win32</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/Win32.git</th></tr>
<tr><th>libraries/xhtml</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/xhtml.git</th></tr>
<tr><th>libraries/random</th>
<th> yes </th>
<th></th>
<th>extra</th>
<th>packages/random.git</th></tr>
<tr><th>libraries/primitive</th>
<th> yes </th>
<th></th>
<th>extra</th>
<th>packages/primitive.git</th></tr>
<tr><th>libraries/vector</th>
<th> yes </th>
<th></th>
<th>extra</th>
<th>packages/vector.git</th></tr>
<tr><th>libraries/dph</th>
<th></th>
<th></th>
<th>extra</th>
<th>packages/dph.git</th></tr>
<tr><th>libraries/parallel</th>
<th></th>
<th> no  </th>
<th>extra</th>
<th>packages/parallel.git</th></tr>
<tr><th>libraries/stm</th>
<th></th>
<th> no  </th>
<th>extra</th>
<th>packages/stm.git</th></tr></table>

## The 'packages' file


The master list of repositories is in the file [packages](/trac/ghc/browser/ghc/packages), and this is where the `sync-all` script finds out about which repositories make up the complete tree.  It duplicates the information in the above table; indeed, it is really the authoritative version (so complain if the table and file differ!).


The "`tag`" in the master table in [packages](/trac/ghc/browser/ghc/packages) has the following significance:

- **"`-`"**: [boot libraries](commentary/libraries), necessary to build GHC
- **"`testsuite`"**: GHC's [regression tests](building/running-tests), not necessary for a build, but is necessary if you're working on GHC
- **"`nofib`"**: GHC's [nofib benchmark suite](building/running-no-fib)
- **"`dph`"**: packages for [Data Parallel Haskell](data-parallel), which is not shipped with GHC but we test all changes to GHC against these repositories so they are usually included in a checked-out source tree.
- **"`extra`"**: extra packages you might want to include in a build (the `parallel` package, for example), but aren't necessary to get a working GHC.


See the [Commentary/Libraries](commentary/libraries) page for more information about GHC's libraries.

## Modifying local packages


For libraries for which there is no upstream repo, you can modify the GHC repo above directly.


When making a change to a library, you must also update the version
number if appropriate. Version number in the repositories should be
maintained such that, if the library were to be release as-is, then
they would have the correct version number. For example, if the last
release of a library was 1.2.0.3 and you remove a function from it
then, as per the
[ Package versioning policy](http://www.haskell.org/haskellwiki/Package_versioning_policy),
the version number should be bumped to 1.3.0.0. If it is already
1.3.0.0 or higher then no further change is necessary. In order to
make this easier, the version line in the `.cabal` file should be
followed by a comment such as

```wiki
-- GHC 7.6.1 released with 1.2.0.3
```

## Mirroring new packages to GitHub


Currently, all our repositories are being mirrored to GitHub by GitHub themselves. If you wish to add/remove a repository you need to email GitHub support at support@… and ask them to do it. Currently there is no way to administer this ourselves.

## Branches


For how we manage release branches, see [WorkingConventions/Releases](working-conventions/releases).


The following branches are active:

<table><tr><th>**7.4 Branch**</th>
<td>
To switch to this branch run:

```wiki
$ ./sync-all checkout ghc-7.4
```

</td></tr></table>