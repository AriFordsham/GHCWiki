# GHC Repositories


This page lists the active repositories relating to GHC. For instructions on actually getting a GHC source tree, see [Getting The Sources](building/getting-the-sources).


For read-only browsing, you can use the:

- [ Trac source browser](http://hackage.haskell.org/trac/ghc/browser)
- [ GitHub GHC mirror](http://github.com/ghc/ghc)
- [ GitWeb GHC browser](http://darcs.haskell.org/cgi-bin/gitweb.cgi)


GHC's repos use git; see [Git Working Conventions](working-conventions/git). For darcs-related stuff see [Darcs To Git](darcs-to-git) and [Git For Darcs Users](git-for-darcs-users).

## Overview


A GHC source tree is made of a collection of repositories. The script [sync-all](building/sync-all) knows how to apply git commands to the whole collection of repositories at once, for example to pull changes from the upstream repositories.


The root of the source tree is the GHC repository itself, the other repositories live in various subdirectories. The master list of repositories is in the file [packages](/trac/ghc/browser/ghc/packages), and this is where the `sync-all` script finds out about which repositories make up the complete tree.


The "`tag`" in the master table in packages has the following significance:

- **"`-`"**: [boot libraries](commentary/libraries), necessary to build GHC
- **"`testsuite`"**: GHC's [regression tests](building/running-tests), necessary for a build, but is necessary if you're working on GHC
- **"`nofib`"**: GHC's [nofib benchmark suite](building/running-no-fib)
- **"`dph`"**: packages for [Data Parallel Haskell](data-parallel), which is not shipped with GHC but we test all changes to GHC against these repositories so they are usually included in a checked-out source tree.
- **"`extra`"**: extra packages you might want to include in a build (the `parallel` package, for example), but aren't necessary to get a working GHC.

## Repository and mirrors


Many of the libraries and tools in a GHC tree are actually maintained by someone else. They therefore have a separate upstream repository, from which we need to pull. That repository may be either a darcs or a git repository; in the darcs case, we also need to convert to a git repository for use in a GHC tree. However, if the darcs repository is on another server, then we first need to mirror it for the conversion program to use. This diagram shows how changes migrate from one repo to another:

not handled: Image


This means that when making changes needed in GHC to one of these libraries, we first need to put the changes in the upstream repository. Note that a git hook prevents you from pushing patches to the ghc repos until they are already in the git mirror repos, so that we cannot forget to send changes upstream.


The mirrors are updated automatically each night, but you can force an immediate update by running `/srv/darcs/do_mirrors` on `darcs.haskell.org`.

## When the master repo is in Git


If the master (upstream) repo is in Git (eg `containers`), you can use the following workflow:

1. Push the change to the upstream repo `//github.com/haskell/containers.git`
1. Push the change to the validated GHC repo for `containers`, namely [ http://darcs.haskell.org/packages/containers.git](http://darcs.haskell.org/packages/containers.git)


In other words, you don't need to interact with the git mirror on `darcs.haskell.org`. It is *only* there so that in step (2), the push script can check that the patch you are pushing is in the mirror, thereby ensuring that we always lag the master repo.  (For some reason this is hard to do directly on the master.)


Note step 2 will fail until the mirror has been updated.

## When the master repo is in Darcs


Things are a bit more complicated when the master repo is in Darcs.  For example, to make a change to Cabal:

1. First push the change as a darcs patch to the upstream Cabal repository, [ http://code.haskell.org/Cabal/](http://code.haskell.org/Cabal/)
1. The patch will be mirrored and converted to git by the mirror script, in the repo [ http://darcs.haskell.org/git-mirrors/Cabal/.git/](http://darcs.haskell.org/git-mirrors/Cabal/.git/)
1. You then need to pull from [ http://darcs.haskell.org/git-mirrors/Cabal/.git/](http://darcs.haskell.org/git-mirrors/Cabal/.git/) into `libraries/Cabal` in a regular GHC tree, validate, and push to the GHC Cabal repo, [ http://darcs.haskell.org/packages/Cabal.git/](http://darcs.haskell.org/packages/Cabal.git/)

## Repository locations

This table shows, for each repository in a GHC tree, where the central repository is, and what mirrors there are.

<table><tr><th>darcs</th>
<th>git</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>darcs upstream</th>
<th>darcs mirror</th>
<th>git upstream</th>
<th>git mirror</th>
<th>ghc (validated) repo</th>
<th>in-tree</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/ghc.git/</th>
<th>.</th>
<th>ghc</th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/ghc-tarballs.git/</th>
<th>ghc-tarballs</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/utils/hsc2hs.git/</th>
<th>utils/hsc2hs</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/haddock.git</th>
<th>utils/haddock</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/array.git/</th>
<th>libraries/array</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/base.git/</th>
<th>libraries/base</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/kolmodin/binary.git</th>
<th>http://darcs.haskell.org/git-mirrors/binary/binary.git/</th>
<th>http://darcs.haskell.org/packages/.git/</th>
<th>libraries/binary</th>
<th></th></tr>
<tr><th>http://darcs.haskell.org/bytestring/</th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/bytestring/.git/</th>
<th>http://darcs.haskell.org/packages/bytestring.git/</th>
<th>libraries/bytestring</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/cabal.git</th>
<th>http://darcs.haskell.org/git-mirrors/Cabal/.git/</th>
<th>http://darcs.haskell.org/packages/Cabal.git/</th>
<th>libraries/Cabal</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/containers.git</th>
<th>http://darcs.haskell.org/git-mirrors/containers/.git/</th>
<th>http://darcs.haskell.org/packages/containers.git/</th>
<th>libraries/containers</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/directory.git/</th>
<th>libraries/directory</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/extensible-exceptions.git/</th>
<th>libraries/extensible-exceptions</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/filepath.git/</th>
<th>libraries/filepath</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/ghc-prim.git/</th>
<th>libraries/ghc-prim</th>
<th></th></tr>
<tr><th>http://code.haskell.org/haskeline/</th>
<th>http://darcs.haskell.org/darcs-mirrors/haskeline/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/haskeline/.git/</th>
<th>http://darcs.haskell.org/packages/haskeline.git/</th>
<th>libraries/haskeline</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell98.git/</th>
<th>libraries/haskell98</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell2010.git/</th>
<th>libraries/haskell2010</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>(pull) git://code.eecs.tufts.edu/hoopl/hoopl.git, (push) linux.cs.tufts.edu:/r/c--/papers/dfopt.git</th>
<th>http://darcs.haskell.org/git-mirrors/hoopl/</th>
<th>http://darcs.haskell.org/packages/hoopl.git/</th>
<th>libraries/hoopl</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/hpc.git/</th>
<th>libraries/hpc</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-gmp.git/</th>
<th>libraries/integer-gmp</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-simple.git/</th>
<th>libraries/integer-simple</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/ekmett/mtl.git</th>
<th>http://darcs.haskell.org/git-mirrors/mtl/.git</th>
<th>http://darcs.haskell.org/packages/mtl.git/</th>
<th>libraries/mtl</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/old-locale.git/</th>
<th>libraries/old-locale</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/old-time.git/</th>
<th>libraries/old-time</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/pretty.git</th>
<th>http://darcs.haskell.org/git-mirrors/pretty/</th>
<th>http://darcs.haskell.org/packages/pretty.git/</th>
<th>libraries/pretty</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/process.git/</th>
<th>libraries/process</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/random.git</th>
<th>http://darcs.haskell.org/git-mirrors/random/</th>
<th>http://darcs.haskell.org/packages/random.git/</th>
<th>libraries/random</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/template-haskell.git/</th>
<th>libraries/template-haskell</th>
<th></th></tr>
<tr><th>http://code.haskell.org/terminfo/</th>
<th>http://darcs.haskell.org/darcs-mirrors/terminfo/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/terminfo/.git/</th>
<th>http://darcs.haskell.org/packages/terminfo.git/</th>
<th>libraries/terminfo</th>
<th></th></tr>
<tr><th>http://code.haskell.org/\~ross/transformers</th>
<th>http://darcs.haskell.org/darcs-mirrors/transformers/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/transformers/.git/</th>
<th>http://darcs.haskell.org/packages/transformers.git/</th>
<th>libraries/transformers</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/unix.git/</th>
<th>libraries/unix</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>https://github.com/glguy/utf8-string.git</th>
<th>http://darcs.haskell.org/git-mirrors/utf8-string/</th>
<th>http://darcs.haskell.org/packages/utf8-string.git/</th>
<th>libraries/utf8-string</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/win32.git</th>
<th>http://darcs.haskell.org/git-mirrors/Win32/</th>
<th>http://darcs.haskell.org/packages/Win32.git/</th>
<th>libraries/Win32</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/xhtml.git</th>
<th>http://darcs.haskell.org/git-mirrors/xhtml/</th>
<th>http://darcs.haskell.org/packages/xhtml.git/</th>
<th>libraries/xhtml</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/testsuite.git/</th>
<th>testsuite</th>
<th>testsuite</th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/nofib.git</th>
<th>nofib</th>
<th>nofib</th></tr>
<tr><th>http://code.haskell.org/primitive/</th>
<th>http://darcs.haskell.org/darcs-mirrors/primitive/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/primitive/.git/</th>
<th>http://darcs.haskell.org/packages/primitive.git/</th>
<th>libraries/primitive</th>
<th>dph</th></tr>
<tr><th>http://code.haskell.org/vector/</th>
<th>http://darcs.haskell.org/darcs-mirrors/vector/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/vector/.git/</th>
<th>http://darcs.haskell.org/packages/vector.git/</th>
<th>libraries/vector</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/dph.git/</th>
<th>libraries/dph</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/deepseq.git/</th>
<th>libraries/deepseq</th>
<th>extra</th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/parallel.git/</th>
<th>libraries/parallel</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/packages/stm.git/</th>
<th>libraries/stm</th>
<th></th></tr></table>

## Mirroring new packages to GitHub


Currently, all our repositories are being mirrored to GitHub by GitHub themselves. If you wish to add/remove a repository you need to email GitHub support at support@… and ask them to do it. Currently there is no way to administer this ourselves.

## Branches


The following branches are active:

<table><tr><th>**7.2 Branch**</th>
<td>
To switch to this branch run:

```wiki
$ ./sync-all checkout ghc-7.2
```

</td></tr></table>

**7.4 Branch**::

>
> To switch to this branch run:
>
> ```wiki
> $ ./sync-all checkout ghc-7.4
> ```