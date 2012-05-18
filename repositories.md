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
1. Push the change to the validated GHC repo for `containers`, namely `darcs.haskell.org:/srv/darcs/packages/containers.git`.


In other words, you don't need to interact with the git mirror on `darcs.haskell.org`. It is *only* there so that in step (2), the push script can check that the patch you are pushing is in the mirror, thereby ensuring that we always lag the master repo.  (For some reason this is hard to do directly on the master.)


Note step 2 will fail until the mirror has been updated.

## When the master repo is in Darcs


Things are a bit more complicated when the master repo is in Darcs.  For example, to make a change to bytestring:

1. First push the change as a darcs patch to the upstream bytestring repository, [ http://darcs.haskell.org/bytestring/](http://darcs.haskell.org/bytestring/)
1. The patch will be mirrored and converted to git by the mirror script, in the repo [ http://darcs.haskell.org/git-mirrors/bytestring/.git/](http://darcs.haskell.org/git-mirrors/bytestring/.git/)
1. You then need to pull from [ http://darcs.haskell.org/git-mirrors/bytestring/.git/](http://darcs.haskell.org/git-mirrors/bytestring/.git/) into `libraries/bytestring` in a regular GHC tree, validate, and push to the GHC bytestring repo, `darcs.haskell.org:/srv/darcs/packages/bytestring.git`.

## Repository locations

This table shows, for each repository in a GHC tree, where the central repository (identified in green) is, and what mirrors there are.

For the common case where the central repository held on `darcs.haskell.org` and appears in the "ghc (validated) repo" column, we give the read-only URL.  To get a read/write URL, replace HTTP prefix `http://darcs.haskell.org` with the SSH prefix `darcs.haskell.org:/srv/darcs`.
darcsgitdarcs upstreamdarcs mirrorgit upstreamgit mirrorghc (validated) repoin-treehttp://darcs.haskell.org/ghc.git/.ghchttp://darcs.haskell.org/ghc-tarballs.git/ghc-tarballshttp://darcs.haskell.org/utils/hsc2hs.git/utils/hsc2hshttp://darcs.haskell.org/haddock.gitutils/haddockhttp://darcs.haskell.org/packages/array.git/libraries/arrayhttp://darcs.haskell.org/packages/base.git/libraries/basegit://github.com/kolmodin/binary.githttp://darcs.haskell.org/git-mirrors/binary/binary.git/http://darcs.haskell.org/packages/.git/libraries/binaryhttp://darcs.haskell.org/bytestring/http://darcs.haskell.org/git-mirrors/bytestring/.git/http://darcs.haskell.org/packages/bytestring.git/libraries/bytestringgit://github.com/haskell/cabal.githttp://darcs.haskell.org/git-mirrors/Cabal/.git/http://darcs.haskell.org/packages/Cabal.git/libraries/Cabalgit://github.com/haskell/containers.githttp://darcs.haskell.org/git-mirrors/containers/.git/http://darcs.haskell.org/packages/containers.git/libraries/containershttp://darcs.haskell.org/packages/directory.git/libraries/directoryhttp://darcs.haskell.org/packages/extensible-exceptions.git/libraries/extensible-exceptionshttp://darcs.haskell.org/packages/filepath.git/libraries/filepathhttp://darcs.haskell.org/packages/ghc-prim.git/libraries/ghc-primhttp://code.haskell.org/haskeline/http://darcs.haskell.org/darcs-mirrors/haskeline/http://darcs.haskell.org/git-mirrors/haskeline/.git/http://darcs.haskell.org/packages/haskeline.git/libraries/haskelinehttp://darcs.haskell.org/packages/haskell98.git/libraries/haskell98http://darcs.haskell.org/packages/haskell2010.git/libraries/haskell2010http://darcs.haskell.org/packages/hoopl.git/libraries/hooplhttp://darcs.haskell.org/packages/hpc.git/libraries/hpchttp://darcs.haskell.org/packages/integer-gmp.git/libraries/integer-gmphttp://darcs.haskell.org/packages/integer-simple.git/libraries/integer-simplegit://github.com/ekmett/mtl.githttp://darcs.haskell.org/git-mirrors/mtl/.githttp://darcs.haskell.org/packages/mtl.git/libraries/mtlhttp://darcs.haskell.org/packages/old-locale.git/libraries/old-localehttp://darcs.haskell.org/packages/old-time.git/libraries/old-timegit://github.com/haskell/pretty.githttp://darcs.haskell.org/git-mirrors/pretty/http://darcs.haskell.org/packages/pretty.git/libraries/prettyhttp://darcs.haskell.org/packages/process.git/libraries/processgit://github.com/haskell/random.githttp://darcs.haskell.org/git-mirrors/random/http://darcs.haskell.org/packages/random.git/libraries/randomhttp://darcs.haskell.org/packages/template-haskell.git/libraries/template-haskellhttp://code.haskell.org/terminfo/http://darcs.haskell.org/darcs-mirrors/terminfo/http://darcs.haskell.org/git-mirrors/terminfo/.git/http://darcs.haskell.org/packages/terminfo.git/libraries/terminfohttp://code.haskell.org/\~ross/transformershttp://darcs.haskell.org/darcs-mirrors/transformers/http://darcs.haskell.org/git-mirrors/transformers/.git/http://darcs.haskell.org/packages/transformers.git/libraries/transformershttp://darcs.haskell.org/packages/unix.git/libraries/unixhttps://github.com/glguy/utf8-string.githttp://darcs.haskell.org/git-mirrors/utf8-string/http://darcs.haskell.org/packages/utf8-string.git/libraries/utf8-stringgit://github.com/haskell/win32.githttp://darcs.haskell.org/git-mirrors/Win32/http://darcs.haskell.org/packages/Win32.git/libraries/Win32git://github.com/haskell/xhtml.githttp://darcs.haskell.org/git-mirrors/xhtml/http://darcs.haskell.org/packages/xhtml.git/libraries/xhtmlhttp://darcs.haskell.org/testsuite.git/testsuitetestsuitehttp://darcs.haskell.org/nofib.gitnofibnofibhttp://code.haskell.org/primitive/http://darcs.haskell.org/darcs-mirrors/primitive/http://darcs.haskell.org/git-mirrors/primitive/.git/http://darcs.haskell.org/packages/primitive.git/libraries/primitivedphhttp://code.haskell.org/vector/http://darcs.haskell.org/darcs-mirrors/vector/http://darcs.haskell.org/git-mirrors/vector/.git/http://darcs.haskell.org/packages/vector.git/libraries/vectorhttp://darcs.haskell.org/packages/dph.git/libraries/dphhttp://darcs.haskell.org/packages/deepseq.git/libraries/deepseqextrahttp://darcs.haskell.org/packages/parallel.git/libraries/parallelhttp://darcs.haskell.org/packages/stm.git/libraries/stm

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