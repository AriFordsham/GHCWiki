# Making Releases

## Branching the tree


Recommended procedure for a clean Git history:

1. Prepare a wip-branch for bumping to the next stable GHC version (NB: You have to update `configure.ac` to a 3-component version, i.e. to `[8.0.0]` rather than `[8.0]`!). Do \*not\* push this branch to a non-wip branch until 1. has been pushed
1. Prepare GHC HEAD for a version bump to the next GHC HEAD major version (e.g. from GHC 7.11 to 8.1). Try to make this into a minimal commit (example: [947c8a530e2f977f56601289e1b11cde42f95322](/trac/ghc/changeset/947c8a530e2f977f56601289e1b11cde42f95322/ghc))
1. After you succeeded with 1., try to rebase your stable wip branch to the commit right before the GHC HEAD version bump.
1. If both worked, and nobody disturbed `master` in the mean-time, push `master`, and then push the new stable `ghc-x.y` branch. If `master` changed, rebase and go back to step 1.
1. create an annotated `ghc-x.yy-start` tag pointing to the commit prepared in step 1.

[](/trac/ghc/attachment/wiki/MakingReleases/branch-example.png)


Example for result of branching procedure described above

### Previous obsolete instructions


Make a `ghc-x.y` branch of all of the repos (including those with a 'dph' and 'extra' tag in `./packages`).


For repos with an upstream maintainer, ask upstream what commit we should be using in the branch.  TODO write updated guide for library release management


Update [ http://ghc.haskell.org/trac/ghc/wiki/Repositories\#Branches](http://ghc.haskell.org/trac/ghc/wiki/Repositories#Branches)

## Make release notes


In `docs/users_guide`, add a `$VERSION-notes.xml` file and write the release notes.


Add a corresponding `relnotes$PATCH_LEVEL` entity to `ug-ent.xml.in`, and use the entity at the end of the chapter in `intro.xml`.


Ensure that the changelogs in `libraries/*` are up-to-date.

## Updating the tree


Update the `ANNOUNCE` file in the root of the tree.


In the `AC_INIT` line of `configure.ac`, set the version number. A few lines below, set `RELEASE=YES`.

## Tagging the release


Create a signed annotated git tag,

```wiki
git tag -asu "Ben Gamari <ben@well-typed.com>" ghc-7.10.2-release HEAD
```


In the case of release candidates an unannotated tag is sufficient (e.g. `git tag ghc-7.10.2-rc1 HEAD`).

## Making the source tarball


The source tarball includes some generated files, such as `Parser.hs` (generated from `Parser.y.pp`). We therefore need to do a build before generating the source tarball.


First [check out the branch](building/getting-the-sources#getting-a-branch), and ensure that the version number and `RELEASE` near the top of `configure.ac` are correct. Then:

```wiki
$ perl boot
$ ./configure
$ ./mk/get-win32-tarballs.sh download all     # ensure all Windows tarballs are available
$ make                                        # GHC <= 7.10 only
$ make sdist
```


It is advisable to use a machine with as recent an `autoreconf` as possible; in particular, 2.61 is known to make a configure script that doesn't work on Windows.


You should now have source tarballs `sdistprep/ghc-<VERSION>-src.tar.bz2` and `sdistprep/ghc-<VERSION>-testsuite.tar.bz2`.


N.B. the `lndir` utility required by `make sdist` is provided by `xutils-dev` on Debian.

## Making the binary builds


First, you should make sure your environment has all of the tools necessary to make a proper release build.  This can include more tools than an "ordinary" build of GHC requires, since documentation requires extra tools.  You will need:

- [ Sphinx](http://www.sphinx-doc.org/)
- HsColour (cabal install hscolour)
- [ xelatex](http://xetex.sourceforge.net/)


A good sanity check is to check the output of `configure` and make sure all of the tool fields are filled out.


Untar the `src` tarball. Then:

```wiki
$ cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_SPHINX_HTML=YES
BUILD_SPHINX_PDF=YES
BeConservative=YES
EOF
```


(Note that `BeConservative` is only relevant on Linux, as it controls usage of `clock_gettime` in the RTS.)


Then:

```wiki
$ ./configure      2>&1 | tee ../conf.log
$ make             2>&1 | tee ../make.log
$ make binary-dist 2>&1 | tee ../bd.log
```


Nightly builders will automatically produce release builds on FreeBSD, putting the results [ here](http://haskell.inf.elte.hu/ghc/dist/freebsd8/).

## Sanity checking the binary builds


The `compare` tool compares the tarballs of different releases, and warns about possible problems:

```wiki
$ cd <</path/to/ghc/tree>>/distrib/compare
$ make
```

```wiki
$ <</path/to/ghc/tree>>/distrib/compare/compare <<previous_release_files>> <<this_release_files>>
```

## Check that the build can build the release


Install the release, set your `$PATH`, then just untar and:

```wiki
$ ./configure
$ make
```

## Create and upload the library documentation

```wiki
haskell.org$ mkdir /srv/web/haskell.org/ghc/docs/<<VERSION>>
```

```wiki
$ <</path/to/ghc/tree>>/distrib/mkDocs/mkDocs ghc-*-x86_64-unknown-linux.tar.bz2 ghc-*-i386-unknown-mingw32.tar.bz2
$ cd docs
$ scp * haskell.org:/srv/web/haskell.org/ghc/docs/<<VERSION>>
```

```wiki
haskell.org$ cd /srv/web/haskell.org/ghc/docs/<<VERSION>>
haskell.org$ mkdir html
haskell.org$ cd html
haskell.org$ mv ../index.html .
haskell.org$ for i in ../*.tar.bz2; do tar -jxf $i; done
```


Sanity check `http://www.haskell.org/ghc/docs/<<VERSION>>/`. In particular, check that the libraries docs include both Win32 and unix.

## Prepare the webpage


In the `http://www.github.com/haskell-infra/ghc-homepage` git repository, create a `download_ghc_<<MANGLED_VERSION>>.shtml` page based on the previous one.


Sanity check `http://www.haskell.org/ghc/download_ghc_<<MANGLED_VERSION>>`. In particular, check that the release notes and documentation links work. Ensure release is in "news" section of `index.html`.


Push your changes to `webhost.haskell.org` (TODO site should pull from git repo).

## Upload the binaries

```wiki
scp -r 7.6.2 haskell.org:/srv/web/haskell.org/ghc/dist/
```


Sanity check that the download links work.

## Announcing


Add the release to "Versions" in the trac admin section, and make it the default version.


Update "Current Stable Release" in `download.shtml`, and move the previous release down to "Older Releases".


Update "Latest News" in `index.shtml`.

```wiki
haskell.org$ ~/mk-latest-links
haskell.org$ ~/mk-latest-links | sh
```


Mail `ANNOUNCE` to `glasgow-haskell-users@haskell.org, haskell@haskell.org`, subject `ANNOUNCE: GHC version <<VERSION>>`.


For an RC, a smaller message is sent to just `glasgow-haskell-users@haskell.org`, e.g.:

```wiki
Subject: ANNOUNCE: GHC x.y.z Release Candidate 1

We are pleased to announce the first release candidate for GHC x.y.z:

    http://www.haskell.org/ghc/dist/x.y.z-rc1/

This includes the source tarball and bindists for Windows, Linux, OS X and FreeBSD, on x86 and x86_64.

We plan to make the x.y.z release <sometime>.

Please test as much as possible; bugs are much cheaper if we find them
before the release!
```

## Uploading libraries

TODOFIXME Normally Herbert and upstream maintainers take care of this - we always attempt to have full releases for all package by the time the final release happens (although not necessarily so for an RC - they may ship slight interim states).


Heuristic for detecting submodule libraries which have no proper release:

```wiki
$ git submodule  | grep -F -- '-g'
 b6658e5d73eb0579b3054593de21f329ab491e77 libffi-tarballs (ghc-7.8.1-release-1-gb6658e5)
 3b573ee058560d1199a19efab10c016278dff252 libraries/Win32 (Win32-2.3.0.2-release-19-g3b573ee)
 33eb2fb7e178c18f2afd0d537d791d021ff75231 libraries/dph (2009-06-25-1149-g33eb2fb)
 29cb0db59803c9d9181f7c4ce35ef1c6cbc6ccfb libraries/primitive (primitive-0.5.2.1-release-22-g29cb0db)
 c0308f1c4f57859d9a8b10d504afe56eebbb27c5 libraries/vector (0_9_1-129-gc0308f1)
 69bae89103aca6e498b811d562f387830fbcb959 nofib (2009-06-25-226-g69bae89)
 f48474f640387dca4b42182c1ac78ba30865742d utils/haddock (haddock-2.16.0-release-32-gf48474f)
 c16032d83c8ce7ac3e11b99f8e80bfdfc77f0d1f utils/hsc2hs (2009-06-25-87-gc16032d)
```


The listing above shows submodules which point to submodules that have no \*annotated\* tag, and which need further investigation. In some cases the tag just wasn't propagated from the upstream repo into our local Git mirror.
