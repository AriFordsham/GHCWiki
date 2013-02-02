# Making Releases

## Making the source tarball


The source tarball includes some generated files, such as `Parser.hs` (generated from `Parser.y.pp`). We therefore need to do a build before generating the source tarball.


First [check out the branch](building/getting-the-sources#getting-a-branch), and ensure that the version number and `RELEASE` near the top of `configure.ac` are correct. Then:

```wiki
$ perl boot
$ ./configure
$ make
$ make sdist
```


It is advisable to use a machine with as recent an `autoreconf` as possible; in particular, 2.61 is known to make a configure script that doesn't work on Windows.


You should now have source tarballs `sdistprep/ghc-<VERSION>-src.tar.bz2` and `sdistprep/ghc-<VERSION>-testsuite.tar.bz2`.

## Making the binary builds


Untar the `src` tarball. Then:

```wiki
$ cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BUILD_DOCBOOK_PDF=YES
BUILD_DOCBOOK_PS=YES
EOF
```


If on Linux:

```wiki
$ echo "BeConservative=YES" >> mk/build.mk
```


Then:

```wiki
$ ./configure      2>&1 | tee ../conf.log
$ make             2>&1 | tee ../make.log
$ make binary-dist 2>&1 | tee ../bd.log
```

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


In the `http://www.haskell.org/ghc` darcs repository, create a `download_ghc_<<MANGLED_VERSION>>.shtml` page based on the previous one.


Sanity check `http://www.haskell.org/ghc/download_ghc_<<MANGLED_VERSION>>`. In particular, check that the release notes and documentation links work.

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

## Tagging repositories


Set `RELEASE` back to `NO` and commit.

```wiki
./sync-all tag ghc-7.6.2-release
```


Also `git tag <<LIBRARY>>-<<VERSION>>-release`[libraries that we maintain](repositories) (other than `ghc-prim`).

```wiki
./sync-all push --tags
```

## Uploading libraries


If any library [that we maintain](repositories) (other than bin-package-db, ghc-prim, integer-gmp and integer-simple which don't get uploaded) has been changed, then the new version should be uploaded to hackage.


For example, for `base`, in a built tree:

```wiki
make sdist_base
cabal upload libraries/base/dist-install/*.tar.gz
```