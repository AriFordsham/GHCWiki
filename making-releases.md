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
```


If on Linux:

```wiki
echo "BeConservative=YES" >> mk/build.mk
```


Then:

```wiki
$ ./configure      2>&1 | tee ../conf.log
$ make             2>&1 | tee ../make.log
$ make binary-dist 2>&1 | tee ../bd.log
```