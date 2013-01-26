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
