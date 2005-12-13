## Using the GHC darcs repository


There are several darcs repositories related to GHC:

<table><tr><th>[ http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc)</th>
<th>The main GHC repository
</th></tr>
<tr><th>[ http://darcs.haskell.org/libraries](http://darcs.haskell.org/libraries)</th>
<th>The libraries
</th></tr>
<tr><th>[ http://darcs.haskell.org/testsuite](http://darcs.haskell.org/testsuite)</th>
<th>The test suite
</th></tr>
<tr><th>[ http://darcs.haskell.org/nofib](http://darcs.haskell.org/nofib)</th>
<th>The benchmark suite
</th></tr></table>


Additionally, we have the following branches:

<table><tr><th>[ http://darcs.haskell.org/ghc.ghc-6.4](http://darcs.haskell.org/ghc.ghc-6.4)</th>
<th>6.4 branch of GHC
</th></tr>
<tr><th>[ http://darcs.haskell.org/libraries.ghc-6.4](http://darcs.haskell.org/libraries.ghc-6.4)</th>
<th>6.4 branch of the libraries
</th></tr></table>


A source tree consists of the GHC repository, with libraries as a sub-directory:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ darcs get --partial http://darcs.haskell.org/libraries
```

**NOTE**: you really want `--partial`, at least when grabbing GHC.  There are some 13000 patches in the repository, which take a long time to download without `--partial`.


Optionally, you might want to grab the testsuite and benchmark suite too, which should also be sub-directories of ghc:

```wiki
  $ darcs get --partial http://darcs.haskell.org/nofib
  $ darcs get --partial http://darcs.haskell.org/testsuite
```


To submit patches to the developers, please use `darcs send`.
