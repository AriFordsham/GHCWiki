[ Video: Getting and Building](http://video.google.com/videoplay?docid=7166458546326012899), layout of the source tree, how to set up build.mk (23'43")

# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ darcs](http://darcs.net/).

## Source distributions


A source distribution is a file like `ghc-6.6-src.tar.bz2`, which contains a complete snapshot of the source tree for a particular version of GHC.  Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download.html).


Starting with GHC 6.6, we have split the source distribution in two:

- `ghc-<version>-src.tar.bz2` contains GHC itself and the minimum libraries needed to bootstrap GHC.
- `ghc-<version>-extralibs.tar.bz2` contains a selection of supplemental libraries that can be built
  and installed at the same time as GHC.  Just unpack this on top of `ghc-<version>-src.tar.bz2`, and
  the extra libraries will be built automatically.


In addition to fixed releases of GHC, source distributions are also made each night from the current source repository, for both the HEAD and STABLE branches.  To download these snapshots, head over to the [download page](http://www.haskell.org/ghc/download.html).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.oirg/happy), so you don't need to install these tools.  See [Building/Prerequisites](building/prerequisites) for details.

## Getting a GHC source tree using darcs


The first thing to do is install [ darcs](http://darcs.net/).


A source tree consists of the GHC repository, with a set of packages in the libraries directory.  We supply a script to automate the checking out of packages, `darcs-all`.  Checking out a tree goes like this:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x darcs-all
  $ ./darcs-all get
```

**NOTE**: you really want `--partial` when grabbing GHC.  There are some 13000 patches in the repository, which take a long time to download without `--partial`.  The `darcs-all` script automatically adds `--partial` for the packages.


The above will grab the "core" set of packages.  This is the minimal set of packages required to bootstrap GHC.  If you want to get a more comprehensive set of packages and include them in your GHC build, then you can say:

```wiki
  $ ./darcs-all --extra get
```


This isn't usually necessary: extra packages can be compiled and installed separately using Cabal, after you have built and installed GHC itself with its core packages.  The list of "core" and "extra" packages is below.


Optionally, you might want to grab the testsuite and benchmark suite too, which should also be sub-directories of ghc:

```wiki
  $ darcs get --partial http://darcs.haskell.org/testsuite
  $ darcs get --partial http://darcs.haskell.org/nofib
```

## List of repositories


These darcs repositories are related to GHC:

<table><tr><th>[ http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc)</th>
<th>The main GHC repository
</th></tr>
<tr><th>[ http://darcs.haskell.org/testsuite](http://darcs.haskell.org/testsuite)</th>
<th>The test suite (requires python 2.4+) 
</th></tr>
<tr><th>[ http://darcs.haskell.org/nofib](http://darcs.haskell.org/nofib)</th>
<th>The benchmark suite
</th></tr></table>


The following repositories are the "core" packages, that populate the libraries directory of a GHC tree:

<table><tr><th>[ http://darcs.haskell.org/packages/base](http://darcs.haskell.org/packages/base)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Cabal](http://darcs.haskell.org/packages/Cabal)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell98](http://darcs.haskell.org/packages/haskell98)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/readline](http://darcs.haskell.org/packages/readline)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-base](http://darcs.haskell.org/packages/regex-base)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-posix](http://darcs.haskell.org/packages/regex-posix)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/regex-compat](http://darcs.haskell.org/packages/regex-compat)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/stm](http://darcs.haskell.org/packages/stm)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/template-haskell](http://darcs.haskell.org/packages/template-haskell)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/unix](http://darcs.haskell.org/packages/unix)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Win32](http://darcs.haskell.org/packages/Win32)</th></tr></table>


And the following repositories are the "extra" packages:

<table><tr><th>[ http://darcs.haskell.org/packages/ALUT](http://darcs.haskell.org/packages/ALUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/GLUT](http://darcs.haskell.org/packages/GLUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HGL](http://darcs.haskell.org/packages/HGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HUnit](http://darcs.haskell.org/packages/HUnit)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HaXml](http://darcs.haskell.org/packages/HaXml)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Japi](http://darcs.haskell.org/packages/Japi)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/ObjectIO](http://darcs.haskell.org/packages/ObjectIO)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenAL](http://darcs.haskell.org/packages/OpenAL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenGL](http://darcs.haskell.org/packages/OpenGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/QuickCheck](http://darcs.haskell.org/packages/QuickCheck)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/X11](http://darcs.haskell.org/packages/X11)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/arrows](http://darcs.haskell.org/packages/arrows)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/fgl](http://darcs.haskell.org/packages/fgl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell-src](http://darcs.haskell.org/packages/haskell-src)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/html](http://darcs.haskell.org/packages/html)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/monads](http://darcs.haskell.org/packages/monads)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/mtl](http://darcs.haskell.org/packages/mtl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/network](http://darcs.haskell.org/packages/network)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/parsec](http://darcs.haskell.org/packages/parsec)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/time](http://darcs.haskell.org/packages/time)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/xhtml](http://darcs.haskell.org/packages/xhtml)</th></tr></table>

### Pulling new patches


To update your tree from the master repositories, the quickest way is to use the `darcs-all` script:

```wiki
  $ ./darcs-all pull -a
```

### Submitting patches


To submit patches to the developers, please use `darcs send`.  You don't need any special permission to do this.

### Committing changes


If you have commit permission (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push` to commit changes directly to the main repository.

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!


Please test changes before committing: you can run a cut-down version of the full test suite like this:

```wiki
  $ cd testsuite
  $ make boot
  $ cd tests/ghc-regress
  $ make fast
```


You need to have `testsuite` checked out, of course.  Running `make fast` should only take a few minutes.

## The 6.6 branch


There is a branch, for the 6.6 series of GHC releases, of the repositories for
ghc, testsuite, nofib and the core libraries in the

<table><tr><th>[ http://darcs.haskell.org/ghc-6.6](http://darcs.haskell.org/ghc-6.6)</th></tr></table>


subtree.


To use this branch instead of the HEAD, replace the first command in the instructions above with

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/ghc
```


and use

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/testsuite
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/nofib
```


to get the testsuite and nofib. Otherwise, the commands needed are identical to those for
working with the HEAD.
