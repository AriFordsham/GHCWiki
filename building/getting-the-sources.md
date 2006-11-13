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


The full list of darcs repositories relating to GHC is at [DarcsRepositories](darcs-repositories).

### Getting a branch


The above instructions will get the HEAD - the main trunk of GHC development.  There are also branches, from which stable releases are made.  The active branches are listed on [DarcsRepositories](darcs-repositories).


To get a branch, add the branch name after [ http://darcs.haskell.org/](http://darcs.haskell.org/).  For example, to get the `ghc-6.6` branch, you would fist say 

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/ghc
```


and then use `darcs-all` as above to get the rest of the respositories.


To get testsuite and/or nofib, you'll need to name the branch repositories:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/testsuite
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/nofib
```

### Pulling new patches


To update your tree from the master repositories, the quickest way is to use the `darcs-all` script:

```wiki
  $ ./darcs-all pull -a
```


See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.
