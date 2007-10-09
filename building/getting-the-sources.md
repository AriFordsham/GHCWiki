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


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools.  See [Building/Prerequisites](building/prerequisites) for details.

## Getting a GHC source tree using darcs


The first thing to do is install [ darcs](http://darcs.net/).


A source tree consists of the GHC repository, 
with a set of library packages in the `libraries` directory.  Each of these
libraries has its own repository: see [DarcsRepositories](darcs-repositories).


If you plan to modify GHC, then you **must** get repositories with full history rather than just partial repositories.  (Why?  Because darcs has some bugs that sometimes cause problems when using partial repositories for anything more than just pulling the latest patches.)
However, you cannot use `darcs get` to get a full GHC repository, for two reasons:

- GHC has more than 16,000 patches and the get will take forever. 
- Darcs has a bug concerning the interaction of case-sensitivity and Windows, which makes Darcs crash if you do `darcs get` on the full GHC repository on Windows.


Instead, follow the following steps:

1. Download a complete bundle of the required repositories first, using your browser rather than darcs. These bundles are on [ http://darcs.haskell.org/](http://darcs.haskell.org/) in files of the form `ghc-HEAD-`*date*`-ghc-corelibs-testsuite.tar.bz2`, e.g. `ghc-HEAD-2007-08-29-ghc-corelibs-testsuite.tar.bz2`.
1. Unpack the bundle, which will create a directory called `ghc`.  You can rename this directory freely.
1. Change into the new directory, and pull patches from the main GHC repository:

  ```wiki
     $ cd ghc
     $ darcs pull -a
  ```
1. Now use the `darcs-all` script to pull patches from all the library repositories, and the testsuite repository:

  ```wiki
     $ chmod +x darcs-all
     $ ./darcs-all pull -a
  ```

  The command `darcs-all` automates the fetching of the repositories for the libraries.


If you omit step (3), `darcs-all` will pull patches into the GHC repository too, and if that pulls a patch that modifies the `darcs-all` script itself, then bizarre things can (or at least could in the past) happen.  The safe thing to do is to get your main `ghc` repo up to date (step 3) and then run the script.


If you only want to download the latest sources and aren't interested in working on GHC, then you can get *partial* repositories:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x darcs-all
  $ ./darcs-all get
```


The command `darcs-all` adds the `--partial` flag by default.

## Getting more packages


The above will grab the "core" set of packages and the testsuite.  This is the minimal set of packages required to bootstrap GHC.  If you want to get a more comprehensive set of packages and include them in your GHC build, then you can say:

```wiki
  $ ./darcs-all --extra get
```


This isn't usually necessary: extra packages can be compiled and installed separately using Cabal, after you have built and installed GHC itself with its core packages.  The "core" and "extra" packages are listed in [DarcsRepositories](darcs-repositories).


Optionally, you might want to grab the testsuite (if you have not already got it) and `nofib` benchmark suite too, which also become sub-directories of ghc:

```wiki
  $ ./darcs-all --testsuite get
  $ ./darcs-all --nofib get
```


The full list of darcs repositories relating to GHC is at [DarcsRepositories](darcs-repositories).

### Getting a branch


The above instructions will get the HEAD - the main trunk of GHC development.  There are also branches, from which stable releases are made.  The active branches are listed on [DarcsRepositories](darcs-repositories).


To get a branch, add the branch name after [ http://darcs.haskell.org/](http://darcs.haskell.org/).  For example, to get the `ghc-6.6` branch, you would first say 

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/ghc
```


and then use `darcs-all` as above to get the rest of the respositories.

### Pulling new patches


To update your tree from the master repositories, the quickest way is to use the `darcs-all` script:

```wiki
  $ ./darcs-all pull -a
```


See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.
