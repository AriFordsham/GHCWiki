[ Video: Getting and Building](http://video.google.com/videoplay?docid=7166458546326012899), layout of the source tree, how to set up build.mk (23'43")

# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ darcs](http://darcs.net/).

## Source distributions


A source distribution is a file like `ghc-6.6-src.tar.bz2`, which contains a complete snapshot of the source tree for a particular version of GHC.  Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download.html).


Starting with GHC 6.6, we have split the source distribution in two:

- `ghc-<version>-src.tar.bz2` contains GHC itself and the minimum libraries needed to bootstrap GHC.
- `ghc-<version>-src-extralibs.tar.bz2` contains a selection of supplemental libraries that can be built
  and installed at the same time as GHC.  Just unpack this on top of `ghc-<version>-src.tar.bz2`, and
  the extra libraries will be built automatically.


In addition to fixed releases of GHC, source distributions are also made each night from the current source repository, for both the HEAD and STABLE branches.  To download these snapshots, head over to the [download page](http://www.haskell.org/ghc/download.html).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools.  See [Building/Preparation](building/preparation) for details.

## Getting a GHC source tree using darcs


The first thing to do is install [ darcs](http://darcs.net/).


A source tree consists of the GHC repository, 
with a set of library packages in the `libraries` directory.  Each of these
libraries has its own repository: see [DarcsRepositories](darcs-repositories).


If you only want to download the latest sources and aren't interested in working on GHC, then you can get *partial* repositories:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x darcs-all
  $ ./darcs-all --testsuite get
```


The [darcs-all script](building/darcs-all) adds the `--partial` flag by default.


The full list of darcs repositories relating to GHC is at [DarcsRepositories](darcs-repositories).


If you plan to modify GHC, then you **must** get repositories with full history rather than just partial repositories.  (Why?  Because darcs has some bugs that sometimes cause problems when using partial repositories for anything more than just pulling the latest patches.)
However, **you cannot use `darcs get` to get a full GHC repository**, for two reasons:

- GHC has more than 16,000 patches and the `darcs get` will take forever. 
- Darcs prior to version 2.3 has a bug concerning case-sensitivity on Windows, and ([ apparently](http://www.haskell.org/pipermail/glasgow-haskell-users/2007-November/013373.html)) MacOS X, which makes Darcs crash if you do `darcs get` on the full GHC repository.  You get this message

  ```wiki
  Applying patch 12 of 17349... Unapplicable patch:
  Thu Jan 11 07:26:13 MST 1996  partain
    * [project @ 1996-01-11 14:06:51 by partain]
  ```

  In darcs verison 2.3 and later, `darcs get` uses the hashed repository format by default, which is not subject to the case-sensitivity bug.


On MacOS X this can be [worked around using filesystem tricks](building/mac-osx#case-insensitivity).  A way to  work around the problem on any system is to follow the following steps:

1. Download a complete bundle of the required repositories first, using your browser rather than darcs. These bundles are on [ http://darcs.haskell.org/](http://darcs.haskell.org/) usually in three files of the form 

  - `ghc-HEAD-2007-08-29-ghc-corelibs-testsuite.tar.bz2` (100Mbytes)
  - `ghc-HEAD-2007-08-29-ghc-corelibs.tar.bz2` (90 Mbytes)
  - `ghc-HEAD-2007-08-29-ghc.tar.bz2` (60 Mbytes)

>
> Each of these is a subset of the previous one; pick the smallest one that has what you need. Note that you need the corelibs to build GHC; the only reason not to get a tarball that includes them is if you want to do `--partial` gets of them to save a little disk space. Of course, the dates may vary.

1. Unpack the bundle, which will create a directory called `ghc`.  You can rename this directory freely.
1. Change into the new directory, and pull patches from the main GHC repository:

  ```wiki
     $ cd ghc
     $ darcs pull -a
  ```
1. Some core libraries might have been added to HEAD which were not in the last tarball. This means that after doing the last pull (which updates the list of core libraries) we need to do this to get any new libraries:

  ```wiki
     $ chmod +x darcs-all
     $ ./darcs-all get
  ```
1. Now use the `darcs-all` script to pull patches from all the library repositories that came in the tarball, and the testsuite repository:

  ```wiki
     $ ./darcs-all pull -a
  ```

  The command `darcs-all` automates the fetching of the repositories for the libraries.


If you omit step (3), then `darcs-all` will pull patches into the GHC repository too. If one of those patches modifies the `darcs-all` script itself, then bizarre things can happen (or at least: in the past, they could happen.) The safe thing to do is to get your main `ghc` repo up to date (step 3) and then run the script.


If you see any of the following error messages on Windows (the filename varies)

```wiki
Warning: ./compiler/main/DynFlags.hs-0: renameFile: permission denied (Permission denied)
darcs failed:  Error applying hunk to file ./compiler/main/DynFlags.hs
Error applying patch to the working directory.
```

```wiki
darcs failed:  ./ghc.mk-0: renameFile: permission denied (Permission denied)
Your repository is now in an inconsistent state.
This must be fixed by running darcs repair.
```


after doing `darcs pull -a`, then use newest darcs. Version 2.0.2 is broken, version 2.3.1 works.

## Getting a GHC source tree using git


NOTE: This is not yet supported. We currently recommend you use darcs to get a source tree.


The first thing to do is install [ darcs](http://darcs.net/) and [ git](http://git.or.cz/).

```wiki
git clone http://darcs.haskell.org/ghc.git ghc
cd ghc
./sync-all --complete get
sh boot
./configure && make
```


Note, on Windows you may have to change git's line-ending behaviour first:

```wiki
git config --global core.autocrlf false
```


since this is a global setting, you probably want to change it back after cloning ghc, and then set it locally for the GHC repo(s).

## Getting a branch


The above instructions will get the HEAD - the main trunk of GHC development.  There are also branches, from which stable releases are made.  The active branches are listed on [DarcsRepositories](darcs-repositories).


To get a branch, add the branch name after [ http://darcs.haskell.org/](http://darcs.haskell.org/).  For example, to get the `ghc-6.6` branch, you would first say 

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/ghc
```


and then use `darcs-all` as above to get the rest of the respositories.

## Pulling new patches


The [darcs-all](building/darcs-all) script makes it easy to pull new patches.
