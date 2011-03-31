[ Video: Getting and Building](http://video.google.com/videoplay?docid=7166458546326012899), layout of the source tree, how to set up build.mk (23'43")

# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ git](http://git-scm.com/).

## Source distributions


A source distribution is a file like `ghc-6.12.2-src.tar.bz2`, which contains a complete snapshot of the source tree for a particular version of GHC.  Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download.html).


In addition to fixed releases of GHC, source distributions are also made each night from the current source repository, for both the HEAD and STABLE branches.  To download these snapshots, head over to the [download page](http://www.haskell.org/ghc/download.html).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools.  See [Building/Preparation](building/preparation) for details.

## Getting a GHC repository using git


The first thing to do is install [ git](http://git-scm.com/).


A source tree consists of more than one repository: at the top level there is the main GHC repository, and certain subdirectories contain separate git repositories (the full list of git repositories relating to GHC is at [Repositories](repositories)).  To get a complete repository tree using git:

```wiki
  $ git clone http://darcs.haskell.org/ghc-git/ghc.git/
  $ cd ghc
  $ chmod +x sync-all
  $ ./sync-all --testsuite get
```

## Making a local clone


You can make a local clone of a GHC tree with

```wiki
 $ git clone ~/ghc ~/ghc-branch
```


where `~/ghc` is the repository you want to branch and `~/ghc-branch` is where you want to put the branch.  Then use `sync-all` as before to branch the rest of the repositories.  You can then use `sync-all -r <path>` to push and pull patches between your two local repository trees.

## Getting a branch


The above instructions will get the HEAD - the main trunk of GHC development.  There are also branches, from which stable releases are made.  The active branches are listed on [Repositories](repositories).


To get a branch, add the branch name after [ http://darcs.haskell.org/](http://darcs.haskell.org/).  For example, to get the `ghc-7.2` branch, you would first say 

```wiki
  $ git clone http://darcs.haskell.org/ghc-7.2/ghc.git
```


and then use `sync-all` as above to get the rest of the repositories.

## Pulling new patches


The [sync-all](building/sync-all) script makes it easy to pull new patches.  For example, `sync-all pull` will pull all new patches from the original repository into the repository tree in the current directory.

## Windows


Note, on Windows you may have to change git's line-ending behaviour first:

```wiki
git config --global core.autocrlf false
```


since this is a global setting, you probably want to change it back after cloning ghc, and then set it locally for the GHC repo(s).
