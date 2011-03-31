# sync-all: operations over all GHC repositories at once


A GHC tree consists of [multiple repositories](repositories). The `sync-all` Perl script lets you operate over them all at once.


The basic interface to `sync-all` is described in comments at the top of the [ source file](http://darcs.haskell.org/ghc/sync-all).

## Pulling new patches


If you have an existing tree, here is how to pull new patches into all repositories.

```wiki
  $ ./sync-all pull
  $ ./sync-all get
```


The second step is required in the event that new packages or repositories have been added to GHC.


See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.


You can also pull patches from another tree:

```wiki
  $ ./sync-all -r /another/ghc pull
```


where `/another/ghc` is a path to another local GHC repository.  You can specify a remote repository here too, e.g. `-r http://darcs.haskell.org/ghc-7.2` (remember to omit the final "ghc.git" when using a remote repo).


The `sync-all` command is useful for finding out what patches you have relative to another repository:

```wiki
  $ ./sync-all -r /another/ghc push --dry-run
```


this tells you which patches there are in your local repository tree relative to the tree over in `/another/ghc`.
