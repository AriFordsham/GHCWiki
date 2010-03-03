# darcs-all: operations over all GHC repositories at once


A GHC tree consists of [multiple repositories](darcs-repositories). The `darcs-all` script lets you operate over them all at once.
To update your tree from the master repositories, the quickest way is to use the `darcs-all` script:

## Pulling new patches


If you have an existing tree, here is how to pull new patches into all repositories.

```wiki
  $ ./darcs-all pull
  $ ./darcs-all get
```


The second step is required in the event that new packages or repositories have been added to GHC.


See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.


You can also pull patches from another tree:

```wiki
  $ ./darcs-all -r /another/ghc pull
```


where `/another/ghc` is a path to another local GHC repository.  You can specify a remote repository here too, e.g. `-r http://darcs.haskell.org/ghc-6.10` (remember to omit the final "ghc" when using a remote repo).


The `darcs-all` command is useful for finding out what patches you have relative to another repository:

```wiki
  $ ./darcs-all -r /another/ghc push --dry-run
```


this tells you which patches there are in your local repository tree relative to the tree over in `/another/ghc`.
