# darcs-all: operations over all GHC repositories at once


A GHC tree consists of [multiple repositories](darcs-repositories). The `darcs-all` Perl script lets you operate over them all at once.


The basic interface to `darcs-all` is described in comments at the top of the [ source file](http://darcs.haskell.org/ghc/darcs-all).

**Warning for Windows users**: see [\#3899](https://gitlab.haskell.org//ghc/ghc/issues/3899). The bottom line is this

- Make sure that `$(TOP)/_darcs/prefs/defaultrepos` contains an HTTP url like `http://darcs/haskell.org/ghc`, not a SSH address like `darcs.haskell.org:/home/darcs/ghc`.

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
