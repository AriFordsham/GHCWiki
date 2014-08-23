# sync-all: operations over all GHC repositories at once


A GHC tree consists of [multiple repositories](repositories). The `sync-all` Perl script lets you operate over them all at once. It is in the root directory of the tree.


The full documentation for `sync-all` is found by using the `--help` option:

```wiki
  $ ./sync-all --help
```


(you can also find it by looking in sync-all)

## Pulling new patches


If you have an existing tree, here is how to pull new patches into all repositories.

```wiki
  $ ./sync-all pull
  $ ./sync-all get
```


The second step is required in the event that new packages or repositories have been added to GHC.


See [Building/Rebuilding](building/rebuilding) for how to update your build after pulling patches.


You can also pull patches from another tree, by registering the other tree as a remote, and giving it a name (here `anotherghc`):

```wiki
  $ ./sync-all -r /another/ghc remote add anotherghc
  $ ./sync-all pull anotherghc
```


where `/another/ghc` is a path to another local GHC repository.  You can specify a remote repository here too, e.g. `-r git://github.com/ghc` (remember to omit the final "ghc.git" when using a remote repo).


The `sync-all` command is useful for finding out what patches you have relative to another repository:

```wiki
  $ ./sync-all fetch anotherghc
  $ ./sync-all new anotherghc
```


this tells you which patches there are in your local repository tree relative to the tree over in `/another/ghc`.
