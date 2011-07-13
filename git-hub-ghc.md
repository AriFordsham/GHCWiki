## Building GHC from GitHub Sources

### Repo Location

[ https://github.com/ghc/ghc](https://github.com/ghc/ghc)

### Getting the Sources


First, go to the GHC repo and create a fork for your account. Then,

```wiki
$ git clone git@github.com:<username>/ghc.git
$ cd ghc
$ ./sync-all -r http://darcs.haskell.org/ get
```