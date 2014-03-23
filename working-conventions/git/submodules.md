# Workflows for Handling GHC's Git Submodules


This page and as well as the [GitRepoReorganization](git-repo-reorganization) is still work in progress;


See [\#8545](https://gitlab.haskell.org//ghc/ghc/issues/8545) for the current state of affairs

## Cloning a fresh GHC source tree


Initial cloning of GHC HEAD (into the folder `./ghc`) is a simple as:

```
git clone --recursive git://git.haskell.org/ghc.git
```


(Obviously, the clone URL can be replaced by any of the supported `ghc.git` URLs as listed on [ http://git.haskell.org/ghc.git](http://git.haskell.org/ghc.git))


Cloning a specific branch, e.g. `ghc-7.8`; or a specific tag, e.g. `ghc-7.8.1-release`:

```
git clone -b ghc-7.8 --recursive git://git.haskell.org/ghc.git ghc-7.8.x
```

```
git clone -b ghc-7.8.1-release --recursive git://git.haskell.org/ghc.git ghc-7.8.1
```


Older tags/branches which were not fully converted into a submodule-configuration, will require an additional `./sync-all get` step to synchronize.


Moreover, when cloning from the [ GitHub GHC Mirror](http://github.com/ghc/ghc.git), the submodule url paths need to be rewritten, e.g. `../packages/deepseq.git` to `../packages-deepseq.git`, so you can't simply use `--recursive`.

## Updating an existing GHC source tree clone


At the top-level of `ghc.git` working copy:

```
git pull --rebase
git submodule update --init
```