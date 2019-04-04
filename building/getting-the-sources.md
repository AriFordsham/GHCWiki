# Getting the GHC sources

There are two ways to get GHC sources:
1. downloading a [source distribution](#via-source-distributions)
2. cloning [Git repositories](#via-git-repositories)

## Via source distributions

A source distribution is a file like `ghc-7.8.3-src.tar.xz`, which contains a complete snapshot of the source tree for a particular version of GHC. All the source distributions we provide are available from the [download page](http://www.haskell.org/ghc/).

Pros: source distributions are easy to build, because they contain the output from running certain external tools like [Happy](http://haskell.org/happy), so you don't need to install these tools.

Cons: source distributions are stuck to a particular GHC version and shouldn't be used to contribute to GHC.


## Via Git repositories

GHC uses [Git](http://git-scm.com/) for revision control (version 1.7.8 or newer recommended). You must install it on your platform before executing any "git" commands.

**Important:** on Windows you must configure Git to check out Unix-style line endings before executing the following commands (the default behaviour is to check out Windows-style line endings) otherwise you will likely encounter issues during the build. This can be changed using the following command: ``git config --global core.autocrlf false`` (set it to "true" the same way to restore the default behavior).

The main GHC repository is located on a Gitlab instance on haskell.org and a complete GHC source tree can be obtained (located in `ghc`) by running the following command:

```
git clone --recursive https://gitlab.haskell.org/ghc/ghc
cd ghc # ensure you are in the ghc source tree for the following commands
```

Notice that we need the unusual "--recursive" flag because GHC uses the "submodule" feature of Git. Using the "--recursive" flag ensures that submodules are cloned too (e.g. into ``libraries/..``). If you forgot the flag, you can perform the same operation later on with the following command:

```
git submodule update --init
```

The above instructions will checkout the HEAD (or "master"), the main trunk of GHC development. There is also a branch for each stable release line, as well as branches for development of major new features. You can list the available branches with:

```
git branch -a
```

and switch between branches with:

```
git checkout <other-branchname>
git submodule update --init
```

You can update your fork of the repository with:

```
git fetch origin
# then checkout, rebase or merge the branch you want (out of the scope of this page)
```

Note: see [this page](active-branches) for a more thorough description of some branches available in the main repository

Note: [this page](repositories) gives a list of all the repositories used by GHC as Git submodules. 

Note: for cloning GHC 7.8 or earlier, see the [legacy](building/getting-the-sources/legacy) instructions.


### Cloning from GitHub

Cloning from GitHub is also supported. The official mirror for GHC on GitHub is located at [https://github.com/ghc/ghc](https://github.com/ghc/ghc).

First configure the following Git url rewrites to account for the different naming scheme on GitHub (due to GitHub not supporting `/` in repository names) before cloning (those rules are persisted in `${HOME}/.gitconfig` so you need to perform it only once):

```
git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/ 
git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/ 
git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/ 
git config --global url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/ 
git config --global url."git@github.com:ghc/packages-".insteadOf       git@github.com:ghc/packages/ 
```

and then simply proceed by

```
git clone --recursive git://github.com/ghc/ghc
```