# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ git](http://git-scm.com/).

## Source distributions


A source distribution is a file like `ghc-7.8.3-src.tar.xz`, which contains a complete snapshot of the source tree for a particular version of GHC. Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools (see below for more).

## Quickly getting the GHC source repository


The first thing to do is install [ git](http://git-scm.com/). You'll also need some tools - see [Building/Preparation](building/preparation) for details.

**Note:** The following directions are valid for cloning GHC 7.9 or newer. For cloning GHC 7.8 or earlier, see [below](building/getting-the-sources#).


On Unix-like systems (Linux, OS X), a complete GHC source tree can be obtained (located in `ghc`) by saying:

```wiki
$ git clone --recursive git://git.haskell.org/ghc.git
```


On Windows, you need to additionally fetch an extra repository containing some build tools. After running `clone`:

```wiki
$ cd ghc/
$ git clone git://git.haskell.org/ghc-tarballs.git
```


A source tree consists of more than one repository: at the top level there is the main GHC repository, and certain subdirectories contain separate git repositories (for a list see [Repositories](repositories)).


And then read [Git Working Conventions](working-conventions/git) for instructions on how to use Git with GHC development.

- `git.haskell.org` is reachable via IPv6 as well as IPv4.

- If you're behind a **firewall blocking port 9418** (or `git clone git://...` fails for some other reason), try replacing `git://` by `http://` or `https://` in the instructions above.

- If you have **push access**, see the [Git Working Conventions - Push Access](working-conventions/git#push-access) for info on setting your push URLs.

## Making a local clone

TODO investigate&document `--reference`-based `git clone` which may work better with submodules and doesn't require resetting the origin Repo back to `git.haskell.org`


You can make a local clone of a GHC tree with

```wiki
 $ git clone ~/ghc ~/ghc-branch
```


where `~/ghc` is the repository you want to branch and `~/ghc-branch` is where you want to put the branch. Then use `sync-all` (see [Building/SyncAll](building/sync-all)) to clone the rest of the repositories.  Note that the `origin` for the local clone will point back to the repository that it was cloned from - if you want `origin` to point back to the main GHC repo then do this:

```wiki
  $ ./sync-all -r git://git.haskell.org remote set-url origin
```

## Making a new workdir


I (ezyang) use Git workdirs to manage all of my GHC checkouts. It is quite nice: there is logically only one local repository, and just many checkouts of it, so it's very easy to share patches between various checkouts and your branches are all in one centralized place. However, sharing all of your submodules too takes a bit of work to setup, since Git doesn't natively support this workflow.


Here's what I do:

1. Start by making some pristine GHC checkout (call it `ghc-pristine`) which is an ordinary Git clone of the main GHC repository.

1. Remove all of the submodules that Git recursively created. This is because they are in the wrong format. You can do it with this command: `for i in `git submodule status | cut -d' ' -f3`; do rm -rf $i; done`

1. Re-checkout the submodules using a normal git clone, rather than the submodule tool. This can be done with this command: `for i in `git submodule status | cut -d' ' -f2`; do git clone git://git.haskell.org/`echo "$i" | sed s/libraries/packages/ | sed s/utils\\///`.git $i; done` (On OS X you might have to escape the backslash two more times)

1. Finish off the configuration by running `git submodule init` and `git submodule update`


Now, to create a new workdir, run `git-new-workdir ghc-pristine ghc-newdir`, and then inside `ghc-newdir`, run `for i in `git submodule status | cut -d' ' -f2`; do rmdir $i; git-new-workdir ../ghc-pristine/$i $i; done` to recursively make workdirs of all the submodules.


ToDo: Someone should scriptify this.

## Getting a branch


The above instructions will get the HEAD, the main trunk of GHC development. There is also a branch for each stable release line, as well as branches for development of major new features. The active branches are listed on [ActiveBranches](active-branches).


To get a branch, you need to get from a repo that contains the branch; in particular, local clones of the central repo will normally not include the branches that are in the central repo.


You can clone a specific branch via:

```wiki
  $ git clone -b <branchname> --recursive git://git.haskell.org/ghc.git ghc-<branchname>
```


and switch between branches on an existing clone by

```wiki
  $ git checkout <other-branchname>
  $ git submodule update --init
```

**Note:** The instructions above apply to branches that contain the commit [\[db19c665ec5055c2193b2174519866045aeff09a/ghc\]](/trac/ghc/changeset/db19c665ec5055c2193b2174519866045aeff09a/ghc) which converted all sub-repos into submodules. To clone a branch prior to that commit, follow the instructions below instead. It is best not to attempt to cross that commit with `git checkout`; instead make a fresh clone of the desired branch directly.

## Getting a tag


Starting with GHC 7.10.1, you can simply clone a specific tag via:

`git clone -b ghc-7.10.1-release --recursive git://git.haskell.org/ghc.git ghc-7.10.1`

## Getting a GHC repository from GitHub


The official mirror for GHC on GitHub is located at [ https://github.com/ghc/ghc](https://github.com/ghc/ghc).


First configure the following Git url rewrites to account for the different naming scheme on GitHub (due to GitHub not supporting `/` in repository names) before cloning (those rules are persisted in `${HOME}/.gitconfig` so you need to perform it only once):

```
git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/ 
git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/ 
git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/ 
git config --global url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/ 
git config --global url."git@github.com:/ghc/packages-".insteadOf      git@github.com:/ghc/packages/ 
```


and then simply proceed by

```
git clone --recursive git://github.com/ghc/ghc
```