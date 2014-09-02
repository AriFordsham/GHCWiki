# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ git](http://git-scm.com/).

## Source distributions


A source distribution is a file like `ghc-7.8.3-src.tar.xz`, which contains a complete snapshot of the source tree for a particular version of GHC. Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools (see below for more).

## Using git: preliminary setup


Please make sure that you have the correct name and email address set for Git that you want your commits to be recorded as. Make sure you use the same name and email on all your machines so we can easily track a single author. This can be done in Git by running:

```wiki
$ git config --global user.name "Firstname Lastname"
$ git config --global user.email "your_email@youremail.com"
```


Then, **if you are on Windows**, ensure that git handles line-endings sanely by running:

```wiki
$ git config --global core.autocrlf false
```

## Quickly getting the GHC source repository


The first thing to do is install [ git](http://git-scm.com/). You'll also need some tools - see [Building/Preparation](building/preparation) for details.


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

- If you're behind a **firewall blocking port 9418** (or `git clone git://...` fails for some other reason), replace `git://` by `http://` in the instructions above.

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

## Getting a branch


The above instructions will get the HEAD, the main trunk of GHC development. There are also branches, from which stable releases are made. The active branches are listed on [Repositories](repositories).


The commands given below are slightly outdated. Since [\[db19c665ec5055c2193b2174519866045aeff09a/ghc\]](/trac/ghc/changeset/db19c665ec5055c2193b2174519866045aeff09a/ghc) converted all sub-repos into submodules, you can simply clone a specific branch via:

`git clone -b ghc-7.10 --recursive git://git.haskell.org/ghc.git ghc-7.10`


and switch between branches on an existing clone (assuming the current repo checkout and the branch to switch to are both past the commit mentioned above) by

`git checkout <other-branchname>` followed by `git submodule update --init`


To get a branch, you need to get from a repo that contains the branch; in particular, local clones of the central repo will normally not include the branches that are in the central repo.


To get one, run

```wiki
  $ git clone -b branch-name http://git.haskell.org/ghc.git
  $ cd ghc
  $ ./sync-all get -b branch-name
```

## Getting a tag


The commands given below may soon be outdated, starting with GHC 7.10.1, you can simply clone a specific tag via:

`git clone -b ghc-7.10.1-release --recursive git://git.haskell.org/ghc.git ghc-7.10.1`


Each release is tagged in the git repository, making it possible to check out an old version of GHC by tag. To see all available tags, run `git tag` in the GHC repository.


To check out a specific version of GHC, run

```wiki

  $ git clone http://git.haskell.org/ghc.git
  $ cd ghc
  $ git checkout <tag>
  $ ./sync-all get
```


For checking out a tag after you have already done `./sync-all [OPTIONS] get`

```wiki
  $ shopt -s extglob
  $ rm -rf !(.git)
  $ git checkout -f <tag>
  $ ./sync-all get
```

## Getting a GHC repository from GitHub


The official mirror for GHC on GitHub is located at [ https://github.com/ghc/ghc](https://github.com/ghc/ghc).

### New method


You don't need `./sync-all` anymore and can use Git commands directly, if you configure the following Git url rewrites to account for the different naming scheme on GitHub (due to GitHub not supporting `/` in repository names) before cloning (those rules are persisted in `${HOME}/.gitconfig` so you need to perform it only once):

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

### Old method

```wiki
  $ git clone git://github.com/ghc/ghc
  $ cd ghc
  $ ./sync-all get
```


If you want to clone your own fork instead, add an argument to `sync-all` to tell it where it can find the other repositories it needs.

```wiki
  $ git clone <your preferred github.com GHC fork URL> ghc
  $ cd ghc
  $ ./sync-all -r git://github.com/ghc get
```