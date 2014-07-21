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
$ cd ghc
$ ./sync-all get
```


A source tree consists of more than one repository: at the top level there is the main GHC repository, and certain subdirectories contain separate git repositories (for a list see [Repositories](repositories)).


And then read [Git Working Conventions](working-conventions/git) for instructions on how to use Git with GHC development.


Note: If you're behind a **firewall blocking port 9418** (or `git clone git://...` fails for some other reason), replace `git://` by `http://` in the instructions above.

### Push access


If you have commit access then you will need to also set the push URL:

```wiki
  $ ./sync-all -r ssh://git@git.haskell.org remote set-url --push origin
```


This uses the `ssh://` protocol (which has much higher latency due to the SSH handshake occurring for each connect) only for `git push` operations, and the very fast unauthenticated `git://` protocol for everything else.


You will probably also want to run

```wiki
  $ git config --global diff.ignoreSubmodules dirty
```


to stop git in the ghc repo from checking for unrecorded changes in the submodules.

## Making a local clone


You can make a local clone of a GHC tree with

```wiki
 $ git clone ~/ghc ~/ghc-branch
```


where `~/ghc` is the repository you want to branch and `~/ghc-branch` is where you want to put the branch. Then use `sync-all` as before to clone the rest of the repositories.  Note that the `origin` for the local clone will point back to the repository that it was cloned from - if you want `origin` to point back to the main GHC repo then do this:

```wiki
  $ ./sync-all -r git://git.haskell.org remote set-url origin
```

## Getting a branch


The above instructions will get the HEAD, the main trunk of GHC development. There are also branches, from which stable releases are made. The active branches are listed on [Repositories](repositories).


To get a branch, you need to get from a repo that contains the branch; in particular, local clones of the central repo will normally not include the branches that are in the central repo.


To get one, run

```wiki
  $ git clone -b branch-name http://git.haskell.org/ghc.git
  $ cd ghc
  $ ./sync-all get -b branch-name
```

## Getting a tag


Each release is tagged in the git repository, making it possible to check out an old version of GHC by tag. To see all available tags, run `git tag` in the GHC repository.


To check out a specific version of GHC, run

```wiki

  $ git clone http://git.haskell.org/ghc.git
  $ cd ghc
  $ git checkout <tag>
  $ ./sync-all --no-dph get
```


For checking out a tag after you have already done `./sync-all [OPTIONS] get`

```wiki
  $ shopt -s extglob
  $ rm -rf !(.git)
  $ git checkout -f <tag>
  $ ./sync-all --no-dph get
```

## Getting a GHC repository from GitHub


The official mirror for GHC on GitHub is located at [ https://github.com/ghc/ghc](https://github.com/ghc/ghc).

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