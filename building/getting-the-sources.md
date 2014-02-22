# Getting the GHC Sources


There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [ git](http://git-scm.com/).

## Source distributions


A source distribution is a file like `ghc-7.0.4-src.tar.bz2`, which contains a complete snapshot of the source tree for a particular version of GHC. Source distributions for all versions of GHC are available from the [download page](http://www.haskell.org/ghc/download).
In addition to fixed releases of GHC, source distributions are also made each night from the current source repository, for both the HEAD and STABLE branches. To download these snapshots, head over to the [download page](http://www.haskell.org/ghc/download).


Source distributions are easier to build, because we also include the output from running certain external tools like [ Happy](http://haskell.org/happy), so you don't need to install these tools. See [Building/Preparation](building/preparation) for details.

## Getting a GHC repository using git


The first thing to do is install [ git](http://git-scm.com/). And then read [Git Working Conventions](working-conventions/git) for instructions on how to use Git with GHC development.


Please make sure that you have the correct name and email address set for Git that you want your commits to be recorded as. Make sure you use the same name and email on all your machines so we can easily track a single author. This can be done in Git by running:

```wiki
$ git config --global user.name "Firstname Lastname"
$ git config --global user.email "your_email@youremail.com"
```


Then, if you are on Windows, ensure that git handles line-endings sanely by running:

```wiki
$ git config --global core.autocrlf false
```


A source tree consists of more than one repository: at the top level there is the main GHC repository, and certain subdirectories contain separate git repositories (for a list see [Repositories](repositories)). To get a complete repository tree using git:

```wiki
  $ git clone git://git.haskell.org/ghc.git
  $ cd ghc
  $ ./sync-all get
```


Note: If you're behind a **firewall blocking port 9418** (or `git clone git://...` fails for some other reason), replace `git://` by `http://` in the instructions above.


Note 2: The `--testsuite` option is gone ([\#8545](https://gitlab.haskell.org//ghc/ghc/issues/8545)).


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
  $ VERSION=7.4.2
  $ git clone http://git.haskell.org/ghc.git
  $ cd ghc
  $ git checkout -b ghc-${VERSION} ghc-${VERSION}-release
  $ ./sync-all --no-dph get
  $ ./sync-all checkout -b ghc-${VERSION} ghc-${VERSION}-release
```

## Getting a GHC repository from GitHub


To get GHC repository from [ GitHub](http://www.github.com) we recommend to just get GHC source code itself from GitHub and combine this with packages from [ http://git.haskell.org](http://git.haskell.org). This is easy since sync-all script supports it well.

```wiki
  $ git clone <your preferred github.com GHC fork URL> ghc
  $ cd ghc
  $ ./sync-all -r git://git.haskell.org get
```


If you want a full clone of every package from GitHub instead, sync-all can handle that as well.

```wiki
  $ git clone git://github.com/ghc/ghc
  $ cd ghc
  $ ./sync-all -r git://github.com/ghc get
```


The official mirror for GHC on github is located at [ https://github.com/ghc/ghc](https://github.com/ghc/ghc).

## Pulling new patches


The [sync-all](building/sync-all) script makes it easy to pull new patches. For example, `sync-all pull` will pull all new patches from the original repository into the repository tree in the current directory.

## Tracking the full repository state


The full state of a GHC repository includes the current state of the repositories for all of the GHC boot libraries that are used to to build GHC ([list of boot libraries](repositories)). The repositories for these libraries are fetched and updated by the `sync-all` script. To record the full repository state (including boot libraries), git submodules could be used, but they are not currently in favor (see [GHC Team perspective on submodules](darcs-conversion#the-perspective-on-submodules) for some reasons why).


As an alternative to git submodules, the `fingerprint.py` script in `utils/fingerprint/` can create a "fingerprint" to uniquely identify a GHC repository state by recording the current commits of the GHC and boot library repositories. This fingerprint can be used later to restore the state of all repositories to the state captured by the fingerprint. 


To create a new fingerprint, run the `create` command in the top level ghc repo. The fingerprint can also be created from a [Builder](builder) log that contains the appropriate output from the `sync-all` command by passing the log file to the create command with the `-l` flag.

```wiki
$ ./utils/fingerprint/fingerprint.py create
$ ./utils/fingerprint/fingerprint.py create -l builder.log
```


This command will create a new fingerprint, which is just pairs of repositories and commits. 


To restore a fingerprint use the `restore` command and pass either a fingerprint file with the `-f` flag or a builder log file with the `-l` flag.

```wiki
$ ./utils/fingerprint/fingerprint.py restore -f 2011-05-23.fp
$ ./utils/fingerprint/fingerprint.py restore -l builder.log
```


This command will read the fingerprint and perform a checkout of the appropriate commit for each repository found in the fingerprint. By default, the `restore` command will create a new branch in the top level ghc repository and add an entry to git config that sets the new branch's remote to `origin`. The config options are added so that future `sync-all` commands will work as expected. Passing the `-n` flag will cause the fingerprint script not to create a new branch. To "unrestore" a fingerprint, simply use `sync-all` to checkout the `master` branch in each repository

```wiki
$ ./sync-all checkout master
```


To make the best use of fingerprinting, you need to collect fingerprints for the states you may wish to restore. To ease the automatic collection of fingerprints, the `fingerprint.py` script allows a `-d DIR` option that will output a fingerprint to the directory `DIR` with the current time stamp as a file name. The `-g DIR` option tells the script to run in the ghc repository pointed to by `DIR`. These options can be useful for collecting fingerprints as a cron job or on a post-commit hook.


To restore the repository state as of a particular date, you can grab a fingerprint emitted by one of the nightly builds.  The nightly build emails are sent to [ the ghc-builds mailing list](http://www.haskell.org/pipermail/ghc-builds/), and have subjects like "\[nightly\] 06-Oct-2012 build of HEAD on x86_64-unknown-linux (cam-04-unx)".  Copy and paste the fingerprint section, which looks something like this:

```wiki
Respository hashes:
.|4a138b708463a99a1087ce2d8a70239de3aa04e4
ghc-tarballs|18e0c37f8023abf469af991e2fc2d3b024319c27
libraries/Cabal|31c3f404dceb221d7281467a66bf68cc166fc2d9
libraries/Win32|21335a30161c099da79ae9619c9782e5e32e4644
libraries/array|e4b800af700cd7a5d42286086f17713ec98698a0
libraries/base|219ea0e539aca217fe1819baf02d4cdd1d670f51
... and so on
```


Put it in a file, and run `./utils/fingerprint.py restore -f <file>` to restore your repository tree to that state.
