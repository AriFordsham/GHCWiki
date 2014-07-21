# Workflows for Handling GHC's Git Submodules


GHC is a large project with several external dependencies. We use git submodules to track these repositories, and here you'll learn a bit about how to manage them.


General information about Git's submodule support:

- [ "git submodule" manual page](http://git-scm.com/docs/git-submodule)
- [ Pro Git "6.6 Git Tools - Submodules" chapter](http://git-scm.com/book/en/Git-Tools-Submodules)
- [ Submodule Tutorial](http://www.vogella.com/tutorials/Git/article.html#submodules)

## Cloning a fresh GHC source tree


Initial cloning of GHC HEAD (into the folder `./ghc`) is a simple as:

```
git clone --recursive git://git.haskell.org/ghc.git
```


(Obviously, the clone URL can be replaced by any of the supported `ghc.git` URLs as listed on [ http://git.haskell.org/ghc.git](http://git.haskell.org/ghc.git))

## Updating an existing GHC source tree clone


Sometimes when you pull in new commits, the authors updated a submodule. After pulling, you'll also need to update your submodules, or you'll get errors.


At the top-level of `ghc.git` working copy:

```
git pull --rebase
git submodule update --init
```


In seldom cases it can happen that `git submodule update` aborts with an error similar to the following one

```wiki
fatal: Needed a single revision
Unable to find current revision in submodule path 'libraries/parallel'
```


This means that for some (unknown) reason, the Git submodule in question is in an unexpected/corrupted state. The easiest remedy is remove the named path (or just move it out of the way in case it contains unsaved work), and retry. E.g.

```wiki
rm -rf libraries/parallel
git submodule update --init
```

### Using a Git alias


A commonly defined Git alias that combines the two commands into one convenient Git alias is:

```
git config --global alias.pullall '!f(){ git pull "$@" && git submodule update --init --recursive; }; f'
```


(the `--global` flag make this alias persist in the `${HOME}/.gitconfig` file, so this needs to be done only once, the `--recursive` option is not needed for GHC but it's commonly used for the `pullall` alias)


After setting this alias, one can now simply use the single invocation

```
git pullall --rebase
```


to update `ghc.git` and all its submodules.

## Making changes to GHC submodules


It's very important to keep in mind that Git submodules track commits (i.e. not branches!) to avoid getting confused. Therefore, `git submodule update` will result in submodules having checked out a so-called [ detached HEAD](http://alblue.bandlem.com/2011/08/git-tip-of-week-detached-heads.html).


So, in order to make change to a submodule you can either:

>
> 1) Work directly on the detached HEAD in the submodule directory.

>
> 2) Checkout the respective branch the commit is supposed to be pointed at from (normally `master`).


The example below will demonstrate the latter approach for the `utils/haddock` submodule:

```
# do this *before* making changes to the submodule
cd utils/haddock
git checkout master
git pull --rebase

# perform modifications and as many `git {add,rm,commit}`s as you deem necessary
$EDITOR src/somefile.hs

# finally, after you're ready to publish your changes, simply push the changes as for an ordinary Git repo
git push

# go back to ghc.git top-level
cd ../..
```


At this point, the remote `haddock.git` contains newer commits in the `master` branch, which still need to be registered with `ghc.git`:

```
# if you want, you can inspect with `git submodule` and/or `git status`
# if there are submodules needing attention;
# specifically, the commands below should report new commits in `util/haddock`
git submodule
git submodule summary
git status

# Register the submodule update for the next `git commit` as you would any other file
# Note: You can think of submodule-references as virtual files which 
#       contain a SHA1 string pointing to the submodule's commit.
git add util/haddock

# you can also add other changes in `ghc.git` (e.g. testsuite changes) and/or other submodules 
# you need to update atomically with the next commit
git add testsuite/...

# prepare a commit, and make sure to mention the string `submodule` in the commit message
git commit -m 'update haddock submodule ... blablabla'# finally, push the commit to the remote `ghc.git` repo
git push
```

### Validation hooks


There are server-side validation hooks in place on `git.haskell.org` to make sure for non-`wip/` branches that `ghc.git` never points to non-existing commits. Also, as a safe-guard against accidental submodule reference updates, the string `submodule`**\*must occur somewhere in commit messages of commits**\* updating submodule references. So just remember that:

>
> 1) If you update a submodule pointer,

>
> 2) You had to have pushed it upstream already,

>
> 3) And you have to say the word 'submodule' in the commit.

## Upstream repositories

<table><tr><th>**Location in tree**</th>
<th>**Upstream repo**</th>
<th>**Upstream GHC branch**</th></tr>
<tr><th>utils/hsc2hs</th>
<th>https://git.haskell.org/hsc2hs.git</th>
<th>master</th></tr>
<tr><th>utils/haddock</th>
<th>https://github.com/haskell/haddock</th>
<th>master</th></tr>
<tr><th>nofib</th>
<th>https://git.haskell.org/nofib.git</th>
<th>master</th></tr>
<tr><th>libraries/array</th>
<th>https://git.haskell.org/array.git</th>
<th>master</th></tr>
<tr><th>libraries/binary</th>
<th>https://github.com/haskell/binary</th>
<th>master</th></tr>
<tr><th>libraries/bytestring</th>
<th>https://github.com/haskell/bytestring</th>
<th>master</th></tr>
<tr><th>libraries/Cabal</th>
<th>https://github.com/haskell/Cabal</th>
<th>master</th></tr>
<tr><th>libraries/containers</th>
<th>https://github.com/haskell/containers</th>
<th>master</th></tr>
<tr><th>libraries/deepseq</th>
<th>https://git.haskell.org/deepseq.git</th>
<th>master</th></tr>
<tr><th>libraries/directory</th>
<th>https://git.haskell.org/directory.git</th>
<th>master</th></tr>
<tr><th>libraries/filepath</th>
<th>https://git.haskell.org/filepath.git</th>
<th>master</th></tr>
<tr><th>libraries/haskeline</th>
<th>https://github.com/judah/haskeline</th>
<th>master</th></tr>
<tr><th>libraries/haskell98</th>
<th>https://git.haskell.org/haskell98.git</th>
<th>master</th></tr>
<tr><th>libraries/haskell2010</th>
<th>https://git.haskell.org/haskell2010.git</th>
<th>master</th></tr>
<tr><th>libraries/hoopl</th>
<th>https://git.haskell.org/hoopl.git</th>
<th>master</th></tr>
<tr><th>libraries/hpc</th>
<th>https://git.haskell.org/hpc.git</th>
<th>master</th></tr>
<tr><th>libraries/old-locale</th>
<th>https://git.haskell.org/old-locale.git</th>
<th>master</th></tr>
<tr><th>libraries/old-time</th>
<th>https://git.haskell.org/old-time.git</th>
<th>master</th></tr>
<tr><th>libraries/process</th>
<th>https://git.haskell.org/process.git</th>
<th>master</th></tr>
<tr><th>libraries/terminfo</th>
<th>https://github.com/judah/terminfo</th>
<th>master</th></tr>
<tr><th>libraries/time</th>
<th>https://github.com/haskell/time</th>
<th>master</th></tr>
<tr><th>libraries/unix</th>
<th>https://github.com/haskell/unix</th>
<th>master</th></tr>
<tr><th>libraries/Win32</th>
<th>https://git.haskell.org/Win32.git</th>
<th>master</th></tr>
<tr><th>libraries/xhtml</th>
<th>https://github.com/haskell/xhtml</th>
<th>master</th></tr>
<tr><th>libraries/random</th>
<th>https://github.com/haskell/random</th>
<th>master</th></tr>
<tr><th>libraries/primitive</th>
<th>https://github.com/haskell/primitive</th>
<th>master</th></tr>
<tr><th>libraries/vector</th>
<th>https://github.com/haskell/vector</th>
<th>master</th></tr>
<tr><th>libraries/dph</th>
<th>https://</th>
<th>packages/dph.git</th></tr>
<tr><th>libraries/parallel</th>
<th>https://git.haskell.org/parallel.git</th>
<th>master</th></tr>
<tr><th>libraries/stm</th>
<th>https://git.haskell.org/stm.git</th>
<th>master</th></tr></table>

## TODO

- Describe how to make use of `git submodule update --remote`