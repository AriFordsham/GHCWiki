# Workflows for Handling GHC's Git Submodules

GHC is a large project with several external dependencies. We use git submodules to track these repositories, and here you'll learn a bit about how to manage them.

General information about Git's submodule support:

- ["git submodule" manual page](http://git-scm.com/docs/git-submodule)
- [Pro Git "6.6 Git Tools - Submodules" chapter](http://git-scm.com/book/en/Git-Tools-Submodules)
- [Submodule Tutorial](http://www.vogella.com/tutorials/Git/article.html#submodules)


## Cloning a fresh GHC source tree

Initial cloning of GHC HEAD (into the folder `./ghc`) is a simple as:

```
git clone --recursive https://gitlab.haskell.org/ghc/ghc
```

(Obviously, the clone URL can be replaced by any of the supported `ghc.git` URLs as listed on [http://git.haskell.org/ghc.git](http://git.haskell.org/ghc.git))
See [getting the sources](building/getting-the-sources) for more ways of getting the sources (e.g., from the GitHub mirror), and specifically this [note on using forks](building/getting-the-sources#using-a-fork-of-ghc).

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


### `git pullall`

A commonly defined Git alias that combines the two commands into one convenient Git alias is:

```
git config --global alias.pullall '!f(){ git pull --ff-only "$@" && git submodule update --init --recursive; }; f'
```

(the `--global` flag make this alias persist in the `${HOME}/.gitconfig` file, so this needs to be done only once, the `--recursive` option is not needed for GHC but it's commonly used for the `pullall` alias)

After setting this alias, one can now simply use the single invocation

```
git pullall --rebase
```

to update `ghc.git` and all its submodules.


## `git status` and dirty submodules

By default, git will consider your submodule as "dirty" when you do `git status` if it has any changes or any untracked files.  Sometimes this can be inconvenient, especially when using [Phabricator](phabricator) which won't allow you to upload a diff if there are dirty submodules.  Phabricator will let you ignore untracked files in the main GHC repo, but to ignore untracked files in a submodule you'll need a change to `.git/config` in the GHC repo.  For example, to ignore untracked files in the `nofib` repo, add the line `ignore = untracked` to the section for `nofib` in `.git/config`:

```wiki
[submodule "nofib"]
	url = /home/simon/ghc-mirror/nofib.git
        ignore = untracked
```


## Making changes to GHC submodules

It's very important to keep in mind that Git submodules track commits (i.e. not branches!) to avoid getting confused. Therefore, `git submodule update` will result in submodules having checked out a so-called [detached HEAD](http://alblue.bandlem.com/2011/08/git-tip-of-week-detached-heads.html).

So, in order to make change to a submodule you can either:

1) Work directly on the detached HEAD in the submodule directory.

2) Checkout the respective branch the commit is supposed to be pointed at from (normally `master`. See the table on [Repositories](repositories) for the full branch/repo summary). 


If you merely need to update a submodule to point to the latest upstream commit of that submodule, which also takes care to lookup the proper upstream Git branch (in case it's not `master`) as specified in the `.gitmodules` file.
For example, to update `libraries/Cabal`, you can run the following commands:

```wiki
git submodule update --remote libraries/Cabal
git commit libraries/Cabal
git push
```

The example below will demonstrate the latter approach for the `utils/haddock` submodule:

```
# do this *before* making changes to the submodule
cd utils/haddock
git checkout ghc-head
git pull --rebase

# perform modifications and as many `git {add,rm,commit}`s as you deem necessary
$EDITOR src/somefile.hs

# finally, after you're ready to publish your changes, simply push the changes as for an ordinary Git repo
git push

# go back to ghc.git top-level
cd ../..
```

At this point, the remote `haddock.git` contains newer commits in the `ghc-head` branch, which still need to be registered with `ghc.git`:

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
git commit -m 'update haddock submodule ... blablabla'

# for the paranoid, inspect your commit one last time, including the submodule's commit headings
git show --submodule

# finally, push the commit to the remote `ghc.git` repo
git push
```

Git supports a recursive `git push` operation. If you issue a

```wiki
git push --recurse-submodules=on-demand
```

this will cause Git to push all submodules changes that have been registered in the revisions
to be pushed to the super repository.

TODO show how to define a `git pushall` alias in the style of the `git pullall` alias


### Validation hooks

GHC's CI infrastructure includes a submodule lint job which ensures that:

1. If you update a submodule pointer,

2. You had to have pushed it upstream already,

3. And you have to say the word 'submodule' in the commit.

## Upstream repositories

Check out the [Repositories](repositories) page for a full breakdown of all the repositories GHC uses.


## TODO

- Describe darcs mirroring for `transformers`
- Describe status of `pretty` which is one-off at the moment and doesn't exactly track upstream.


## Mirror configuration

GHC maintains mirrors of its core libraries in the GHC/Packages> namespace. There are a few reasons for these mirrors:

 * to provide GHC contributors with a place other than upstream to push their changes for testing in CI
 * to provide a level of safety against forced pushes by upstream
 * to provide a single trusted source for GHC and its dependencies

To accomplish this we configure the submodule projects as follows:

 * The submodule project is configured with pull mirroring from the upstream repository with the "Only mirror protected branches" option enabled
 * All of the "interesting" branches we want to mirror from upstream are added as "Protected branches" (e.g. `master`, and `1.24`)
 * A push rule is added to restrict pushes to the interesting branches and `wip/.*`

This ensures that commits can only be pushed to `wip/.*`, which the submodule check linter does not consider as roots.

## Testing submodule changes

The submodule references in the GHC tree pull from a set of mirrors maintained at <https://gitlab.haskell.org/ghc/packages>. Members of the `ghc` [group](https://gitlab.haskell.org/ghc) (i.e. most GHC developers; if you are not currently a member just [ask](https://gitlab.haskell.org/ghc/ghc/wikis/mailing-lists-and-irc#mailing-lists-and-irc)) can push branches with the prefix `wip/` to these mirrors to test their changes without merging them upstream. For instance, one might test changes in the `haddock` submodule via,
```
$ cd utils/contain
$ git checkout -b wip/my-changes
$ git remote add origin-push git@gitlab.haskell.org:ghc/haddock
$ git push origin-push wip/my-changes
```
One can then push a GHC branch needing `wip/my-changes` and CI will find the commits. Note, however, that this is only for testing purposing. Before the GHC branch can be merged any submodule updates much be first merged upstream.
