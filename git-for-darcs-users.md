# Git for Darcs Users


Just like Darcs, every Git command comes with a `--help` option.  For example `git add --help`.  You can also check out the [ official Git documentation](http://git.or.cz/gitwiki/GitDocumentation).


Also see "General Notes" below for features present in Git but not in Darcs.

# Git Concepts


In order to understand some commands it is important to compare both Git's and Darcs' internal model.  A Darcs repository is a collection of patches with some dependencies between them.  The working directory is what you get when you apply all those patches in a valid order.


Git on the other hand tracks states of the working directory.  Each commit refers to a particular "version" of the working tree.  So far, this is just a different view of the same thing, in fact, Git internally stores some sort of diffs for space efficiency.  The important difference is that a Git commit also remembers which version we make our patch against, i.e., the parent of the new commit.  Several commits can have the same parent or multiple parents, in which case they are merges.  As a result, a Git repository forms a directed acyclic graph.  These are often depicted in ASCII art like this:

```wiki
          o---o---o---o
         /             \
o---o---A---o---o---o---B---o
```


After commit "A" two developers (or the same developer in different branches) performed different commits on top of the same original version.  The "B" commit is a merge.  If the two branches contained conflicting changes "B" would contain its resolution. 

## Branches


A **branch** in Git is now merely *a pointer to a commit*.  For example, a typical situation is this:

```wiki
                o---o---o <-- feature1
               /
          o---o <-- develop  
         /
o---o---A---o---o---o <-- master
```


Here we have three branches "feature1", "develop", and "master".  "master" is the default branch that is created automatically, when you initialise a new Git repository.  Some commands also default to the master branch for certain actions, but otherwise it is not special in any way.


Note that Git branches all exist in the same repository.  You can have several physical clones of the same repository like in Darcs, but it is often more convenient to work with multiple branches in the same directory.  The most important commands for working with branch are `git checkout` and `git branch`, see below.


The typical Git workflow is to use one branch per feature and merge into their original branch once they are considered ready.  For example, if we think ready that "feature1" is ready, we can `git merge` it into master:

```wiki
$ git checkout develop   # switch to develop branch
$ git merge feature1     # merge feature1 into current branch

          o---o---o---o---o <-- develop, feature1  
         /
o---o---A---o---o---o <-- master
```


The above merge is actually a **fast-forward**, meaning that no new commit is necessary, since "develop" didn't contain any other changes--the pointer for the "develop" branch is merely set to point to the same commit that feature1 pointed to.  On the other hand, if we now merge "devel" into "master" we get a now commit:

```wiki
$ git checkout master
$ git merge devel
          o---o---o---o---o <-- develop, feature1  
         /                 \
o---o---A---o---o---o-------o <-- master
```

## Remote Repositories


XXX: talk about `remote/origin/master`, `fetch`, and how it's mostly just already known `merge`

## Rebase


First of all, `git rebase` is a very dangerous feature, *it should never be done in shared repositories* (this is like `darcs amend-record` and `darcs unpull`, but even harder to fix.)

`git rebase` is a way to "rewrite history".  You can use this to remove bad patches or to move a branch. For example, consider the following history:

```wiki
      A---B---C---D <-- feature1
     /
o---o---o---o---o <-- master
```


Now, it turns out that `B` was not a good patch.  With `git rebase`, you can choose to just remove `B` completely (similar to `darcs unpull`)

```wiki
      A---C'---D' <-- feature1
     /
o---o---o---o---o <-- master
```


or you could edit `B` and replace it with another change `B'` (similar to `darcs amend-record`).

```wiki
      A---B'---C'---D' <-- feature1
     /
o---o---o---o---o <-- master
```


The easiest way is to use `rebase` for these use cases is via `git rebase -i` (see its documentation).


Note in the above examples how patches `C` and `D` got renamed to `C'`, `D'`.  This indicates that they now have different commit ids because they are now are the same patches but based upon a different history.  That means that another user that already has the original patches `C` and `D` and tries to update from the rebased branch will get conflicts!  (This is comparable to how an amended patch in darcs will lead to conflicts, if someone else already has the original patch in the repository.)


Therefore: **Never rebase a published branch**.


Another use case is to move a branch.  In the above example we can rebase `feature1` onto `master`, resulting in this history:

```wiki
                  A'---B'---C'---D' <-- feature1
                 /
o---o---o---o---o <-- master
```


You can do this if the changes in the feature branch semantically aren't really a branch, but make equally sense on top of the master's history.  In the above example, merging `feature1` into `master` will now be a simple fast-forward and will not introduce a merge commit.


Finally, `rebase` can be used to move a feature branch onto another branch.  For example, say we have a big feature and we want to implement a smaller feature:

```wiki
                o---o---o <-- small_feature
               /
          o---o <-- big_feature  
         /
o---o---A---o---o---o <-- master
```


Now we realise that `small_feature` is already useful and want to merge it into master without the commits that were made on the `big_feature` branch.  We can therefore rebase `small_feature` onto master:

```wiki
$ git rebase --onto master big_feature small_feature
          o---o <-- big_feature
         /
        |             o'--o'--o' <-- small_feature
        |            / 
o---o---A---o---o---o <-- master
```


See `git rebase --help` for more usage information and more examples.

### Uh-oh, I trashed my repo with a rebase, what do I do?


Two Options:

1. Recover from the backup copy (you *did* make backup copy before the rebase, didn't you?)

1. In case you were in a hurry and option 1 is not an option for you, there may be some hope.  Git actually has a bit of a functional philosophy in that it doesn't immediately throw away the orignal commits.  Git uses garbage collection for this, which is called automatically every once in a while.


Suppose we started with this repository state

```wiki
      A---B---C---D <-- feature1
     /
o---o---o---o---o <-- master
```


and decided to remove `B` from our history.  The actual repository now looks like this:

```wiki
        C'--D' <-- feature1
       /
      A---B---C---D
     /
o---o---o---o---o <-- master
```


If you happen to have the commit id of `D` (maybe in your console backlog) you can create a branch `feature1_old` that points to `D` via

```wiki
$ git checkout -b feature1_old <commit-id-of-D>
```


Now you have a handle to both `D'` and `D`.

# General Settings


Just like Darcs, Git has global and per-repository configuration options.  To globally set your committer name and email use

```wiki
git config --global user.name "Haskell Curry"
git config --global user.email haskell@example.com
```


Since Git keeps several branches in the same repository it is very useful to [ show the current branch in your shell prompt](http://unboundimagination.com/Current-Git-Branch-in-Bash-Prompt). 

# Git Overview


For an overview of what repositories (or parts of repositories) are modified by various git commands:

> [ http://osteele.com/images/2008/git-transport.png](http://osteele.com/images/2008/git-transport.png)


The git "local repository" corresponds to darcs internal store of patches.  The git "workspace" corresponds to your normal code tree in darcs. The git "index" is a staging area within the current repository, for storing partially-committed hunks.  It has no equivalent in darcs.

# Commands

## darcs init

```wiki
git init
```

## darcs get

```wiki
git clone <repo-url> [<local-name>]
```


Possible repo URLs look like this:

```wiki
git clone http://darcs.haskell.org/ghc.git  # via HTTP (slowest)
git clone git://darcs.haskell.org/ghc.git   # git's protocol (fast, read-only)
git clone [username@]darcs.haskell.org:ghc.git  # via SSH
```

## darcs put


There's no default command to do that, but the following should work:

```wiki
ssh me@remote
cd /some/directory.git
git init --bare
exit
cd my/local/repo
git push me@remote:/some/directory
```


The `--bare` option disables a checkout of the working copy, i.e., only the contents of the `.git` directory are stored.

**Note**: If the repository is supposed to be shared by several users, it's best to init it with either of these commands:

```wiki
git init --bare --shared=group   # everyone in the same group can read and write
git init --bare --shared=all     # same group can read/write, others can read
```


You can also set this after the fact, by setting the configuration variable `core.sharedRepository`.  See `git config --help` for more information.

## darcs add

```wiki
git add <dir-or-file>
```

## darcs record


Git supports interactive recording very similar to darcs.

```wiki
git add -p
```


or

```wiki
git add -i
```


The main difference is that Git does not automatically commit the changes.  You have to do that manually using

```wiki
git commit [-m "commit message"]
```


If you do not supply a commit message, it will open your default editor.  If you want to abort the commit, use an empty commit message.


To see what will be committed, use

```wiki
git diff --cached
```

**Tip**: If you want to see the diff when you edit the commit message, use

```wiki
git commit -v
```

### darcs record -a

```wiki
git commit -a
```


This will add and commit all (not ignored) files.  It will *not* add newly created files. (To do this call `git add .` before in the repo root directory.)

## darcs pull


There is no direct mapping for the interactive `darcs pull`.  Cherry-picking is not as streamlined as in Darcs.

### darcs pull -a


Here is how you update (everything) from the source repo:

```wiki
git pull
```


If all you want to do is to keep updated then this is fine.  The above is actually a shortcut for

```wiki
git pull origin
```


where `origin` is the name of your default remote branch.  (You can name it as you like, but certain Git commands will use `origin` as the default if no argument is specified.)


XXX: will this pull into the current branch, or always into master?  (Websites suggest always into master, so you likely need to follow `git pull` with `git rebase <branch-name>`)


Like in Darcs, you may get conflicts.  To resolve conflicts, edit the conflicting file, `git add` it, and `git commit` it.


If you want to see whether you get conflicts before pulling `git pull` is actually ... XXX

## darcs push


Selectively pushing patches is not available directly in Git.  `git push` does the same as `darcs push -a`.


A comparable interactive workflow is to merge a selection of patches from a local branch into the local master branch and then `git push` that.


In general, even though a central repository is possible, Git promotes a pull model.  That is, to work on a project you typically "fork" (`git clone`) the source repository, add your changes, publish *your* repository, and send a pull-request to the upstream maintainer.  The reasoning behind that is that you don't have something akin to a list of committers, but rather the maintainer has a set of trusted peers.  This model is very different than what seems to be common among darcs users, but it has its advantages.


Obviously, this requires that it's made easy to publish your version of the repository easily.  This is where websites like [ GitHub](http://github.com) come into play.  GitHub is free for open source projects (it offers a paid service with private repos), and makes it particularly easy to share with Git.  GitHub automates things like forking and sending pull requests.  GitHub has a quota of 100 MB, but *a forked repository will not count on your quota*.  This is particularly useful for large code bases like GHC.  (The GitHub quota isn't always correct; so if it seems wrong check again the next day.)

`darcs push` is also used to exchange patches between local repositories.  See "Local Branches" below for how to work with branches in Git.


Of course, you need to be able to publish your local changes to a remote repo (even if it's not the main repo).  This is done using `git push` which is largely equivalent to `darcs push -a`

### darcs push -a

```wiki
git push [<repo-url-or-alias>]
```


Without argument this will push to your `origin`.

## darcs changes

```wiki
git log
git log <file-or-directory>
```

### darcs changes --last \<N\>

```wiki
git log -n <N>
```

### darcs changes --summary

```wiki
git log --stat
```

### darcs changes --match

```wiki
git log --grep="something"
```


(the `=`-sign is important)

### Other useful variants

```wiki
git log -p
```


Shows the patch for each commit.

```wiki
git grep <text>
```


Look for something anywhere in the repository's history (tag names, commit messages, file contents).

```wiki
git show <commit-id>
```


Show the changes by the given patch


More examples.

```wiki
git log v2.5..v2.6            # commits between v2.5 and v2.6
git log v2.5..                # commits since v2.5
git log --since="2 weeks ago" # commits from the last 2 weeks
git log v2.5.. Makefile       # commits since v2.5 which modify
                              # Makefile
```


See `git log --help` for a lot of extra options, to refine the output.

## darcs tag

```wiki
git tag <tagname>
```


This will fail if the tag already exists.  If you want to move an existing tag use `git tag -f <tagname>`, but **never move a tag in a public repo/branch**.  Use this only on local branches, and only if the tag exists nowhere else.  `git tag --help` contains a discussion of this.

## darcs whatsnew

```wiki
git status
```

## darcs diff

```wiki
git diff
```

```wiki
git diff <commit1>..<commit2>  # show diff between two commits
```

## darcs revert


Not sure if this gives you fine-grained reversion of individual hunks:

```wiki
git reset --hard
```

**Note**: `git reset` only resets the staged files, i.e., the things added with `git add`.


To revert one file back to its original state:

```wiki
git checkout <thefile>
```

## darcs unrecord


Not sure if this will unrecord any patches except the most recent???

```wiki
git reset --soft HEAD^
```

## darcs unpull


This can be done with `git rebase`, but consider the consequences of what you are about to do (see "Git Concepts" above).


Given a (partial) history like this

```wiki
A---B---C---D---E <-- mybranch
```


you want to remove patch `C`.  Using interactive rebase, it can be done like this:

```wiki
git rebase -i C^ mybranch
```


This opens your editor with a list of the commits `C`, `D`, and `E`.  By default they are labelled with `pick`.  Now remove the line for patch `C` and save your changes.  Git rebase will now check out version `B` and apply the remaining patches `D` and `E` in order, resulting in the desired outcome:

```wiki
A---B---D'---E' <-- mybranch
```

## darcs amend-record


If the change to be amended is the latest commit

```wiki
git commit --amend
```


If the patch is not the last commit you can also use `git rebase -i` as above, but instead of deleting the line, change `pick` to `edit`.  Rebase will then stop and let you edit the files.  Then do your edits, `git commit --amend`, then continue the rebase with `git rebase --continue`.


You can do a lot more things with `git rebase -i`, like reordering changes or joining or splitting patches.  See the "Interactive Mode" section of `git rebase --help` for more information.

TODO add note for merge commits

## darcs rollback

```wiki
git revert <commit-id>
```


Working directory must be clean.  (You can use `git stash` to save local changes).

## darcs annotate

```wiki
git blame
```

# General Notes

## The Index

## Local Branches

TODO

```wiki
git branch
git branch <name>
git branch -b <name>
git checkout
git branch -d <name>
git branch -D <name>
git stash
git show-branch
```

```wiki
git pull
git fetch
git merge
```

## Suggested Workflow

TODO

- feature branches
- `git rerere`

## Example Workflows

### Fix a bug

```wiki
git pull <upstream>  # get latest changes
git checkout -b fix_bug    # start a branch to fix the bug
# ... hack ...
git add -i           # select the proper changes
git diff --cached    # verify what will be committed
git commit
# ... test ... oops, forgot something
git add -i           # add the new patches
git commit --amend   # add them to previous commit
# ... test ... looks fine now
git checkout master  # back to main branch
git pull             # make sure it's up to date
git merge fix_bug    # merge in our local changes
# ... if we get a conflict here, edit the file then
# ... add the changed files with git add, then git commit
git push             # push changes to personal public repo
                     # or directly to <upstream>
git branch -d fix_foo # delete the branch we no longer need
```


Yes, it is a bit more complicated than using darcs to do the same thing:

```wiki
darcs pull <upstream>  # get latest changes
# ... hack ...
darcs record           # select the proper changes
# ... test ... oops, forgot something
darcs amend-record     # add the new patches
# ... test ... looks fine now
darcs pull             # make sure it's up to date
# ... if we get a conflict here, edit the file then
# ... darcs amend-record
darcs push             # push changes  <upstream>
```