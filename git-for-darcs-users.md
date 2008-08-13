# Git for Darcs Users


Just like Darcs, every Git command comes with a `--help` option.  For example `git add --help`.  You can also check out the [ official Git documentation](http://git.or.cz/gitwiki/GitDocumentation).


Also see "General Notes" below for features present in Git but not in Darcs.

# General Settings


Just like Darcs, Git has global and per-repository configuration options.  To globally set your committer name and email use

```wiki
git config --global user.name "Haskell Curry"
git config --global user.email haskell@example.com
```


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
cd /some/directory
git init
exit
cd my/local/repo
git push me@remote:/some/directory
```

**Note**: If the repository is supposed to be shared by several users, it's best to init it with either of these commands:

```wiki
git init --shared=group   # everyone in the same group can read and write
git init --shared=all     # same group can read/write, others can read
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


There is a direct mapping for `darcs pull -a`, but not for the interactive `darcs pull`.  Cherry-picking is not as streamlined as in Darcs.  For a start, here is how you update from the source repo:

```wiki
git pull
```


If all you want to do is to keep updated then this is fine.  The above is actually a shortcut for

```wiki
git pull origin
```


where `origin` is the name of your default remote branch.  (You can name it as you like, but certain Git commands will use `origin` as the default if no argument is specified.)


XXX: will this pull into the current branch, or always into master?


Like in Darcs, you may get conflicts.  To resolve conflicts, edit the conflicting file, `git add` it, and `git commit` it.


If you want to see whether you get conflicts before pulling `git pull` is actually ... XXX

## darcs push


Selectively pushing patches is not available directly in Git.  In general, Git promotes a pull model, that is, to work on a project you typically "fork" (`git clone`) the source repository, add your changes, publish *your* repository, and send a pull-request to the upstream maintainer.  The reasoning behind that is that you don't have something akin to a list of committers, but rather the maintainer has a set of trusted peers.  This model is very different than what seems to be common among darcs users, but it has its advantages.


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

## darcs unrecord


Not sure if this will unrecord any patches except the most recent???

```wiki
git reset --soft HEAD^
```

## darcs unpull


unknown

## darcs amend-record


If the change to be amended is the latest commit

```wiki
git commit --amend
```

TODO describe workflow if amended patch is not the current HEAD.

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