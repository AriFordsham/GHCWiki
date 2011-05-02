# GIT notes by Simon PJ


Here are notes about using Git that Simon PJ has found useful.

---

## Configuration

### Push only the current branch


When you say `git push` (with no arguments), push only only patches on
the *current branch*.  If you have un-pushed commits on other branches, leave them be.

```wiki
git config --global remote.origin.push HEAD
```

### Creating tracking branches


Suppose you create a new branch on your local machine. Now you want to push
it up to the global repo.  You almost certainly want your local branch to become
a tracking option of the remote one, so that `git pull` will merge changes to
the remote copy into your local copy.

```wiki
git config --global branch.autosetupmerge true
```

---

## Looking at the current state of affairs

### Show one-line-per-file diff summary


Show a one-line-per-file summary of diffs between working files and the local repo:

```wiki
git diff --stat
```

### Show delta between branch and trunk


Show the commits that are on branch `my-test` but not on the main trunk:

```wiki
git log `git merge-base master my-test`..my-test
```


The `git merge-base b1 b2` thing returns the name of the commit that is the common ancestor of branches `b1` and `b2`.

---

## Doing useful things

### Create a branch after doing some edits


You are sitting on a branch (say master), and do some edits. Now you decide it wasn't as simple as you thought so you want to create a branch to keep your edits safe while you do something else. 

```wiki
git checkout -b <new-branch-name>
```


This creates the new branch and switches to it, but **does not change your working files**.  Now you can safely commit on the branch [ Stackoverflow link](http://stackoverflow.com/questions/2569459/git-create-a-branch-from-unstagged-uncommited-changes-on-master).  Then to push to the master repo:

```wiki
git push origin <new-branch-name>
```


That will create `<new-branch-name>` in the master repo if it does not already exist.

### Work on a branch gotten from the main repo


You have done a `git fetch` to get the upstream repo, which has a branch `origin/experiment`.  You want a local `experiment` branch which tracks `origin/experiment`:

```wiki
git branch --track experiment origin/experiment
```


Now you can `git checkout experiment` to move onto your local `experiment` branch.

### Connect up a local branch with its remote counterpart


You are on local branch `experiment` and do `git pull` to pull down changes from `origin/experiment`, but you get this:

```wiki
You asked me to pull without telling me which branch you
want to merge with, and 'branch.experiment.merge' in
your configuration file does not tell me, either. Please
```


Somehow you in a state where `experiment` isn't tracking `origin/experiment`.  To make it tracking, use `--set-upstream`:

```wiki
git branch --set-upstream experiment origin/experiment
```

---

## `git gui` on Windows


I’ve been using `git gui` (on Windows at least) as a way to examine and stage changes.  But I suddenly found that it wasn’t displaying the diff in the main pane.  


A google search [ http://code.google.com/p/msysgit/issues/detail?id=394](http://code.google.com/p/msysgit/issues/detail?id=394) suggested that (bizarrely) it might have something to do with ‘nice’.  


So I renamed `c:/cygwin/bin/nice.exe to c:/cygwin/bin/cygin-nice.exe`, and that made `git gui` worked fine.  Wierd.
