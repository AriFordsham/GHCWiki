# How to use git with GHC


GHC uses [ Git](http://git-scm.com/) (version 1.7.3.4 or newer recommended) for revision control. This page describes various suggestions and tips for using Git effectively with the GHC repositories.

# Setup


This page is mostly concerned about using Git for contributing actively to GHC and extends upon the
instructions provided in [Building/GettingTheSources](building/getting-the-sources). See: [WorkingConventions/FixingBugs](working-conventions/fixing-bugs) for how to contribute a patch to GHC.
 

## Push access


If (as a developer) you have been granted push privileges to `git.haskell.org`, you need to take into account that only the `ssh://` URLs support authentication (and hence `git push`ing to).


The following Git URL rewrite rules (which need to be configured only once as they're persisted in the `${HOME}/.gitconfig` file due to `--global`) take care of transparently redirecting `git push`es to the `ssh://` Git URL counterparts:

```
git config --global url."ssh://git@git.haskell.org/".pushInsteadOf git://git.haskell.org/ 
```


This uses the `ssh://` protocol (which has much higher latency due to the SSH handshake occurring for each connect) only for `git push` operations, and the very fast unauthenticated `git://` protocol for everything else (if you originally cloned `git://git.haskell.org/ghc.git`)

- If possible, commit often.  This helps to avoid conflicts.

# Working with the tree

## Submodules


When making changes to other repositories in a GHC tree, see [WorkingConventions/Git/Submodules](working-conventions/git/submodules).

## Branches

#### The stable branch


See [WorkingConventions/Releases](working-conventions/releases).

#### Development branches


See [ActiveBranches](active-branches) for a description of **known** development branches.


The live list of currently **existing** branches in the `ghc` repository can be browsed via [ http://git.haskell.org/ghc.git/heads](http://git.haskell.org/ghc.git/heads).


New development branches names should be prefixed with `wip/` (e.g. "`wip/dependent-types`"), as otherwise the current Git server-side configuration disallows branch deletion and [ non-fast-forward updates](http://stackoverflow.com/questions/4684352/whats-a-fast-forward-in-git).

## Tricks


See [WorkingConventions/Git/Tricks](working-conventions/git/tricks).
