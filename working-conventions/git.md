# Guidelines for using git with GHC


GHC uses [ Git](http://git-scm.com/) (version 1.7.3.4 or newer recommended) for revision control. This page describes various GHC-specific conventions for using git, together with some suggestions and tips for using git effectively.

# Setup

## General Guidelines

- Try to make small patches (i.e. work in consistent increments).

- Separate changes that affect functionality from those that just affect
  code layout, indentation, whitespace, filenames etc.  This means that
  when looking at patches later, we don't have to wade through loads of
  non-functional changes to get to the important parts of the patch.   

- If possible, commit often.  This helps to avoid conflicts.

- Only push when your tree passes validation: see [TestingPatches](testing-patches).

- Discuss anything you think might be controversial before pushing it.

- When making changes to other repositories in a GHC tree, see [WorkingConventions/Git/Submodules](working-conventions/git/submodules).

## Author


Please make sure you have setup git to use the correct name and email for your commits. Use the same name and email on all machines you may push from.

```wiki
$ git config --global user.name "Firstname Lastname" # Sets the name of the user for all git instances on the system
$ git config --global user.email "your_email@youremail.com"
```


This will set your name and email globally. To set it for just the GHC repo, remove the `--global` flag. Also, the environment variables `GIT_COMMITTER_NAME`, `GIT_COMMITTER_EMAIL`, `GIT_AUTHOR_NAME` and `GIT_AUTHOR_EMAIL` will override git-config settings if they are defined.

## Line endings


Files in GHC repos should use Unix conventions for line endings.
If you are on Windows, ensure that git handles line-endings sanely by running:

```wiki
git config --global core.autocrlf false
```


To find out what files in your tree have windows (CRLF) line endings,  use

```wiki
find . -name '*hs' | xargs file | grep CRLF
```


Do this before you commit them!

## Push access


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

# Working with the tree

## Tricks


See [WorkingConventions/Git/Tricks](working-conventions/git/tricks).

## Commit messages


Please try to follow the general convention for the [ Git commit message structure](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html) as many Git tools rely on this. Moreover, take into account that the commit message text is interpreted as [WikiFormatting](wiki-formatting) in Trac.


In particular, if your patch addresses or fixes a bug/ticket, then include the ticket number in the form "`#NNNN`" in the commit message, e.g.

```wiki
  withMVar family have a bug (fixes #767)
```

***Git will then add a link to the commit from the ticket*** (as soon as the commit becomes reachable from the `master` HEAD), so that people watching the ticket can see that a fix has been committed, and in the future we can easily find the patch that addressed the ticket.  When navigating the Git history on Trac, you will also be able to jump directly to the ticket from the commit.

## Contributing patches


Please write your patch and then rebase to the latest version of GHC HEAD before sending to us. You can use the following command to send patches via email:

```wiki
git send-email --to=ghc-devs@haskell.org <hash-id> -1
```


where `<hash-id>` is the hash of the commit to send. If you'd prefer to create patch files and send them via email another way (or attach them to trac tickets) then you can use this command:

```wiki
git format-patch [-o <outputdir>] <revision range>
```


Where `<revision range>` specifies the commit that git should stop at when going from HEAD backwards, creating a patch for each commit in the range \<revision range\>..HEAD.

## Applying patches from email

```wiki
git am -3 <email>
```

# Submodules


GHC uses many git repositories which are tracked as Git submodules. For more information, check the [Submodules](working-conventions/git/submodules) page.

# Branches

## The stable branch


See [WorkingConventions/Releases](working-conventions/releases).

## Development branches


See [ActiveBranches](active-branches) for a description of **known** development branches.


The live list of currently **existing** branches in the `ghc` repository can be browsed via [ http://git.haskell.org/ghc.git/heads](http://git.haskell.org/ghc.git/heads).


New development branches names should be prefixed with `wip/` (e.g. "`wip/dependent-types`"), as otherwise the current Git server-side configuration disallows branch deletion and [ non-fast-forward updates](http://stackoverflow.com/questions/4684352/whats-a-fast-forward-in-git).
