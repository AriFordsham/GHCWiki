# Guidelines for using git with GHC


GHC uses [ git](http://git-scm.com/) for revision control. This page describes various GHC-specific conventions for using git, together with some suggestions and tips for using git effectively.  


Existing darcs users see: [Git For Darcs Users](git-for-darcs-users). If you have an existing source tree in darcs and need to convert patches, see [Darcs To Git](darcs-to-git). Simon PJ's git notes are [GIT SPJ](working-conventions/git-spj).

## General Guidelines

- Try to make small patches (i.e. work in consistent increments).

- Separate changes that affect functionality from those that just affect
  code layout, indentation, whitespace, filenames etc.  This means that
  when looking at patches later, we don't have to wade through loads of
  non-functional changes to get to the important parts of the patch.   

- If possible, commit often.  This helps to avoid conflicts.

- Only push when your tree passes validation: see [TestingPatches](testing-patches).

- Discuss anything you think might be controversial before pushing it.

- When making changes to other repositories in a GHC tree, see [WorkingConventions/Repositories](working-conventions/repositories).

## Author


Please make sure you have setup git to use the correct name and email for your commits. Use the same name and email on all machines you may push from.

```wiki
$ git config --global user.name "Firstname Lastname" # Sets the name of the user for all git instances on the system
$ git config --global user.email "your_email@youremail.com"
```


This will set your name and email globally. To set it for just the GHC repo, remove the `--global` flag. Also, the environment variables `GIT_COMMITTER_NAME`, `GIT_COMMITTER_EMAIL`, `GIT_AUTHOR_NAME` and `GIT_AUTHOR_EMAIL` will override git-config settings if they are defined.

## Commit messages


We have a simple convention for commit log messages:

- If your patch fixes breakage in the build, then begin the patch name with `"FIX BUILD"`. e.g.

  ```wiki
    FIX BUILD Use the right find on Windows systems; fixes bindist creation
  ```
- If your patch fixes a bug, then include the ticket number in the form `#NNNN` in the patch name, e.g.

  ```wiki
    withMVar family have a bug (fixes #767)
  ```

  **Trac will then create a link from the commit to the ticket**, making navigation easier.

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

## Workflow with validate


All changes to GHC and the libraries need to be [validated](testing-patches) before they can be pushed to the main repositories.  Validation can take a while - 30 minutes on a 4-core machine is typical - so ideally you want to be validating changes while you are working in a separate tree.  In fact, there are other compelling reasons to have two trees in your development workflow, one for working in and one for validation:

- Validation uses build settings that are different to the ones you would normally use while developing: it adds more libraries (DPH), builds extra ways (dynamic libraries), and builds all the documentation, so you don't want to use the same build for validation and ordinary development.  In the development tree we use build settings optimised for development: `-O0 -DDEBUG` for the compiler, minimal libraries and ways so that rebuilding is fast.

- Having two trees eliminates a common source of breakage in the main repository: with one tree it is easy to add new files but forget to commit them.  Your tests will work, but the build will be broken for others.  If you have to pull your changes into a separate tree for testing, you'll notice the missing files before you push.


The typical workflow is to work in the development tree, pull into the validate tree, validate, and then push from the validate tree.  But what if validate fails?  There are two options:

1. discard the patch in the validate tree (using some instance of `git reset`) and go back to the working tree to fix it
1. or, add a new patch in the validate tree to fix the problem and re-validate


(1) is more for "back to the drawing board" kinds of failure, whereas (2) is for cases where you just need to fix a warning or some other minor error exposed by validate.

### Setting up the trees


Let's call the two trees `ghc-working` and `ghc-validate`.


Set up your repos like this:

```wiki
$ git clone http://darcs.haskell.org/ghc.git ghc-working
$ cd ghc-working
$ ./sync-all --testsuite --no-dph get
$ cd ..
$ git clone ghc-working ghc-validate
$ cd ghc-validate
$ ./sync-all --testsuite get
$ ./sync-all -r http://darcs.haskell.org/ remote set-url origin
  # Get the dph libraries too
$ ./sync-all --testsuite get
$ ./sync-all -r `pwd`/../ghc-working remote add working
$ ./sync-all -r <account>@darcs.haskell.org:/home/darcs remote set-url --push origin
```


(where `<account>` is your account on `darcs.haskell.org`; omit this step if you don't have one, you can still submit patches via the mailing list (using `git format-patch` will help you with this) or send a pull request to get your changes in GHC).


Now you have `ghc-working` and `ghc-validate` repos, and additionally the `ghc-validate` repo tree is set up with a remote `working` pointing to the `ghc-working` tree, and pushing from `ghc-validate` will push changes via SSH to `darcs.haskell.org`.

### The rebase workflow


How do we move patches from `ghc-working` and `ghc-validate`?  There are several options here.  One is to just use `sync-all pull working` and do merging as usual.  This works fine, but results in extra "merge commits" that aren't particularly helpful and clutter the commit logs and the mailing list.  A better approach is to rebase patches before committing.  This is done as follows:

1. Pull from `ghc-working` into `ghc-validate`: `./sync-all pull working master`
1. Rebase onto origin/master: `./sync-all pull --rebase`.  You may encounter conflicts, in which case git will tell you what to do (usually fix the conflict and then `git rebase --continue` in the appropriate repository), then you can resume with `./sync-all --resume pull --rebase` at the top.
1. Check what you have relative to origin: `./sync-all new`
1. `./validate`
1. if validate went through, `./sync-all push` (you might like to check once more what will be pushed: `./sync-all new`).


If push fails because patches have been pushed by someone else while you were validating, it is acceptable to `git pull --rebase` in that repository and push if there are no conflicts (no need to validate again).


Now, the patches pushed this way are different (have different hashes) from the patches that you originally committed in `ghc-working`, and if you try to pull these patches in `ghc-working` again, confusion and conflicts will ensue.  Fortunately there's an easy solution: just rebase again in `ghc-working`, and git will notice that your patches are already upstream and will discard the old versions.  It's as simple as

```wiki
 $ cd ghc-working
 $ ./sync-all pull --rebase
```


If rebase encounters a conflict at any point, it will tell you what to do.  After fixing the conflict and completing the rebase manually, you can then resume the pull with `./sync-all --resume pull --rebase`.


There is a slight tweak to this workflow that you might find more convenient: do a `./sync-all pull --rebase` in the `ghc-working` tree prior to pulling into `ghc-validate`.  This lets you fix conflicts in `ghc-working` rather than in `ghc-validate`, and test the resolution before validating.  The downside is that you might now have to do a lot of rebuilding in your `ghc-working` tree if there are a lot of changes to pull.

## Contributing patches


Please write your patch and then rebase to the latest version of GHC HEAD before sending to us. You can use the following command to send patches via email:

```wiki
git send-email --to=cvs-ghc@haskell.org <hash-id> -1
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

## The stable branch


See [WorkingConventions/Releases](working-conventions/releases).
