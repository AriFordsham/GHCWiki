# Guidelines for using git with GHC


GHC uses [ git](http://git-scm.com/) for revision control.  This page describes various GHC-specific conventions for using git, together with some suggestions and tips for using git effectively.

## General Guidelines

- Try to make small patches (i.e. work in consistent increments).

- Separate changes that affect functionality from those that just affect
  code layout, indendation, whitespace, filenames etc.  This means that
  when looking at patches later, we don't have to wade through loads of
  non-functional changes to get to the important parts of the patch.   

- If possible, commit often.  This helps to avoid conflicts.

- Only push when your tree passes validation: see [TestingPatches](testing-patches).

- Discuss anything you think might be controversial before pushing it.

## Patch naming


We have a simple naming convention for certain kinds of patches:

- If your patch fixes breakage in the build, then begin the patch name with `"FIX BUILD"`. e.g.

  ```wiki
    FIX BUILD Use the right find on Windows systems; fixes bindist creation
  ```

- If your patch fixes a bug, then include the ticket number in the form `#NNNN` in the patch name, e.g.

  ```wiki
    withMVar family have a bug (fixes #767)
  ```

  Trac will then create a link from the commit to the ticket, making navigation easier.

## Normal workflow


Typical workflow for developing GHC is to have two trees, one called `ghc-working` and one called `ghc-validate`.  The idea is that you develop in the `ghc-working` tree, and when you're ready to push, [test the changes](testing-patches) in the `ghc-validate` tree before pushing up to the main GHC repo (or submitting patches, or sending a pull request).


Set up your repos like this:

```wiki
$ git clone http://darcs.haskell.org/ghc.git ghc-working
$ cd ghc-working
$ ./sync-all --testsuite get
$ cd ..
$ git clone ghc-working ghc-validate
$ cd ghc-validate
$ ./sync-all -r http://darcs.haskell.org/ remote set-url origin
$ ./sync-all -r `pwd`/../ghc-working remote add working
$ ./sync-all -r <account>@darcs.haskell.org:/home/darcs remote set-url --push origin
```


(where `<account>` is your account on `darcs.haskell.org`; omit this step if you don't have one, you can still submit patches via the mailing list or send a pull request to get your changes in GHC).


Now you have `ghc-working` and `ghc-validate` repos, and additionally the `ghc-validate` repo tree is set up with a remote `working` pointing to the `ghc-working` tree, and pushing from `ghc-validate` will push changes via SSH to `darcs.haskell.org`.


To pull from working into the `ghc-valdate` tree:

```wiki
cd ghc-validate
./sync-all fetch working
```


Then merge changes from `working` into the `master` branch of `ghc-validate` as appropriate.  Then:

```wiki
./sync-all pull
```


To get changes from upstream, merging if necessary.  At this point you can check what changes are new in your tree relative to upstreadm:

```wiki
./sync-all new
```


Then run `sh validate`, and if all is well:

```wiki
./sync-all push
```


to push.

## The stable branch

## Committing changes


where `~/ghc-HEAD` is my vanilla HEAD, with all the sub-repositories checked out using `git-all`.  This command tells me all the patches in the local repository tree relative to `~/ghc-HEAD`. 

**Tip**: add `pull --no-set-default` and `push --no-set-default` to your `~/.git/defaults` file, to avoid having to give `--no-set-default` in commands like the above.


To actually push to the HEAD, you can do this:

```wiki
  $ ./git-all -r simonmar@git.haskell.org:/home/git push --no-set-default
```


it'll use SSH for the push, but continue to use HTTP for pulling, which is what you want (HTTP is much faster than SSH for git operations, but for pushing we can only use SSH).
