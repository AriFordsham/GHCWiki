# Guidelines for using darcs with GHC


GHC uses [ darcs](http://darcs.net/) for revision control.  This page describes various GHC-specific conventions for using darcs, together with some suggestions and tips for using darcs effectively.

## General Guidelines

- Try to make small patches (i.e. work in consistent increments).

- Separate changes that affect functionality from those that just affect
  code layout, indendation, whitespace, filenames etc.  This means that
  when looking at patches later, we don't have to wade through loads of
  non-functional changes to get to the important parts of the patch.   

- If possible, record often.  This helps to avoid conflicts.

- Do not push conflicts (see [WorkingConventions/Darcs](working-conventions/darcs#conflicts)).

- Every patch must pass at least minimal validation: see [TestingPatches](testing-patches).

- Discuss anything you think might be controversial before pushing it.

## Patch naming


We have a simple naming convention for certain kinds of patches:

- If your patch fixes breakage in the build, then begin the patch name with `"FIX BUILD"`. e.g.

  ```wiki
    FIX BUILD Use the right find on Windows systems; fixes bindist creation
  ```

  The point here is that if someone pulls GHC from darcs and experiences a build failure, they can try
  `darcs pull -a -p "FIX BUILD"` in order to grab patches that fix it, without grabbing anything else
  that might introduce further breakage.

- If your patch fixes a bug, then begin the patch name with `"FIX #NNNN"`, where `NNNN` is the ticket
  number. e.g.

  ```wiki
    FIX #767 (withMVar family have a bug)
  ```

## The stable branch


The preferred way to make a change to the stable branch is to first record it on the HEAD,
with a note in the commit message that it should be merged. You can then also push it to the
stable branch if you wish, or you can leave it for the stable branch maintainer to do.


Sometimes it is not possible to push the HEAD patch to the stable branch, as it may depend on
other patches that are meant only for the HEAD. In this case the changes will need to be remade
in the stable branch. The commit message for this new change should include the entire commit
message of the original change, prefaced with `"MERGED: "`.
The same patch should be pushed to both branches whenever possible, though, as it makes it
easier to compare the two branches.


If you first fix a problem in the stable branch then please do apply it only to that branch
rather than leaving it lying around for when you have time to make the HEAD patch.

## Conflicts


Right now, we avoid having any conflicts in the GHC HEAD repository, because darcs currently doesn't handle conflicts well.  Don't push and conflicts, even resolved ones, to the GHC HEAD.  Instead amend-record or re-record your patches to fix the conflicts before pushing.


Conflicts on branches are less of a problem, because branches usually have a limited life-span.  As long as the branch is not intended to be merged with the HEAD in the future (e.g. the stable branches), then small conflicts are ok.


We're aware that this policy creates problems for development branches of GHC, and this is truly unfortunate.  We're hopeful that darcs' conflict handling will improve in the future and we can get the full power of darcs for separate development.

## Committing changes


If you have permission to push patches directly to the repository (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push`:

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!

## Pushing a whole tree


A GHC tree consists of several repositories (see [Building/GettingTheSources](building/getting-the-sources)).  Sometimes you want to push from them all at the same time, for example after running a validate (see [TestingPatches](testing-patches)).


It's good practice to have a completely clean set of repositories locally, i.e. a locally cached copy of the main repositories on `darcs.haskell.org`.  This is useful for creating new trees quickly, or comparing your trees to the HEAD.  To see which patches you have in a GHC tree relative to a clean repository, you can use the [darcs-all script](building/darcs-all) in the root of the GHC repository:

```wiki
  $ ./darcs-all -r ~/ghc-HEAD push --dry-run --no-set-default
```


where `~/ghc-HEAD` is my vanilla HEAD, with all the sub-repositories checked out using `darcs-all`.  This command tells me all the patches in the local repository tree relative to `~/ghc-HEAD`. 

**Tip**: add `pull --no-set-default` and `push --no-set-default` to your `~/.darcs/defaults` file, to avoid having to give `--no-set-default` in commands like the above.


To actually push to the HEAD, you can do this:

```wiki
  $ ./darcs-all -r simonmar@darcs.haskell.org:/home/darcs push --no-set-default
```


it'll use SSH for the push, but continue to use HTTP for pulling, which is what you want (HTTP is much faster than SSH for darcs operations, but for pushing we can only use SSH).
