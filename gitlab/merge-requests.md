# Merge request conventions

This page describes GHC's conventions for handling merge requests and code review.

## Field meanings

 * **Milestone:** The first release which should contain the merge request (or a backported version of it)
 * **Labels:** This encodes a number of things including:
    * The state of the merge request (e.g. ~"backport needed")
    * The topical areas the merge request affects (e.g. ~simplifier)
    * Whether the merge request affects the user-facing libraries shipped with GHC (the ~"user facing" label)

## Merge request checklist

See the [merge request description template](https://gitlab.haskell.org/ghc/ghc/blob/master/.gitlab/merge_request_templates/merge-request.md) for checklist of requirements for a merge request.

## Review checklist

When reviewing a merge request here are a few things to check for:

 * Are the commits logically structure? Are their commit messages descriptive?
 * Are ticket numbers referenced as appropriate?
 * Is a GHC release notes entry included (e.g. `docs/users_guide/*-notes.rst`)?
 * Have changelog entries been added to any changed packages (e.g. `libraries/*/changelog.md`)?
 * Has a test case been added?
 * Milestone and ~"backport needed" label set as appropriate
 * Does the patch add a significant new user-facing feature to GHC? If so perhaps a [GHC proposal](https://github.com/ghc-proposals/ghc-proposals) is in order.
 * Does the patch change GHC's core libraries (e.g. `base`, `template-haskell`, `ghc-prim`)? If so:
    * Has the [core libraries committee](https://wiki.haskell.org/Core_Libraries_Committee) consented
    * Has the ~"user-facing" label been applied?
    * Has the [head.hackage job](https://gitlab.haskell.org/ghc/head.hackage/) been run to characterise the effect of the change on user code?
    * Changelog and release notes entries are mandatory
    * Have package versions been bumped as appropriate?
    * Has an entry been added to the next release's migration guide?
 

## Backports

After a patch has made it to `master` it might be appropriate to backport it to the stable branch (e.g. `ghc-8.8`). If backporting is desired first ensure that the issue is milestoned for the next minor release and mark the merge request containing the fix with the ~"backport needed" label.

While the release manager can perform the backport on your behalf, it is appreciated if you open a merge request with the backported patches yourself. There are two ways to backport a merge request:

 * Via the web interface using the "Cherry-pick" button on the merged MR. While convenient, this is only possible if there are no merge conflicts with the stable branch. Be sure to **select the correct target branch**.
 * Via the command-line using the `git cherry-pick` command. In this case select the appropriate "backport" template (e.g. `backport-for-ghc-8.8`) when creating your merge request.

After the merge request is created ensure it has the ~backport label applied and that its milestone is set appropriately.

Once the backport MR lands the ~"backport needed" label can be removed from the source MR.