# Merge request conventions

This page describes GHC's conventions for handling merge requests and code review.

## Merge request checklist

See the [merge request description template](https://gitlab.haskell.org/ghc/ghc/blob/master/.gitlab/merge_request_templates/merge-request.md) for checklist of requirements for a merge request.

## Review checklist

When reviewing a merge request here are a few things to check for:

 * Are the commits logically structure? Are their commit messages descriptive?
 * Are ticket numbers referenced as appropriate?
 * Changelog entry included?
 * Release notes entry included?
 * Test case present?
 * Milestone and ~"backport needed" label set as appropriate


## Backports

After a patch has made it to `master` it might be appropriate to backport it to the stable branch (e.g. `ghc-8.8`). If backporting is desired first ensure that the issue is milestoned for the next minor release and mark the merge request containing the fix with the ~"backport needed" label.

While the release manager can perform the backport on your behalf, it is appreciated if you open a merge request with the backported patches yourself. There are two ways to backport a merge request:

 * Via the web interface using the "Cherry-pick" button on the merged MR. While convenient, this is only possible if there are no merge conflicts with the stable branch. Be sure to **select the correct target branch**.
 * Via the command-line using the `git cherry-pick` command. In this case select the appropriate "backport" template (e.g. `backport-for-ghc-8.8`) when creating your merge request.

After the merge request is created ensure that the ~backport label is applied.

Once the backport MR lands the ~"backport needed" label can be removed from the source MR.