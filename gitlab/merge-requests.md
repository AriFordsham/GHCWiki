# Merge request conventions

## Merge request checklist

See the [merge request description template](https://gitlab.haskell.org/ghc/ghc/blob/master/.gitlab/merge_request_templates/merge-request.md) for checklist of requirements for a merge request.

## Backports

After a patch has made it to `master` it might be appropriate to backport it to the stable branch (e.g. `ghc-8.8`). If backporting is desired first ensure that the issue is milestoned for the next minor release and mark the merge request containing the fix with the ~"backport needed" label.

There are two ways to backport the patches:

 * Via the web interface using the "Cherry-pick" button on the merged MR. While convenient, this is only possible if there are no merge conflicts with the stable branch. Be sure to **select the correct target branch**.
 * Via the `git cherry-pick` command. In this case select the appropriate "backport" template (e.g. `backport-for-ghc-8.8`) when creating your merge request.

After the merge request is created ensure that the ~backport label is applied.