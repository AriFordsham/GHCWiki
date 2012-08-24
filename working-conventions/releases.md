# Release management and branches


Releases are made by the release manager, currently either Ian Lynagh or Paolo Capriotti.  The release manager is also the maintainer of the stable branch, see [\#Branches](working-conventions/releases#branches).

## Release Schedule

- Major releases are made once per year, typically around October-November.  We don't work to a fixed deadline, the release will be made when it is ready.

- Minor releases are made throughout the year, with no fixed schedule.  Generally there will be 2-3 minor releases for every major release.

- For each release we make a release candidate first, possibly followed by further release candidates as necessary.  Release candidates are announced on the [ glasgow-haskell-users mailing list](http://www.haskell.org/mailman/admindb/glasgow-haskell-users).

## Release policies

- Tier 1 platforms must all be in a working state before the release is made.  We make every effort to fix bugs in other platforms too, but bugs on Tier 2/3 platforms are not treated as release-blockers.

## Release Checklist

## Branches


The `master` branch of all repositories is the main development branch.  We often call this "HEAD" for historical reasons.


There is a **stable branch** corresponding to each major release, named after the major version.  For example, the stable branch corresponding to the 7.4 series of releases is called `ghc-7.4`.  Every repository (see [Repositories](working-conventions/repositories)) has a `ghc-7.4` branch, so you can switch a complete tree to the branch with `./sync-all checkout ghc-7.4`.

**Releases are only made from the stable branch.**  The stable branch for a major release is created by the release manager shortly before the release candidate (a couple of weeks or so).  When the release is made, the stable branch (of all repositories) is tagged with the release name, e.g. `ghc-7.4.1`.

**Only the release manager modifies the stable branch.**  Other developers request that changes are merged to the branch in one of the following ways:

- By moving a ticket into the "merge" state once it is fixed.  Please do this if you fix a bug and the fix is suitable for the branch.  Check that the milestone field of the ticket identifies the correct release target. This helps later on when we need to know whether a bug was fixed in a particular release, and also helps us to collect a list of all bugs that were fixed in a given release.

- By emailing the release manager to ask that a patch be merged.  This is appropriate when there isn't a ticket corresponding to the patch.


The release manager will usually merge patches with `git cherry-pick`, so patches on the branch will have different SHA-1 hashes from their corresponding patches on `master`.  This means that you can't use tricks like `git name-rev` to see whether a particular patch is on the branch - just search through the output of `git log ghc-7.4` instead.
