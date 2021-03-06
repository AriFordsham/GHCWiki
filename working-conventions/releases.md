# Release management and branches


Releases are made by the release manager, currently Ben Gamari. The release manager is also the maintainer of the stable branch, see [\#Branches](working-conventions/releases#branches).

## Release policies


GHC's release policies are designed to make life easier for our users, including tool builders and Haskell package ecosystems. We try hard to avoid breaking downstream tool-chains without strong reason. 


To that end, we follow these release policies:

- **Minor releases** generally will not introduce new interfaces or compiler features

- **Platforms**.  Tier 1 [platforms](platforms) must all be in a working state before the release is made.  We make every effort to fix bugs in other platforms too, but bugs on Tier 2/3 platforms are not treated as release-blockers.

- **Source distributions**.  As a policy we provide source distributions to our external binary distribution contributors a week before the binary release is announced to put these contributed distributions on equal footing as the GHC-HQ-provided distributions

- **Two-release policy**.  Every release of GHC must be bootstrappable with the most recent minor release of the two most-recent major releases of GHC.

## Release Schedule

- Major releases are made twice per year, typically around January-February and June-July. We try to stick to this schedule

- Minor releases are made throughout the year, with no fixed schedule.  Generally there will be 2-3 minor releases for every major release.

- For each release we make a release candidate first, possibly followed by further release candidates as necessary.  Release candidates are announced on the [glasgow-haskell-users mailing list](http://www.haskell.org/mailman/listinfo/glasgow-haskell-users).

### Major releases


Major releases have a version number `x.y.1`, where `y` is even.


They may include new compiler features (and remove old features), and new major versions of libraries.

### Minor releases


Minor releases have a version number `x.y.z`, where `y` is even and `z` is `2` or higher.


They generally do not add or remove any features, or include new major versions of libraries. They only fix bugs and performance issues in the previous release on that branch.

### Snapshots


We may, from time to time, recommend a particular snapshot of HEAD, for example for people interested in testing a new feature or who want to check that there have been no performance regressions. Snapshots will have a version number `x.y.z`, where `y` is odd.


The advantages over using one of these recommended snapshots, rather than any other snapshot of HEAD, are:

- They provide a common base for everyone testing a particular feature.
- We will make binary builds for all the tier-1 platforms of these snapshots.
- In general, at any given time there may be major bugs in HEAD while development progresses. However, the recommended snapshots will not have any known major bugs.


However, these are not proper releases. For example:

- Building future GHC releases with them will not be supported.
- They are not suitable for incorporation into the Haskell Platform.
- We do not expect that library maintainers will support them.
- There will be no release notes.
- There may be some failing tests, minor bugs, etc.


This last point means that if you want to use libraries from Hackage with the snapshot, then you should expect to have to make changes to them to allow them to build with the snapshot; at the very least, it is likely that some version bounds in `.cabal` files will need updating.

## Release Checklist


Before a release can be made, the release manager needs to work with the core library upstream maintainers to agree on which library versions will ship with the new GHC release. See [WorkingConventions/BootLibraries](working-conventions/boot-libraries) for details.


See [MakingReleases](making-releases) and [TodoAfterReleases](todo-after-releases).

## Branches


The `master` branch of all repositories is the main development branch.  We often call this "HEAD" for historical reasons.


There is a **stable branch** corresponding to each major release, named after the major version.  For example, the stable branch corresponding to the 7.10 series of releases is called `ghc-7.10`.  Every repository (see [Repositories](working-conventions/repositories)) has a `ghc-7.10` branch, so you can switch a complete tree to the branch with `git checkout ghc-7.10; git submodule update`.

**Releases are only made from the stable branch.**  The stable branch for a major release is created by the release manager shortly before the release candidate (a couple of weeks or so).  When the release is made, the stable branch (of all repositories) is tagged with the release name, e.g. `ghc-7.8.1`.

**Only the release manager modifies the stable branch.**  Other developers request that changes are merged to the branch in one of the following ways:

- By moving a ticket into the "merge" state once it is fixed.  Please do this if you fix a bug and the fix is suitable for the branch.  Check that the milestone field of the ticket identifies the correct release target. This helps later on when we need to know whether a bug was fixed in a particular release, and also helps us to collect a list of all bugs that were fixed in a given release.

- By emailing the release manager to ask that a patch be merged.  This is appropriate when there isn't a ticket corresponding to the patch.


The release manager will usually merge patches with `git cherry-pick`, so patches on the branch will have different SHA-1 hashes from their corresponding patches on `master`.  This means that you can't use tricks like `git name-rev` to see whether a particular patch is on the branch - just search through the output of `git log ghc-7.8` instead.


The list of active (and defunct) [development branches](active-branches) is available. Note that currently **only administrators have privileges to perform disrupting changes** (such as deleting branches) to the repository.
