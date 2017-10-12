# Continuous Integration


This page is to support the discussion of GHC DevOps Group on the CI solution for GHC to provide continuous testing and release artefact generation. See also [\#13716](https://gitlab.haskell.org//ghc/ghc/issues/13716).

## Requirements

**Primary**

- Build GHC for Tier 1 platforms (Windows, Linux & macOS), run `./validate`, and produce release artefacts (distributions and documentation).
- Build PRs (differentials) and run `./validate` on Linux/x86-64.
- Security: PRs builds run arbitrary user code; this must not be able to compromise other builds and especially not release artefacts.
- Infrastructure reproducibility (infrastructure can be spun up and configured automatically)
- Infrastructure forkability (devs forking the GHC repo can run their own CI without additional work)
- Low maintenance overhead
- Low set up costs

**Secondary**

- Build PRs (differentials) and run `./validate` on non-Linux/x86-64 Tier 1 platforms.
- Build GHC for non-Tier 1 platforms & run `./validate`

## Possible solutions

### Jenkins


Pros

- We can run build nodes on any architecture and OS we choose to set up.


Cons

- Security is low on PR builds unless we spend further effort to sandbox builds properly.
- Jenkins is well known to be time consuming to set up.
- Additional time spent setting up servers.
- Additional time spent maintaining servers.
- It is unclear how easy it is to make the set up reproducible.
- The set up is not forkable (a forker would need to set up their own servers).

### CircleCI & Appveyor


Pros

- Easy to set up
- Excellent security
- Low maintenance cost
- Infrastructure reproducible 
- Infrastructure forkable
- Easy to integrate with GitHub


Cons

- Direct support only for Linux, Windows, macOS, iOS & Android, everything else needs to rely on cross-compiling and QEMU, or building on remote drones.

## Todo list