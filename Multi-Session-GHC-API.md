This page is to track work by @DanielG and @mpickering on making GHC work better for the interactive tooling use-case. This is part of @DanielG's GSoC 2019 project: [A stronger foundation for interactive Haskell tooling](https://summerofcode.withgoogle.com/projects/#6687588310581248), see *Section 2* in [the proposal (PDF)](http://dxld.at/gsoc19.pdf) for the original motivation.

The primary goal is

> It should be possible to use multiple GHC API [`Session`s](https://hackage.haskell.org/package/ghc-8.6.5/docs/GhcMonad.html#t:Session) in a single process concurrently. 

This functionality is important for tooling such as `haskell-ide-engine` which needs to deal with giving feedback to user's on multiple different projects simultaneously. Orchestrating this from a single process is 
simpler and more efficient. 

# Current Situation

Lot's of arbitrary restrictions within GHC:
 - One GHC "session" per process
    - Global linker state with GHCi/TH
 - One package being built per "session"

# Desired API

No arbitrary restrictions:
 - Multiple sessions per process
 - Multiple packages per session
 - No global linker state

Eventually, enough state should be removed that the notion of a "session" at all is purely optional: just for caching and code execution (ghci/TH). This, strictly speaking is out of scope of this feature, but keeping such a goal like in mind may be help in guiding efforts.

# Work to do

- GSOC proposal proposed focusing on multiple sessions per process
  - Remove singleton state where appropriate
  - Cross fingers on linker MR (!388) but use `-fexternal-interpreter` as a workaround (per-process linker state cordoned off in iserv processes).
- !935 modifies `HscEnv` to solve the packages per session
  - Better UX when developing multiple *related* packages, and changes in dependencies should be propagated downstream
  - Also needed for multi-package code execution (vs meerly using GHCi for fast type checking)
- !388 solves the linker problem, or at least the haskell (vs C) part of it?

# Related MRs

* https://gitlab.haskell.org/ghc/ghc/merge_requests/388 (Allow multiple linker instances)
* https://gitlab.haskell.org/ghc/ghc/merge_requests/935 (Start multi-package support)