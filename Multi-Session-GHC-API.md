This page is to track work by @DanielG and @mpickering on making GHC work better for the interactive tooling use-case. This is part of @DanielG's GSoC 2019 project: [A stronger foundation for interactive Haskell tooling](https://summerofcode.withgoogle.com/projects/#6687588310581248), see *Section 2* in [the proposal (PDF)](http://dxld.at/gsoc19.pdf) for the original motivation.

The primary goal is

> It should be possible to use multiple GHC API [`Session`s](https://hackage.haskell.org/package/ghc-8.6.5/docs/GhcMonad.html#t:Session) in a single process concurrently. 

This functionality is important for tooling such as `haskell-ide-engine` which needs to deal with giving feedback to user's on multiple different projects simultaneously. Orchestrating this from a single process is 
simpler and more efficient. 

# Current Situation

# Desired API

# Work to do

# Related MRs

* https://gitlab.haskell.org/ghc/ghc/merge_requests/388 (Allow multiple linker instances)
* https://gitlab.haskell.org/ghc/ghc/merge_requests/935 (Start multi-package support)