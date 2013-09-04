# Release plans for GHC 7.8

## Timeline


The planned timeline for 7.8 is to have a feature freeze approximately at the time of ICFP, followed by a period of frantic bugfixing. A release candidate (and the 7.8 branch) will be formed after this, likely sometime in October.

## Tickets


We would like to fix all of the [ high and highest priority tickets in the 7.8.1 milestone](http://ghc.haskell.org/trac/ghc/query?priority=highest&priority=high&status=infoneeded&status=merge&status=new&status=patch&milestone=7.8.1&col=id&col=summary&col=status&col=type&col=priority&col=milestone&col=component&order=priority), but there are currently a lot of them so this seems optimistic. Please feel free to take a ticket and help us!

## Completed new features


The features already completed are documented in the release notes:
[docs/users_guide/7.8.1-notes.xml](/trac/ghc/browser/ghc/docs/users_guide/7.8.1-notes.xml)

## Pending new features


The following new features are planned for 7.8. They are at varying degrees of completeness, and may not all make it in.

- Patrick Palka has been working on making `--make` mode parallel with a `-j` flag. He has a stable set of improvements he'd like to see in 7.8: see the latest updates in [\#910](https://gitlab.haskell.org//ghc/ghc/issues/910).

- Nicolas Frisby is adding a few Core-to-Core optimisations. They usually slightly improve performance, but can sometimes make a big difference, both good and bad. They are off by default.

  - (will merge by 6 Sept)`-fdicts-strict` will make all dictionary arguments strict if they are certainly not part of a knot.
  - (merged, cf [af12cf66d1a416a135cb98b86717aba2cd247e1a](/trac/ghc/changeset/af12cf66d1a416a135cb98b86717aba2cd247e1a/ghc) and section in [LateDmd](late-dmd)) `-ffun-to-thunk` flag to revert a new (default) behavior that prevents GHC from creating sharing
  - (merged, cf [LateDmd](late-dmd)) `-flate-dmd-anal` will run demand analysis near the end of the pipeline. cf [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782).

    - TODO communally determine if -O2 should imply it

- Pedro and Richard were working on tidying up the poly-kinded Typable, and propositional equality (`gcast` and friends) story.  See [\#8132](https://gitlab.haskell.org//ghc/ghc/issues/8132).

- Geoff Mainland is working on a better Template Haskell implementation ([ http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges](http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges), [ http://gmainland.blogspot.co.uk/2013/05/type-safe-runtime-code-generation-with.html](http://gmainland.blogspot.co.uk/2013/05/type-safe-runtime-code-generation-with.html)), in the `th-new` branch of various repos. ETA: September 13.

- Geoff Mainland is working generalising the support for SSE-like instructions. ETA: September 13.

- Iavor Diatchki is working on type-level nats ([\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385), [ http://ghc.haskell.org/trac/ghc/wiki/TypeNats](http://ghc.haskell.org/trac/ghc/wiki/TypeNats)).  The plan is to support simple type-level arithmetic in GHC 7.8. The branch `type-nats-simple` has the code.

- Austin Seipp would like to do some official ARMv7 binary releases with a working stage2 compiler and GHCi, but there are probably some bugs waiting here.

- Edsko de Vries would like to have "Source plugins" in GHC, which would allow API clients and external users to run code over the type-checked AST. Thomas Schilling and others are also interested. Edsko has proposed a patch, but it has yet to be integrated. See [ http://www.haskell.org/pipermail/ghc-devs/2013-June/001358.html](http://www.haskell.org/pipermail/ghc-devs/2013-June/001358.html) and [ http://www.haskell.org/pipermail/ghc-devs/2013-July/001624.html](http://www.haskell.org/pipermail/ghc-devs/2013-July/001624.html)

- Dynamic GHCi ([\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)). This is working in HEAD, and enabled if `DYNAMIC_GHC_PROGRAMS=YES`. Currently it's enabled by default if dynamic libraries are supported, except for FreeBSD and Windows.
  On FreeBSD the reason it's disabled is due to a bug in FreeBSD's rtld. This has been fixed, but we're waiting for the fix to make it into releases. This might be in time for 7.8, but certainly will be for 7.10. See [\#7819](https://gitlab.haskell.org//ghc/ghc/issues/7819).
  On Windows, there are a couple of build time annoyances: `-dynamic-too` doesn't work on Windows, and linking takes a very long time when dynamic linking is used. There's no technical reason why it couldn't be enabled, though.
  The plan is/was to use dynamic GHCi on as many platforms as possible in 7.8, and to remove support for non-dynamic-ghci in HEAD soon after. See discussion in [\#8039](https://gitlab.haskell.org//ghc/ghc/issues/8039), however.

- Ryan Newton has added a larger set of atomic memory primops than were previously available.  The code was developed on the `atomics` branch, which is now merged.  The set of PrimOps may continue to expand slightly, but for now includes `casArray#`, `casIntArray#`, and `fetchAddIntArray#`.  In future work, Carter Schonwald will provide optimized (inline) versions of these PrimOps, at least for the LLVM backend.

- The Applicative-Monad warning would preferably be integrated. See [\#8004](https://gitlab.haskell.org//ghc/ghc/issues/8004). David LDavid Luposchainsky is driving this.

## Features that will definitely not make it

- [Newtype wrappers](newtype-wrappers), by Joachim Breitner.  This will not be ready for 7.8.

- [Overloaded record fields](records/overloaded-record-fields/plan), by Adam Gundry (GSOC).  Not sure if this will be done.

- [PatternSynonyms](pattern-synonyms), by Gergo Erdi. Progress tracked at [\#5144](https://gitlab.haskell.org//ghc/ghc/issues/5144).
