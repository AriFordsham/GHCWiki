# Planning notes for GHC


This page is an internal planning document, for Ian L, Simon M, and Simon PJ. It tracks the various things we'd like to get done, apart from the usual bug triage and release cycle.

## Current active mini-projects


Ian's projects

- **[BuildBot](build-bot)**: Install on darcs.haskell.org and set up slaves

  - Set up Windows nightly build
  - Set up unregisterised nightly build?

- **Building libraries using Cabal**: Tidy up patches, do haddocking, test, then push.
  Don't worry about unreg way: we'll probably drop it anyway, and for nightly testing we
  can just do a full unreg build. Can't build the GHC package with --make due to
  a bug where GHC gets confused when as it learns more about a type as it compiles
  recursive modules: [930](https://gitlab.haskell.org//ghc/ghc/issues/930).

  - Look into whether we can do `SplitLibraries` with Cabal or not.

- Freeing resources on unload.  Why is it hard to guarantee that Haskell finalisers run?

- **Dynamic linking and shared libraries**

  - GHCi seems to be working unregisterised
  - Make -fPIC work with the NCG on various arches
  - DLL/SO for RTS+Base libs.  Then lots of DLLs/SOs can share one RTS.
  - nofib -fPIC vs normal code on the common arches
  - nofib DLL vs static on the common arches
  - Doc updates
  - Write Wiki page describing GHCi linker

    - GHCi’s linker (.o files) vs system linker (.so and .dll only)
    - GHCi’s linker only works on 5-ish platforms.  

- What does this mean: Make sure that DLL/SO works ok when all you are doing is exporting a Haskell function or functions.  Issues: initialising the Haskell RTS

- **trac** Add a "not_ghc" milestone or somesuch, for extralibs, proposals, etc.

- **Mailing lists**:

  - Put procmail in front of mailman so we can allow big darcs patches through.
  - get sudo access on haskell.org
  - install spam-filtering technology
  - reoganise mailing lists: remove cvs-all, resubscribe everyone to the other lists
    (announce beforehand).

- **Broken tests**
  Change `fail` to `broken(123)` and try to get the HEAD to a state where all
  test failures are broken and have a bug annotated. Update building guide
  to know about this change.

- **Ghc Performance Index** ([\#1009](https://gitlab.haskell.org//ghc/ghc/issues/1009)).

- **libffi**


Simon PJ's projects

- **Lambda match** Look at Claus' patch
- **Patch submission** Write some blurb on patch submission guidelines
- **Implication constraints**
- **Demand analysis** with Kirsten Chevalier
- **Associated data types and type synonyms**, with Manuel: [TypeFunctions](type-functions)
- **Data parallel Haksell**, with Manuel, Gabi, Roman; see [ http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)


Simon M's projects

- **darcs** Get darcs.h.o:\~igloo/darcs/ installed
- **Parallel garbage collection**

## Awaiting attention


This list intended to be in priority order (but of course the prorities might not be right!

- **Windows installers**. Want to help Neil get going (see also [\#604](https://gitlab.haskell.org//ghc/ghc/issues/604)).
