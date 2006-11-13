# Planning notes for GHC


This page is an internal planning document, for Ian L, Simon M, and Simon PJ. It tracks the various things we'd like to get done, apart from the usual bug triage and release cycle.

## Current active mini-projects


Ian's projects

- **[BuildBot](build-bot)**: Make stable backport and give to Paul to install when he is ready

- **Building libraries using Cabal**: Tidy up patches, do haddocking, test, then push.
  Don't worry about unreg way: we'll probably drop it anyway, and for nightly testing we
  can just do a full unreg build. Can't build the GHC package with --make due to
  a bug where GHC gets confused when as it learns more about a type as it compiles
  recursive modules: [930](https://gitlab.haskell.org//ghc/ghc/issues/930).

- **Dynamic linking and shared libraries**

  - GHCi seems to be working unregisterised
  - nofib -fPIC vs normal code on the common arches
  - Try it with native code gen 
  - Try it with registerised
  - DLL/SO for RTS+Base libs.  Then lots of DLLs/SOs can share one RTS.
  - nofib DLL vs static on the common arches
  - Instructions for how to build DLL and SOs
  - Freeing resources on unload.  Why is it hard to guarantee that Haskell finalisers run?
  - Make sure that DLL/SO works ok when all you are doing is exporting a Haskell function or functions.  Issues: initialising the Haskell RTS
  - Linking to C++ (may be a separate issue from \*dynamic\* linking)
  - Write Wiki page describing linking

    - GHCi’s linker (.o files) vs system linker (.so and .dll only)
    - GHCi’s linker only works on 5-ish platforms.  
    - Do we need indirections between modules in one package, or only for cross-package links?  And is this decided when compiling the module, or when linking the module?

- **trac** Add a "not_ghc" milestone or somesuch, for extralibs, proposals, etc.

- **List filtering**
  Put procmail in front of mailman so we can allow big darcs patches through.
  In the longer term we can look at running some sort of spam discarder or marker with procmail too.

- **Broken tests**
  Change `fail` to `broken(123)` and try to get the HEAD to a state where all
  test failures are broken and have a bug annotated. Update building guide
  to know about this change.


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

- **Long-term benchmarking**  Simon & I were talking over coffee this morning about having a lightweight way to detect when either (a) the compiler's performance changes, or (b) the performance of compiled code changes.  This was triggerred by my noticing that the HEAD nightly builds seem somewhat slower than usual right now.

> >
> > So the idea is pretty simple.  We decide on a fixed set of nofib programs --intiially all of them, but we want the set to remain stable in the future -- and as part of the nightly build we time how long it takes to compile this set with a fixed set of flags (probably -O -fasm, to rule out changes in gcc).  Then we time how long it takes to run all those programs, maybe 5 times each.  Then we keep track of these two figures for every successful nightly build, and plot the results, giving us a nice way to track GHC's performance over time.  We can go back and get some historical figures too.  Of course the results are specific to a particular machine, so we'd have to keep track of results per nightly build machine or something.

> >
> > So basically: some small changes to nofib to allow "just build everything" (I think even NoFibRuns=0 might do this), and to build a fixed set of programs (the set could be built into the nofib build system, no need to make it dynamic).  And some way to collect the results and plot graphs.  What do you think?

- **libffi**

- **Integrating coverage testing.**  What's involved here?

- **Windows installers**. Want to help Neil get going
