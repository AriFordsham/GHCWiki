# Planning notes for GHC


This page is an internal planning document, for Ian L, Simon M, and Simon PJ. It tracks the various things we'd like to get done, apart from the usual bug triage and release cycle.

## Current active mini-projects


Currently active

- **BuildBot**: nearly ready.  Need to install on darcs.haskell.org.

- **Building libraries using Cabal**

- **Dynamic linking and shared libraries**

  - Seems to be working unregisterised
  - Try it with native code gen 
  - Try it with registerised
  - DLL/SO for RTS+Base libs.  Then lots of DLLs/SOs can share one RTS.
  - Instructions for how to build DLL and SOs
  - Freeing resources on unload.  Why is it hard to guarantee that Haskell finalisers run?
  - Make sure that DLL/SO works ok when all you are doing is exporting a Haskell function or functions.  Issues: initialising the Haskell RTS
  - Linking to C++ (may be a separate issue from \*dynamic\* linking)
  - Write Wiki page describing linking

    - GHCi’s linker (.o files) vs system linker (.so and .dll only)
    - GHCi’s linker only works on 5-ish platforms.  
    - Do we need indirections between modules in one package, or only for cross-package links?  And is this decided when compiling the module, or when linking the module?

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
