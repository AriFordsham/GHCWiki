# Profiling GHC itself


If GHC itself is running too slowly, you can profile the compiler itself.  The way to do this is to add

```wiki
GhcProfiled=YES 
```


to your `build.mk` file.  This is more robust than trying things like `GhcStage2HcOpts += -prof` because there are several things to do: first we build the ghc library, then we build the ghc program, linked against the library.


You can also use the prof BuildFlavor:

```wiki
# Profile the stage2 compiler:
BuildFlavour = prof
```


Once you've done this, you should be able to run GHC (stage 2) to generate time and space profiles. For example:

```wiki
$(TOP)/inplace/bin/ghc-stage2 +RTS -p -RTS
```


Note that this builds a profiled *stage-2* compiler.  In principle it's possible to build a profiled *stage-1* compiler, but the build system isn't set up to do that right now.  Notably, various libraries (eg Cabal) are built and installed by the bootstrap compiler before building GHC; these would need to be built and installed in a profiled way too. Additionally, the built compiler will manifest any profiling bugs that were in your bootstrapping compiler.


If you want to profile GHC while compiling GHC, the easiest way to do this is to build a *stage-3* compiler with your profiled *stage-2* compiler. You’ll need to run `make stage=3` the first time you do this in order to build the dependencies for the stage3 compiler; see [Rebuilding GHC](building/using#rebuilding-the-ghc-binary-after-making-changes) and below for more details.


Setting `GhcProfiled` also enables profiled Haddock, which can be built by running `make HADDOCK_DOCS=yes`. This is useful if you're investigating a regression which is showing up from a Haddock performance test.
