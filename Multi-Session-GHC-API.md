This page is to track work by @DanielG and @mpickering on making GHC work better for the interactive tooling use-case. This is part of @DanielG's GSoC 2019 project: [A stronger foundation for interactive Haskell tooling](https://summerofcode.withgoogle.com/projects/#6687588310581248), see *Section 2* in [the proposal (PDF)](http://dxld.at/gsoc19.pdf) for the original motivation.
The page also tracks the work by @fendor, @Ericson2314 and @mpickering to enable GHC and GHCi to support multiple home packages. This is part og @fendor's GSoC 2020 project: [Multiple Home Packages for GHC](https://summerofcode.withgoogle.com/projects/#5269390116782080)

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

# Multiple Packages per session (!935)

## Motivation

Primary goal:

> Enable GHC and GHCi to work with multiple units at once.

Most importantly to the end user, this allows development of multiple units in GHCi. There are other workflows which can benefit from such functionality, such as incremental compilation and Haskell Language Server.

Original GSoC 2020 proposal: [HomePackages.pdf](uploads/4d52e9f036c25a5d33928def7deddb7b/HomePackages.pdf)

## Architectural changes

The important change is to `HscEnv`, where we extract the DynFlags and Home Package Table, into a Map from `UnitId -> InternalUnitEnv`. 
The Internal UnitEnv itself is a tuple of DynFlags and Home Package Table (HPT). It contains all information necessary to compile the package identified by its Unit Id.
To maintain backwards compatibility (e.g. dont break almost every GHC API program in existence), we additionally save the currently "active" home unit in `HscEnv` in the field `hsc_currentUnit`. This way, the existing functions `hsc_dflags` and `hsc_HPT` can continue to function. Note, however, that this will still break a lot of GHC API programs, as these functions have been demoted from fields to ordinary functions. Anything that modifies the `HscEnv` directly will, thus, fail.

The Design of `hsc_currentUnit` is reasonable, but entails some statefulness that is far from optimal.
Within GHC, we will need a thourogh guideline, when to use `hsc_dflags` or `hsc_HPT`, and when to pass in the explicit unit-id, which can be used to query the Internal Unit Env for the appropriate DynFlags/HPT.

## Changes to Finder

The finder needs to know in which HPT it should look for home modules.
Therefore, a lot of changes are to add an explicit unit-id parameter.
We prefer this approach over modifying the `hsc_currentUnit` field.

## Legacy support

### Usage of `hsc_dflags` and `hsc_HPT`

These have been demoted from fields in `HscEnv` to ordinary functions.
Therefore, a lot of existing code that modified `HscEnv`'s record might break.
Within GHC, any occurrence of `hsc_dflags` and `hsc_HPT` needs to be reviewed individually, as any usage of these functions must be considered outdated in the face of multiple home packages.

### Updating the Internal Unit Environment

Initially we are creating an empty Unit Environment:
```haskell
UnitEnv:
  main -> {}
```

where the name "main" is defined by `homeUnitId` which is specified by "-this-unit-id" or defaults to "main". Additionally, it points to an empty HPT by default.

The problem is when users now modify the `homeUnitId`(part of DynFlags) and then call `setSessionDynFlags` to change the active DynFlags.
We need to change the key (hsc_currentUnit) and rename the old key to the new within the internal UnitEnv, as well, otherwise we will face breaking user code a lot.

## Compilation

Compilation with multiple home-modules works very similar to with a single package. 
Essentially, we now have a set of targets annotated with unit-ids. We can search for each of these targets and let the downsweep/upsweep do its thing. The only real change we do is to modify the ModSummary to include the DynFlags from the UnitId saved in the Internal UnitEnv. This is necessary for the compilation later, where we might have a different unit state. Therefore, before compiling a module, we have to change the currently active unit (hsc_currentUnit), to the `homeUnitId` saved in ModSummary.

The Finder logic needs to be adapted to be able to find the different modules and especially when we are looking only for "some" module, we have to look everywhere:

* In the unit-state/EPS
* In every HPT that is currently saved in the UnitEnv

Reliance on functions such as `hsc_dflags` are discouraged, to avoid stateful function calls. Use whenever possible the explicit parameter `UnitId` and lookup the requires information via `hsc_unitDflags` or pass in directly DynFlags.
(Is this the best approach?)

## Linking

It is up for debate, whether we should be able to only produce a single executable, e.g. only from unit id `main`, or from any home package that declares that it wants to produce an executable.

Linking is complicated. For linking an executable, we need the dependencies of the executable as `UnitInfos`. However, one of our dependencies might be one of the local home packages. For linking, we must build a UnitInfo out of the HPT and DynFlags that can be linked into the executable.
For a first step, it can be enough to just compile every home package and produce the required artefacts. In a separate step we may link the various executables as a first step.

## Migration Guide for Users

There will be no backwards compatability story, since this change touches too many places.

The functionality of existing GHC API programs will remain vastly unchanged.
A few replacements will be necessary though:

* Replace record updates of HscEnv with `set_hsc_dflags` and `set_hsc_HPT` respectively.
* Usage of RecordWildCards on HscEnv will no longer work for `hsc_dflags` and `hsc_HPT`. They always must be applied to HscEnv.
* Targets have an additional field now. The field can be obtained by querying `homeUnitId` from `getSessionDynFlags`.
* Modifying `DynFlags` in a session requires a call to `setSessionDynFlags` so that the Internal Unit Env will be updated appropriately, in case of changes to `homeUnitId`.

Other than that, we expect that nothing else needs to be modified to compile with only a single home package table.

# Related MRs

* https://gitlab.haskell.org/ghc/ghc/merge_requests/388 (Allow multiple linker instances)
* https://gitlab.haskell.org/ghc/ghc/merge_requests/935 (Start multi-package support)