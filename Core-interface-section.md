Several projects want to be able to see the Core for compiled Haskell modules.

There are a few common considerations:
- Platform-agnostic Core
    - Some of these projects want to see Core in an abstract way that doesn't include any of the platform-specific behaviour or optimizations that GHC does. 
    - Obviously CPP and explicit references to machine types and operations will still be there. 
- Non-interference
    - Some of these projects want to run independently of normal code generation, so any mechanism should not affect how normal code generation proceeds.

# Examples

## Plutus

Plutus uses a GHC plugin to compile Core for *some* of the program.

Currently uses unfoldings, which it gets by putting `INLINABLE` on everything.

Desiderata:
- Platform-agnostic Core
    - Compiles to a completely different "platform".
- Non-interference
    - Only parts of the program are compiled specially, the normal Haskell program is used.

## Liquid Haskell

Liquid Haskell operates on Core. How does it work now? (MPJ: find out). It would probably like to see the Core for compiled modules.

Desiderata:
- Platform-agnostic Core
    - Things like machine arithmetic probably make LH's life harder.
- Non-interference
    - Programs analyzed with LH are supposed to be compiled normally, so you don't want it to interfere with normal codegen.

## Clash

Clash compiles Haskell code into a low-level hardware description language. 

Clash works from Core. It currently uses unfoldings, using `-fexpose-all-unfoldings`. It can evidently tolerate post-optimization Core, so possibly it doesn't care about "Platform-agnostic Core". I don't know if it cares about "Non-interference".

## GRIN

GRIN is an experimental backend for GHC, which converts STG into its own IR. It
works from STG.

GRIN currently adds a GHC compiler plugin, which generates its own, new,
interface files during compilation of dependencies. They patch Cabal to ensure
that their interface files are installed. Then at the end they can load the GRIN
interface files for dependencies and assemble the final result.

GRIN couldn't directly use the new section proposed on this page, but we could potentially add an STG section in the same vein.

## Interpreting Core for dependencies

The [Staged TH](https://github.com/ghc-proposals/ghc-proposals/pull/243) proposal discusses using a Core interpreter to run TH splices in some circumstances. This would require having access to the Core of dependencies.

Desiderata:
- Platform-agnostic Core

# Proposal: add a Core section to interface files

There are already plans to make interface files extensible, so we can just add additional sections.

## What goes in?

At least: the post-desugar but pre-optimization Core for every binding in the module.

In particular, we must not do any cross-module inlining, as the inlined unfolding will have been optimized (this is particularly important since that may violate "Platform-agnostic Core"). Intra-module inlining is probably fine but it might be safest to just not do anything.

Possibly we should add more than this. If we added essentially the entire `ModGuts`, then we could support *resumable* compilation. We might also need to preserve some `DynFlags`, or require them to be passed identically again.

## How is generation controlled?

There should be a compiler flag that controls it, much like for HIE files etc.

It's tempting to say that plugins should be able to control it. However, plugins can't be the *primary* method of control, since this is an option that one needs to pass to all one's dependencies, and one cannot easily enable a plugin for all one's dependencies, since that would require adding an extra dependency to all of them! Which is quite non-trivial (think about `base`), and not supported by Cabal either.

In the future *might* want to run more of GHC's optimizer on the Core. I don't know how this should be controlled, but I note that clients can do this themselves after loading the Core if necessary.

# Resumable compilation

If we're adding the post-desugaring Core to interface, we could try and support *resuming* compilation from such interface files: i.e. just loading the `ModGuts` back into the pipeline and proceeding from there. This was proposed in the original [fat interface files](https://gitlab.haskell.org/ghc/ghc/issues/10871) proposal.

This is something of an odd feature: none of our use cases require this (and the approach to Backpack which would have required this was dropped). However, it *would* make testing much easier, and gives us a very clear invariant for the behaviour of the new interface section, namely:

> Compiling with the Core interface section and then resuming compilation should produce the same results as a normal compilation.

I think we should keep this in mind as a goal, and do it if it seems relatively easy.

# GHC API changes

The intended consumers of this section will probably want to load the content via the GHC API. We will therefore need some such function, perhaps something like:
```
loadCore :: ModIface -> m (Maybe ModGuts)
```
where `m` is some suitable monad. This must be able to fail, since we can't guarantee that any given interface file was compiled with the Core section.

# Unfoldings

GHC does provide some information about the Core for bindings in separately
compiled modules. "Unfoldings" are right-hand-sides of bindings, which are
included in interface files to allow them to be inlined when compiling a
dependent module.

Unfoldings, when present, provide a way to get access to the Core for a binding.
However, they are not intended for this purpose, and this has a number of
knock-on effects:

- There are very few guarantees about whether unfoldings will be present and for
  what.
  - `Issue 16615 <https://gitlab.haskell.org/ghc/ghc/issues/16615>`_ revealed a case where
    one of two mutually-recursive ``INLINABLE`` functions did not have an
    unfolding at the start of the compilation pipeline. In the end, we may be
    unable to change this if it affects optimization poorly.
- Changing which unfoldings are present has serious effects on optimization.
  This means:
  - Even fixing something that looks like a "bug" in unfoldings might be
    undesirable if it affects optimization adversely.
  - Adding more unfoldings to support white-box compilation will influence the
    optimization done in the normal case as well. This violates "Non-interference"
- There are no guarantees that unfoldings will continue to be suitable for
  white-box compilation.
  - The GHC developers could reasonably change the behaviour of unfoldings in
    pursuit of better optimization such that other use cases were broken.
  - The GHC developers should not be restricted from making such changes
    because people are (ab)using unfoldings in an unusual way.

Overall unfoldings are one of the most practical approaches at the moment, but I
think they are fundamentally unsuitable in the long run.

# Performance

Adding a Core section could cost us in two ways:
- Size of interface files will grow.
- Compilation time will increase due to serialization.

We should see how much of an impact this actually has: if it's small, we might even want to consider enabling it by default.

# Implementation

There is already work in progress to make interface files extensible.

Edward Yang wrote a patch for "fat interface files" that got quite far ([Github](https://github.com/ezyang/ghc/tree/ghc-fat-interface), [Phabricator](https://phabricator.haskell.org/D1318>)). This could be used as a starting point.