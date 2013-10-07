# Release plans for GHC 7.8

## Timeline

~~The planned timeline for 7.8 is to have a feature freeze approximately at the time of ICFP, followed by a period of frantic bugfixing. A release candidate (and the 7.8 branch) will be formed after this, likely sometime in October.~~


See recent ghc-devs posting [ GHC 7.8 Release Status & Schedule](http://permalink.gmane.org/gmane.comp.lang.haskell.ghc.devel/2569) for latest timeline.

## Tickets


We would like to fix all of the [ high and highest priority tickets in the 7.8.1 milestone](http://ghc.haskell.org/trac/ghc/query?priority=highest&priority=high&status=infoneeded&status=merge&status=new&status=patch&milestone=7.8.1&col=id&col=summary&col=status&col=type&col=priority&col=milestone&col=component&order=priority), but there are currently a lot of them so this seems optimistic. Please feel free to take a ticket and help us!

## Completed new features


The features already completed are documented in the release notes:
[docs/users_guide/7.8.1-notes.xml](/trac/ghc/browser/ghc/docs/users_guide/7.8.1-notes.xml)

## Pending new features


The following **new** features are planned for 7.8 **but have not yet made it**. They are at varying degrees of completeness, and may not all make it in.

- Jan Stolarek has completed the [boolean-primop story](prim-bool), but they are going through a major refactoring. **90% Complete**.

  - ~~Simon Marlow needs to accept patches sent by Jan Stolarek and either upload new versions of Alex and Happy to Hackage or tell Jan to do that~~
  - ~~Jan Stolarek can then push his changes into HEAD~~
  - ~~Jan Stolarek needs to send patches for primitive to Roman Leschinskiy.~~
  - ~~Roman Leschinskiy needs to upload new version of primitive to Hackage~~
  - ~~Herbert Valerio Riedel can then pull latest version of primitive into GHC tree.~~
  - Austin Seipp will implement shim package for backwards compatibility and update [ this wiki page accordingly](http://ghc.haskell.org/trac/ghc/wiki/NewPrimopsInGHC7.8). See also [ the compatibility module page](http://www.haskell.org/haskellwiki/Compatibility_Modules). **In progress**.

- ~~Geoff Mainland needs to merge the new Template Haskell implementation, and will do so very soon (see [ http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges](http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges) & [ http://gmainland.blogspot.co.uk/2013/05/type-safe-runtime-code-generation-with.html](http://gmainland.blogspot.co.uk/2013/05/type-safe-runtime-code-generation-with.html)).~~**Merged**.

- ~~Geoff Mainland is working generalising the support for SSE-like instructions. ETA: September 13.~~**Merged**.

- ~~Iavor Diatchki is working on type-level nats ([\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385), [ http://ghc.haskell.org/trac/ghc/wiki/TypeNats](http://ghc.haskell.org/trac/ghc/wiki/TypeNats)).  The plan is to support simple type-level arithmetic in GHC 7.8. The branch `type-nats-simple` has the code.~~**Merged**.

- Austin Seipp would like to do some official ARMv7 binary releases with a working stage2 compiler and GHCi. He believes the last major remaining bug is [\#7794](https://gitlab.haskell.org//ghc/ghc/issues/7794), which trips the info table generation for bytecode. Otherwise, GHC can build and successfully use Template-Haskell, vector/dph, etc.

- Austin also still has a lingering patch for [\#7602](https://gitlab.haskell.org//ghc/ghc/issues/7602) to fix a large OS X performance regression, but it's still not merged. The final details must be sorted out with Simon Marlow. ETA: First week of October.

- ~~Edsko de Vries and Luite Stegeman are working an improved story for hooking into the compilation pipeline, using the GHC API. They have improvements they'd like to land this week.~~**Merged**.

- Dynamic GHCi ([\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)). This is working in HEAD, and enabled if `DYNAMIC_GHC_PROGRAMS=YES`. Currently it's enabled by default if dynamic libraries are supported, except for FreeBSD and Windows.
  On FreeBSD the reason it's disabled is due to a bug in FreeBSD's rtld. This has been fixed, but we're waiting for the fix to make it into releases. This might be in time for 7.8, but certainly will be for 7.10. See [\#7819](https://gitlab.haskell.org//ghc/ghc/issues/7819).
  On Windows, there are a couple of build time annoyances: `-dynamic-too` doesn't work on Windows ([\#8228](https://gitlab.haskell.org//ghc/ghc/issues/8228)), and linking takes a very long time when dynamic linking is used ([\#8229](https://gitlab.haskell.org//ghc/ghc/issues/8229)). There's no technical reason why it couldn't be enabled, though.
  The plan is/was to use dynamic GHCi on as many platforms as possible in 7.8, and to remove support for non-dynamic-ghci in HEAD soon after. See discussion in [\#8039](https://gitlab.haskell.org//ghc/ghc/issues/8039), however.

- ~~Ryan Newton has added a larger set of atomic memory primops than were previously available. The set of PrimOps may continue to expand slightly, but for now includes `casArray#`, `casIntArray#`, and `fetchAddIntArray#`.~~**Done and merged**.

- ~~The Applicative-Monad warnings would preferably go in (see [\#8004](https://gitlab.haskell.org//ghc/ghc/issues/8004).)~~**Merged**.

- ~~[Newtype wrappers](newtype-wrappers), by Joachim Breitner.~~**Merged**.

## Features that will definitely not make it

- Trevor Elliot and Iavor Diatchki have implemented [GhcKinds/KindsWithoutData](ghc-kinds/kinds-without-data), but the implementation is still in flux. 

- [Overloaded record fields](records/overloaded-record-fields/plan), by Adam Gundry (GSOC). This is essentially done, apart from some refactoring, but will be merged into HEAD after 7.8 to allow time for tinkering with the design.

- [PatternSynonyms](pattern-synonyms), by Gergo Erdi. Progress tracked at [\#5144](https://gitlab.haskell.org//ghc/ghc/issues/5144).

- [Explicit type application](explicit-type-application) (Stephanie and her students)
