# Release plans for GHC 7.8


The following new features are planned for 7.8. They are at varying degrees of completeness, and may not all make it in.

- Unboxed Booleans. Patch just needs some polishing? Jan Stolarek is working on it. See [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135).

- Dynamic GHCi ([\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)). This is working in HEAD, and enabled if `DYNAMIC_GHC_PROGRAMS=YES`. Currently it's enabled by default if dynamic libraries are supported, except for FreeBSD and Windows.
  On FreeBSD the reason it's disabled is due to a bug in FreeBSD's rtld. This has been fixed, but we're waiting for the fix to make it into releases. This might be in time for 7.8, but certainly will be for 7.10. See [\#7819](https://gitlab.haskell.org//ghc/ghc/issues/7819).
  On Windows, there are a couple of build time annoyances: `-dynamic-too` doesn't work on Windows, and linking takes a very long time when dynamic linking is used. There's no technical reason why it couldn't be enabled, though.
  The plan is/was to use dynamic GHCi on as many platforms as possible in 7.8, and to remove support for non-dynamic-ghci in HEAD soon after. See discussion in [\#8039](https://gitlab.haskell.org//ghc/ghc/issues/8039), however.
