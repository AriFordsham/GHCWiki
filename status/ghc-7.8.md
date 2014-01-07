# Release plans for GHC 7.8

## Timeline


See recent ghc-devs posting [ GHC 7.8 Release Status & Schedule](http://permalink.gmane.org/gmane.comp.lang.haskell.ghc.devel/2569) for latest timeline.

## Tickets


We would like to fix all of the [ high and highest priority tickets in the 7.8.1 milestone](http://ghc.haskell.org/trac/ghc/query?priority=highest&priority=high&status=infoneeded&status=merge&status=new&status=patch&milestone=7.8.1&col=id&col=summary&col=status&col=type&col=priority&col=milestone&col=component&order=priority), but there are currently a lot of them so this seems optimistic. Please feel free to take a ticket and help us!

## Completed new features


The features already completed are documented in the release notes:
[docs/users_guide/7.8.1-notes.xml](/trac/ghc/browser/ghc/docs/users_guide/7.8.1-notes.xml)

## Pending new features


The following **new** features are planned for 7.8 **but have not yet made it**. They are at varying degrees of completeness, and may not all make it in.

- Austin Seipp needs to upload the primops compatibility package for 7.8. This is mostly a copy of `compiler/utils/ExtsCompat64.hs` into a Cabal package. See also [ the compatibility module page](http://www.haskell.org/haskellwiki/Compatibility_Modules). **In progress**.

- Austin Seipp needs to merge a patch from Ben Gamari to fix LLVM + dynamic linking.

- Austin also still has a lingering patch for [\#7602](https://gitlab.haskell.org//ghc/ghc/issues/7602) to fix a large OS X performance regression, but it's still not merged. This is because the thread-local storage implementation changed in OS X Mavericks, requiring more investigation.

- Dynamic GHCi ([\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)). This is working in HEAD, and enabled if `DYNAMIC_GHC_PROGRAMS=YES`, which causes GHC itself to be built dynamically. Currently it's enabled by default if dynamic libraries are supported, except for FreeBSD and Windows.
  On FreeBSD the reason it's disabled is due to a bug in FreeBSD's rtld. This has been fixed, but we're waiting for the fix to make it into releases. This might be in time for 7.8, but certainly will be for 7.10. See [\#7819](https://gitlab.haskell.org//ghc/ghc/issues/7819).
  On Windows, there are a couple of build time annoyances: `-dynamic-too` doesn't work on Windows ([\#8228](https://gitlab.haskell.org//ghc/ghc/issues/8228)), and linking takes a very long time when dynamic linking is used ([\#8229](https://gitlab.haskell.org//ghc/ghc/issues/8229)). There's no technical reason why it couldn't be enabled, though.
  The plan is/was to use dynamic GHCi on as many platforms as possible in 7.8, and to remove support for non-dynamic-ghci in HEAD soon after. See discussion in [\#8039](https://gitlab.haskell.org//ghc/ghc/issues/8039), however.
