# GHC Status Report, May 2016


As usual, GHC development churns onward - and **GHC 8.0 is right around the corner**! The final set of bugs are being fixed, and we hope to have a final release candidate, followed by the final release, in just a few weeks.

# Major changes in GHC 8.0.1

- **Support for simple, implicit callstacks with source locations** \[ImplicitCallstacks\] implicit parameters providing callstacks/source locations\], allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([ Phab:D861](https://phabricator.haskell.org/D861))

- **Injective type families** \[[InjectiveTypeFamilies](injective-type-families)\]. Injective TFs allow you to specify type families which are injective, i.e. have a one-to-one relationship. ([ Phab:D202](https://phabricator.haskell.org/D202)).

- **Applicative do notation** \[[ApplicativeDo](applicative-do)\]. With the new `-XApplicativeDo`, GHC tries to desugar do-notation to `Applicative` where possible, giving a more convenient sugar for many common Applicative expressions. ([ Phab:D729](https://phabricator.haskell.org/D729))

- **A beautiful new users guide**. Now rewritten in reStructured Text, and with significantly improved output and documentation.

- **Visible type application** - \[[ExplicitTypeApplication](explicit-type-application)\]. This allows you to say, for example `id @Bool` to specialize `id` to `Bool -> Bool`. With this feature, proxies are never needed.

- **Kind Equalities**, which form the first step to building Dependent Haskell. This feature enables promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. ([ Phab:D808](https://phabricator.haskell.org/D808))

- **Record system enhancements** \[[OverloadedRecordFields](overloaded-record-fields)\]. At long last, `OverloadedRecordFields` will finally be available in GHC 8.0, allowing multiple uses of the same field name and a form of type-directed name resolution.

- A huge improvement to pattern matching (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [ their paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf).

- **More Backpack improvements**. There's a new user-facing syntax which allows multiple modules to be defined a single file, and we're hoping to release at least the ability to publish multiple "units" in a single Cabal file. **TODOFIXME**: Edward, is there ANY documentation about this?!

- **Support for DWARF-based stacktraces** \[DWARF\]. Haskell has at long last gained the ability to collect stack-traces of running programs. While still experimental, `base` now includes an interface which user code can use to request a representation of the current execution stack when running on a supported machine (currently Linux x86-64). Furthermore, the runtime system will now provide a backtrace of the currently running thread when thrown a `SIGUSR2` signal. Note that this functionality is highly experimental and there are some known issues which can potentially threaten the stability of the program.

- **Remote GHCi**[RemoteGHCi](remote-gh-ci) The `-fexternal-interpreter` flag tells GHC to run interpreted code in a separate process.  This provides various benefits, including allowing the interpreter to run profiled code (for example), thereby gaining access to [ stack traces](http://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html) in GHCi.

- A new **Strict language extension** \[[StrictPragma](strict-pragma)\] implemented by Adam Sandberg Eriksson and Johan Tibell

- Significant improvements in cross-platform support, including a variety of fixes to **Windows linker support** ([ Phab:D1696](https://phabricator.haskell.org/D1696), [ Phab:D1805](https://phabricator.haskell.org/D1805)), great improvements in reliability on ARM ([\#11206](https://gitlab.haskell.org//ghc/ghc/issues/11206)), revived unregisterised \[[ support](https://trofi.github.io/posts/191-ghc-on-m68k.html|m68k), and new support for AIX targets (Herbert)

# Upcoming plans for GHC 8.2


Naturally, there were several things we didn't get around to this cycle, or things which are still in flight and being worked on. (And you can always try to join us if you want something done!)

## Libraries, source language, type system

- **Indexed `Typeable` representations**TypeableT/BenGamari? (Ben Gamari, Simon Peyton Jones, etc). While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

> >
> > GHC 8.2 will address this by introducing indexed type representations, leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [ http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/\|paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/|paper) for an description of the proposal and the [ https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari\|Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari|Wiki) for the current status of the implementation.

- Future source-visible Backpack plans? (Edward should answer)
- What about MonadFail? (Herbert, David L)
- FIXME accumulate some of the scattered changes/plans for `base`. (Edward K, Austin, Herbert?)

## Back-end and runtime system

- Compact changes? (Edward Yang)
- Maybe mention -fexternal-interpreter here? (Simon Marlow)
- More work 

## Frontend, build system and miscellaneous changes

- New shake build system is Really Finally Going to get merged? (Andrey)
- The ImprovedLLVMBackend plan didn't make the cut for 8.0, but will for 8.2 (Austin)
- Donated Mac buildbot, thanks to FutureIce! (Austin)
- GHC-pkg is learning some new tricks, RE: environment files and Cabal changes. (Austin/Duncan?)
- 

# Development updates and "Thank You"s


2015 has been a remarkable year for GHC development. 


Insert incredible levels of praise and adoration for contributors like Thomas, Tamar, Ömer, etc, and
lots of our newer regular contributors like Ryan, Michael Sloan, etc.

# References


Insert many links pointing deeply into the web, so you can read even more.
