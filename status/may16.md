# GHC Status Report, May 2016


GHC development churns onward - and **GHC 8.0 is right around the corner**! The final set of bugs are being fixed, and we hope to have a final release candidate, followed by the final release, in just a few weeks.

# Major changes in GHC 8.0.1

- **Support for simple, implicit callstacks with source locations** \[ImplicitCallstacks\] implicit parameters providing callstacks/source locations\], allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([ Phab:D861](https://phabricator.haskell.org/D861))

- **Injective type families** ([Wiki](injective-type-families), [ http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf\|paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf|paper)). Allows to annotate type families with injectivity information. Correctness of injectivity annotation is then verified by the compiler. Once compiler knows the annotation is correct it can us injectivity information during type checking.

- **Applicative do notation** \[[ApplicativeDo](applicative-do)\]. With the new `-XApplicativeDo`, GHC tries to desugar `do`-notation to `Applicative` where possible, giving a more convenient sugar for many common `Applicative` expressions. ([ Phab:D729](https://phabricator.haskell.org/D729))

- **A beautiful new users guide**. Now rewritten in reStructured Text, and with significantly improved output and documentation.

- **Visible type application** - \[[ExplicitTypeApplication](explicit-type-application)\]. This allows you to say, for example, `id @Bool` to specialize `id` to `Bool -> Bool`. With this feature, proxies are never needed.

- **Kind Equalities**, which form the first step to building Dependent Haskell. This feature enables promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. ([ Phab:D808](https://phabricator.haskell.org/D808))

- **Record system enhancements** \[[OverloadedRecordFields](overloaded-record-fields)\]. At long last, `OverloadedRecordFields` will finally be available in GHC 8.0, allowing multiple uses of the same field name and a form of type-directed name resolution.

- A huge improvement to **pattern match checking** (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [ http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf their paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf their paper).

- **Custom type errors** \[CustomTypeErrors\], allowing library authors to offer more descriptive error messages than those offered by GHC.

- **Improved generics representation** leveraging type-level literals. This makes `GHC.Generics` more expressive and uses new type system features to give more natural types to its representations.

- A new **DeriveLift language extension**, allowing the `Lift` type class from `Language.Haskell.TH.Syntax` to be derived automatically. This was introduced in the spirit of `DeriveDataTypeable` and `DeriveGeneric` to allow easier metaprogramming, and to allow users to easily define `Lift` instances without needing to depend on the existence of Template Haskell itself.

- **More Backpack improvements**. There's a new user-facing syntax which allows multiple modules to be defined a single file, and we're hoping to release at least the ability to publish multiple "units" in a single Cabal file. **TODOFIXME**: Edward, is there ANY documentation about this?!

- **Support for DWARF-based stacktraces** \[DWARF\]. Haskell has at long last gained the ability to collect stack-traces of running programs. While still experimental, `base` now includes an interface which user code can use to request a representation of the current execution stack when running on a supported machine (currently Linux x86-64). Furthermore, the runtime system will now provide a backtrace of the currently running thread when thrown a `SIGUSR2` signal. Note that this functionality is highly experimental and there are some known issues which can potentially threaten the stability of the program.

- **Remote GHCi**[RemoteGHCi](remote-gh-ci) The `-fexternal-interpreter` flag tells GHC to run interpreted code in a separate process.  This provides various benefits, including allowing the interpreter to run profiled code (for example), thereby gaining access to [ stack traces](http://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html) in GHCi.

- A new **Strict language extension** \[[StrictPragma](strict-pragma)\], allowing modules to be compiled such that local bindings are evaluated eagerly. Implemented by Adam Sandberg Eriksson based on an proposal by Johan Tibell.

- Significant improvements in cross-platform support, including a variety of fixes to **Windows linker support** ([ Phab:D1696](https://phabricator.haskell.org/D1696), [ Phab:D1805](https://phabricator.haskell.org/D1805)), great improvements in reliability on ARM ([\#11206](https://gitlab.haskell.org//ghc/ghc/issues/11206)), revived unregisterised [ m68k support](https://trofi.github.io/posts/191-ghc-on-m68k.html), and new support for AIX targets (Herbert) and Linux PowerPC 64-bit big endian and little endian native code generation ([ Phab:D629](https://phabricator.haskell.org/D629)).

# Upcoming plans for GHC 8.2


With the super-major GHC 8.0 release out the door, plans have begun to form for the next major release, 8.2. Given that 8.0 saw a remarkable amount of churn, we hope to make the focus of 8.2 consolidation, stabilization, and optimization.  For this reason, we hope you'll note there are relatively few *new* features in the lists below; instead we'd like to encourage contributors to polish, optimize, document, refactor, or finish the features we already have.


Of course, GHC only evolves because of its contributors. Please let us know if you have a pet project that you'd like to see merged!

## Libraries, source language, type system

- **Indexed `Typeable` representations**[Typeable/BenGamari](typeable/ben-gamari) (Ben Gamari, Simon Peyton Jones, etc). While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

>
> GHC 8.2 will address this by introducing indexed type representations, leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [ paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/) for an description of the proposal and the [ Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari) for the current status of the implementation.

- Future source-visible Backpack plans? (Edward should answer)
- What about `MonadFail`? (Herbert, David L)
- 
- FIXME accumulate some of the scattered changes/plans for `base`. (Edward K, Austin, Herbert?)

## Back-end and runtime system

- Compact regions (Giovanni Campagna, Edward Yang, [ Phab:D1264](https://phabricator.haskell.org/D1264)): [ paper](http://ezyang.com/papers/ezyang15-cnf.pdf)

- Maybe mention -fexternal-interpreter here? (Simon Marlow)

- Refactoring and improvements to the cost-center profiler (Ben Gamari, [ Phab:D1722](https://phabricator.haskell.org/D1722)): Allow
  heap profiler samples to be directed to the GHC eventlog, allowing
  correlation with other program events, enabling easier analysis by tooling,
  and eventual removal of the old, rather crufty profile format.

- Further improvements to debugging information (Ben Gamari): There are still a number of outstanding issues with GHC's DWARF implementation, some of which even carry the potential to crash the program during stacktrace collection. GHC 8.2 will hopefully have these issues resolved, allowing debugging information to be used by end-user code in production.

>
> With stable stack unwinding support comes a number of opportunities for new serial and parallel performance analysis tools (e.g. statistical profiling) and debugging. As GHC's debugging information improves, we expect to see tooling developed to support these applications. See the [ DWARF status page](https://ghc.haskell.org/trac/ghc/wiki/DWARF/80Status) for futher information.

- TODO What else?

## Frontend, build system and miscellaneous changes

- New shake build system is Really Finally Going to get merged? (Andrey)
- The [improved LLVM backend plan](improved-llvm-backend) plan didn't make the cut for 8.0, but will for 8.2 (Austin Seipp)
- `ghc-pkg` is learning some new tricks, RE: environment files and Cabal changes. (Austin/Duncan?)
- 

# Development updates and "Thank You"s


2015 has been a remarkable year for GHC development. Over the last twelve months the GHC repository gained nearly 2500 commits by over one hundred authors. At the time of writing alone alone there is over a dozen open and actively updated differentials on Phabricator. It seems fair to say that GHC's development community is stronger than ever.


We have been very lucky to have Thomas Miedema, who has devoted countless hours triaging bugs, cleaning up the build system, advising new contributors, and helping out in a multitude of ways which often go un-noticed. We all have benefitted immensely from Thomas' hard work; thanks Thomas!


Herbert Valerio Riedel has also been as act


On the Windows front, Tamar Christina has been doing amazing work cleaning up the many nooks that have gone unattended until now. His work on the runtime linker should mean that GHC 8.0 will be considerably more reliable when linking against many Windows libraries.


The past year has brought a number of new contributors: Ryan Scott and Michael Sloan have picked up various generics and Template Haskell projects, Andrew Farmer has contributed a number of fixes to the cost-centre profiler, and Bartosz Nitka has made numerous contributions improving compiler determinism. We also also saw the beginnings of some very interesting work from Ömer Sinan Ağacan, who is looking at teaching GHC to unpack sum types.


Insert incredible levels of praise and adoration for contributors like Thomas, Tamar, Ömer, etc, and lots of our newer regular contributors like Ryan, Michael Sloan, etc.

# References


Insert many links pointing deeply into the web, so you can read even more.
