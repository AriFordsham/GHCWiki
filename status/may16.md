# GHC Status Report, May 2016


GHC development churns onward - and **GHC 8.0 is right around the corner**! The final set of bugs are being fixed, and we hope to have a final release candidate, followed by the final release, in just a few weeks.

# Major changes in GHC 8.0.1

- **Support for simple, implicit callstacks with source locations**[Implicit parameters providing callstacks/source locations](explicit-call-stack/implicit-locations), allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([ Phab:D861](https://phabricator.haskell.org/D861))

- **Injective type families** ([Wiki](injective-type-families), [ paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf)). Allows to annotate type families with injectivity information. Correctness of injectivity annotation is then verified by the compiler. Once compiler knows the annotation is correct it can us injectivity information during type checking.

- **Applicative do notation** ([ApplicativeDo](applicative-do), [ Phab:D729](https://phabricator.haskell.org/D729)). With the new `-XApplicativeDo`, GHC tries to desugar `do`-notation to `Applicative` where possible, giving a more convenient sugar for many common `Applicative` expressions. See the [ draft paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/list-comp/applicativedo.pdf) for details.

- **A beautiful new users guide**. Now rewritten in reStructured Text, and with significantly improved output and documentation.

- **Visible type application**. ([ExplicitTypeApplication](explicit-type-application)). This allows you to say, for example, `id @Bool` to specialize `id` to `Bool -> Bool`. With this feature, proxies are needed only in data constructors for pattern matching. Visible type patterns are due to be included sometime in the indeterminate future.

- **Kind Equalities**, ([DependentHaskell/Phase1](dependent-haskell/phase1)) which form the first step to building Dependent Haskell. This feature enables promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`.

- **Record system enhancements** ([OverloadedRecordFields](overloaded-record-fields)). A new extension `DuplicateRecordFields` will be available in GHC 8.0, allowing multiple uses of the same field name with a very limited form of type-directed name resolution. Support for polymorphism over record fields is being worked on; another provisional new extension `OverloadedLabels` represents a first step in this process.

- A huge improvement to **pattern match checking** (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see their [ their paper](https://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf) with Tom Schrijvers and Dimitrios Vytiniotis. Also, more information can be found in [PatternMatchCheck](pattern-match-check) and [PatternMatchCheckImplementation](pattern-match-check-implementation). 

- **Custom type errors** ([Proposal/CustomTypeErrors](proposal/custom-type-errors)), allowing library authors to offer more descriptive error messages than those offered by GHC.

- **Improved generics representation** leveraging type-level literals. This makes `GHC.Generics` more expressive and uses new type system features to give more natural types to its representations.

- A new **DeriveLift language extension**, allowing the `Lift` type class from `Language.Haskell.TH.Syntax` to be derived automatically. This was introduced in the spirit of `DeriveDataTypeable` and `DeriveGeneric` to allow easier metaprogramming, and to allow users to easily define `Lift` instances without needing to depend on the existence of Template Haskell itself (see [\#1830](https://gitlab.haskell.org//ghc/ghc/issues/1830)).

- **More Backpack improvements**. There's a new user-facing syntax which allows multiple modules to be defined a single file, and we're hoping to release at least the ability to publish multiple "units" in a single Cabal file.

- **Support for DWARF-based stacktraces** ([DWARF](dwarf)). Haskell has at long last gained the ability to collect stack-traces of running programs. While still experimental, `base` now includes an interface which user code can use to request a representation of the current execution stack when running on a supported machine (currently Linux x86-64). Furthermore, the runtime system will now provide a backtrace of the currently running thread when thrown a `SIGUSR2` signal. Note that this functionality is highly experimental and there are some known issues which can potentially threaten the stability of the program.

- **Remote GHCi** ([RemoteGHCi](remote-gh-ci)). The `-fexternal-interpreter` flag tells GHC to run interpreted code in a separate process.  This provides various benefits, including allowing the interpreter to run profiled code (for example), thereby gaining access to [ stack traces](http://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html) in GHCi.

-  GHC now supports **environment files**. This is not any fundamental new capability but may prove to be a useful convenience. Build systems like Cabal call GHC with flags that define an (ephemeral) package environment, such as `-hide-all-packages -package-db=... -package this -package that`. An environment file lets the same information be stashed persistently in a file that GHC will pick up and use automatically. In principle this allows tools such as `Cabal` to generate an environment file and then you can use `ghc` or `ghci` directly and get the package environment of your project, rather than the default global environment. In addition to environments that live in a particular directory, it is possible to make a default global environment, or different global environments for different shell sessions. 

- A new **Strict language extension** ([StrictPragma](strict-pragma)), allowing modules to be compiled such that local bindings are evaluated eagerly. Implemented by Adam Sandberg Eriksson based on an proposal by Johan Tibell.

- Significant improvements in cross-platform support, including a variety of fixes to **Windows linker support** ([ Phab:D1696](https://phabricator.haskell.org/D1696), [ Phab:D1805](https://phabricator.haskell.org/D1805)), great improvements in **reliability on ARM** ([\#11206](https://gitlab.haskell.org//ghc/ghc/issues/11206)), revived **unregisterised [ m68k support](https://trofi.github.io/posts/191-ghc-on-m68k.html)**, and new support for **AIX targets** (Herbert) and Linux **PowerPC 64-bit big- and little-endian native code generation** ([ Phab:D629](https://phabricator.haskell.org/D629)).

- Improved support for **pattern synonyms**, including record syntax ([\#8582](https://gitlab.haskell.org//ghc/ghc/issues/8582)) and the ability to associate pattern synonyms with type constructors on export, implemented by Matthew Pickering. See Matthew's [ blog](http://mpickering.github.io/posts/2015-12-12-pattern-synonyms-8.html) for details.

# Upcoming plans for GHC 8.2


With the super-major GHC 8.0 release out the door, plans have begun to form for the next major release, 8.2. Given that 8.0 saw a remarkable amount of churn, we hope to make the focus of 8.2 consolidation, stabilization, and optimization.  For this reason, we hope you'll note there are relatively few *new* features in the lists below; instead we'd like to encourage contributors to polish, optimize, document, refactor, or finish the features we already have.


Of course, GHC only evolves because of its contributors. Please let us know if you have a pet project that you'd like to see merged!

## Libraries, source language, type system

- **Indexed `Typeable` representations**[Typeable/BenGamari](typeable/ben-gamari) (Ben Gamari, Simon Peyton Jones, etc). While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

>
> GHC 8.2 will address this by introducing indexed type representations, leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [ paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/) for an description of the proposal and the [ Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari) for the current status of the implementation.

- Backpack continues its march forward (Edward Z Yang)
- Merge `Bifoldable` and `Bitraversable` into `base` (Edward Kmett, Ryan Scott)
- Generalize the `deriving` algorithms for `Eq`, `Functor`, etc. to be able to derive the data types in `Data.Functor.Classes` (`Eq1`, `Eq2`, etc.), `Bifunctor`, `Bifoldable`, and `Bitraversable` (Ryan Scott)
- Deriving strategies (Ryan Scott): grant users the ability to choose explicitly how a class should be `derived` (using a built-in algorithm, `GeneralizedNewtypeDeriving`, `DeriveAnyClass`, or otherwise), addressing [\#10598](https://gitlab.haskell.org//ghc/ghc/issues/10598).
- Exhaustiveness checking for `EmptyCase`s ([ Phab:D2105](https://phabricator.haskell.org/D2105)), addressing [\#10746](https://gitlab.haskell.org//ghc/ghc/issues/10746). 

## Back-end and runtime system

- Compact regions (Giovanni Campagna, Edward Yang, [ Phab:D1264](https://phabricator.haskell.org/D1264)): [ paper](http://ezyang.com/papers/ezyang15-cnf.pdf)

- Refactoring and improvements to the cost-center profiler (Ben Gamari, [ Phab:D1722](https://phabricator.haskell.org/D1722)): Allow heap profiler samples to be directed to the GHC eventlog, allowing correlation with other program events, enabling easier analysis by tooling and eventual removal of the old, rather crufty `.hp` profile format. 

- Further improvements to debugging information (Ben Gamari): There are still a number of outstanding issues with GHC's DWARF implementation, some of which even carry the potential to crash the runtime system during stacktrace collection. GHC 8.2 will hopefully have these issues resolved, allowing debugging information to be used by end-user code in production.

>
> With stable stack unwinding support comes a number of opportunities for new serial and parallel performance analysis tools (e.g. statistical profiling) and debugging. As GHC's debugging information improves, we expect to see tooling developed to support these applications. See the [ DWARF status page](https://ghc.haskell.org/trac/ghc/wiki/DWARF/80Status) for futher information.

- Support for NUMA systems (Simon Marlow, [ in-progress](https://github.com/simonmar/ghc/tree/numa)).  The aim is to reduce the number of remote memory accesses for multi-socket systems that have a mixture of local and remote memory.

- Experimental changes to the scheduler (Simon Marlow, [ in progress](https://github.com/simonmar/ghc/commit/7e05ec18b4eda8d97e37015d415e627353de6b50)) that enable the number of threads used for GC to be lower than the `-N` setting.

## Frontend, build system and miscellaneous changes

- New Shake-based build system, `hadrian`, will be merged.  (Andrey Mokhov)
- The [improved LLVM backend plan](improved-llvm-backend) plan didn't make the cut for 8.0, but will for 8.2 (Austin Seipp)

# Development updates and acknowledgments


2015 has been a remarkable year for GHC development. Over the last twelve months the GHC repository gained nearly 2500 commits by over one hundred authors. Of these authors, nearly half are first-time contributors. At the time of writing alone alone there is over one dozen open and actively updated differentials on Phabricator. It seems fair to say that GHC's development community is stronger than ever.


We have been very lucky to have Thomas Miedema, who has devoted countless hours triaging bugs, cleaning up the build system, advising new contributors, and generally helping out in a multitude of ways which often go un-noticed. We all have benefited immensely from Thomas' hard work and generosity; thanks Thomas!


Another contributor deserving of recognition is Herbert Valerio Riedel, who meticulously handles many of the finer details of GHC development: over the past year Herbert has spent numerous weekends thinking through compatibility considerations for library and warning changes, managing GHC's interaction with its external core libraries, cleaning up GHC's build system, and maintaining his invaluable PPA repository for Ubuntu and Debian-based systems. Our releases wouldn't be nearly as smooth without Herbert's attention to detail.


On the Windows front, Tamar Christina has been doing amazing work cleaning up the many nooks that have gone unattended until now. His work on the runtime linker should mean that GHC 8.0 will be considerably more reliable when linking against many Windows libraries.


The past year has brought a number of new contributors: Ryan Scott and Michael Sloan have picked up various generics and Template Haskell projects, Andrew Farmer has contributed a number of fixes to the cost-centre profiler, and Bartosz Nitka has made numerous contributions improving compiler determinism. We also also saw the beginnings of some very interesting work from Ömer Sinan Ağacan, who is looking at teaching GHC to unpack sum types. David Lupochainsky and Herbert Valerio Riedel have also started honing GHC's warnings system by both bringing consistency to the currently rather organic flags and making the messages themselves more informative. George Karachalias merged his full rewrite of the pattern match checker, which is now far more precise than GHC's previous implementation.


In recent years the growth in the Haskell community has required that better develop our infrastructure for change management. This has lead to the formation of the Core Libraries Committee, which is now in its third year of existence. As such, we are now beginning to see some of the committee's efforts come to fruition. With GHC 8.0 progress was made on all three currently active proposals:

- `Semigroup`-`Monoid` proposal: the `Data.Semigroup` module is now available in `base` and there are now opt-in warnings for missing `Semigroup` instances in preparation for the eventual addition of `Semigroup` as a superclass of `Monoid`

- `MonadFail` proposal: the `Control.Monad.Fail` module is available in `base` and a `-XMonadFailDesugaring` language extension has been added, allowing users to use the new class in `do` desugaring

- `ExpandFloating` proposal: `expm1`, `log1p`, `log1pexp`, `log1mexp` have been added to the `Floating` class with defaults


Of course, GHC has also benefited from countless more contributors who we don't have room to acknowledge here. We'd like to thank everyone who has contributed patches, bug reports, code reviews, and discussion to the GHC community over the last year. GHC only improves through your efforts!

# References


Insert many links pointing deeply into the web, so you can read even more.
