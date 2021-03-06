# GHC Status Report, May 2015


GHC 7.10.1 was released in March this year, shipping several major improvements, and development continues to steam forward as it always does. However, things have been relatively quiet for most of 2015 so far, as people have simply been working away. Currently our `master` branch does not heavily diverge much from GHC 7.10, but that could change soon!

## Major changes in GHC 7.10.1



When we shipped GHC 7.10, we incorporated some major new features - but not without some major decision making, it turns out. These included:


- **Making Applicative a superclass of Monad** - Yes, finally!

- **Generalizing Prelude operators** [\[Prelude710](prelude710)\].  Known in various circles as 'The Burning-Bridges Proposal' (BBP), 'The Foldable Traversable Proposal' (FTP), this proposal offered to generalize many `Prelude` operations to functions in `Data.Traversable` and `Data.Foldable`. However, this plan stirred up a relatively large amount of debate regarding deviations from the standard, communicating plans, and the implications will be. In the end, Simon Peyton Jones and Simon Marlow [sought feedback from the community](https://mail.haskell.org/pipermail/libraries/2015-February/024925.html), and ended up [ making a final decision](https://mail.haskell.org/pipermail/libraries/2015-February/025009.html) in February, after hundreds of input votes from the community, and decided to move forward with the plan.

- **Distributed programming, `static` values, and reflection**  [\[StaticPointers](static-pointers)\].  Mathieu Boespflug and Facundo Dom nguez at TweagIO completely reimplemented their old proposal for `static` values, primarily intended to support Cloud Haskell, and it was merged into GHC 7.10. The support as it stands should be experimental, but the new implementation is much simpler and easier to understand.  This is part of a larger project involving runtime reflection [\[Typeable](typeable)\] and distributed progmraming [\[DistributedHaskell](distributed-haskell)\],

- **Binary literals** (#9224) - Herbert Valerio Riedel implemented the `-XBinaryLiterals` language extension which finally closes the syntax gap relative to other languages which allow to write base-2 literals such as `0b11001001`.

- **Partial type signatures** [\[PartialTypeSignatures](partial-type-signatures)\]. Thomas Winant and Dominique Devriese implemented partial type signatures for GHC. A partial type signature is a type signature that can contain *wildcards*, written as underscores. These wildcards can be types unknown to the programmer or types he doesn't care to annotate. The type checker will use the annotated parts of the partial type signature to type check the program, and infer the types for the wildcards. A wildcard can also occur at the end of the constraints part of a type signature, which indicates that an arbitrary number of extra constraints may be inferred. Whereas `-XTypedHoles` allow holes in your terms, `-XPartialTypeSignatures` allow holes in your types!

- **Preliminary backpack support** - Edward Yang has been working tirelessly on support for Backpack features in GHC. GHC 7.10 shipped with some preliminary code to support it, including signature file support and some Cabal support, but we have a new plan for GHC 7.12, with new syntax and a new implementation strategy. You can find out more by checking out the algorithm specification [https://github.com/ghc/ghc/blob/master/docs/backpack/algorithm.pdf](https://github.com/ghc/ghc/blob/master/docs/backpack/algorithm.pdf) . Work is currently proceeding on the ghc-backpack branch [ https://github.com/ezyang/ghc/tree/ghc-backpack](https://github.com/ezyang/ghc/tree/ghc-backpack) \[Backpack\]

- **Reimplemented GMP-based `Integer` backend (#9281)** - Herbert Valerio Riedel completely reimplemented the `integer-gmp` backend, and is now shipping it on all Tier 1 platforms. This should make interoperation with GMP (and C libraries that depend on GMP) radically simpler, while being easier to maintain.

- **DWARF support for debugging symbols** [\[DWARF](dwarf)\]. Peter Wortmann has gotten the first piece of his long-term work in place: support for GHC to emit DWARF symbols to object files, so debuggers can utilize it. The preliminary support works for simple cases, but is very experimental! (Case in point: it was broken in 7.10.1 due to #10236.) \[DWARF\]

- **API Annotations and other GHC API changes** [\[ApiAnnotations](api-annotations)\]. Alan Zimmerman has added API Annotations to the AST, so that the precise layout of the original source code can be regenerated. An initial library making use of these to fully round trip Haskell source code is here [https://github.com/alanz/ghc-exactprint](https://github.com/alanz/ghc-exactprint). This will be updated shortly after 7.10.2 comes out, and then used by HaRe to handle the low level AST manipulation.  Also, the landmines have been removed from the AST, so that traversals over it no longer need to tiptoe around embedded `panic` values. Andrew Gibiansky has added more parser entry points, so that tools can now parse fragments of source code [\[GhcApi](ghc-api)\]

- **Typechecker plugins** [\[TCPlugins](plugins/type-checker)\]. Iavor Diatchki, Eric Seidel and Adam Gundry implemented preliminary support for extending the typechecker using plugins, making it easier to experiment with custom constraint solvers. 

# Upcoming plans for the next release



The current plan is to steam forward to the end of the year, and begin to get ready for a new release, probably in February of 2016. We have some tentative plans marked below - and some of them are huge! In particular - we may ship GHC 8.0 next year, if we're going to change the entire Core language!


## Libraries, source language, type system


- **Signature sections**.  Lennart Augustsson is implementing `(:: ty)` to work the same as `(\x -> x :: ty)`. FIXME is there a ticket for this?

- **ApplicativeDo**  [\[ApplicativeDo](applicative-do)\]. Now that `Applicative` is a superclass of `Monad`, Simon Marlow has implemented a new extension for GHC, which will allow `do` notation to be used in the context of `Applicative`, not just `Monad`. The patch for review is available at [https://phabricator.haskell.org/D729](https://phabricator.haskell.org/D729), and Simon Marlow believes it's ready for review and merge.

- **Overloaded record fields** [\[OverloadedRecordFields](records/overloaded-record-fields)\]. After countless more discussions and several revisions, Adam Gundry implemented the new `-XOverloadedRecordFields` extension for GHC - again! - but this time with a newer design - and the first piece of the implementation is up for review at [https://phabricator.haskell.org/D761](https://phabricator.haskell.org/D761) - we're hoping to review it and integrate it soon.

- **Using an SMT Solver as a type-checker plugin** [\[repo](https://github.com/yav/type-nat-solver)\] [ \[paper](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)\]. Iavor Diatchki is working on implementing support for using SMT solvers in the typechecker, via the plugins mechanism. Currently, the main focus for this is improved support for reasoning with type-level natural numbers, but it opens the doors to other interesting functionality, such as supported for lifted (i.e., type-level) `(&&)`, and `(||)`, type-level bit-vectors (perhaps this could be used to implement type-level sets of fixed size), and others. 

- **Kind equality, kind coercions, and dependently typed Core** \[KindEqualities\].  Richard Eisenberg (with support from Simon PJ and Stephanie Weirich, among others) is implementing a change to the Core language, as described in "[System FC with explicit kind equality](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf)" \[FCkinds\].  When this work is complete, *all* types will be promotable to kinds, and *all* data constructors will be promotable to types. This will include promoting type synonyms and type families. As the details come together, there may be other source language effects, such as the ability to make kind variables explicit. It is not expected for this to be a breaking change -- the change should allow strictly more programs to be accepted.
  This can also go down as one of the **larger** changes in recent memory - [https://phabricator.haskell.org/D808](https://phabricator.haskell.org/D808) is the biggest Phabricator review we've done to date, changing over 10,000 lines of code in the compiler!

- **Injective type families**  [\[InjectiveTypeFamilies](injective-type-families)\].  Jan Stolarek (with support from Richard Eisenberg and Simon PJ) is working on adding injective type families to GHC. With this feature it will be possible to annotate declaration of a type family - closed, open or associated with class - with injectivity annotation and GHC will be able to use that information during type checking.

- **Safe Haskell & Overlapping Instances** [SafeOverlappingInstances](safe-haskell/new-overlapping-instances).  David Terei has overhauled how overlapping instances work under Safe Haskell. This greatly expands the number of regular Haskell programs that work under Safe Haskell and makes use of the new per-instance overlapping instances added in GHC 7.10. It also unifies how overlapping instances work when inferring a modules safety, vs. explicit use of `-XSafe`. A bug in the previous design.

- **Safe Haskell, GND & Roles** \[[SafeRoles](safe-roles)\].  David Terei and Richard Eisenberg are currently discussing possible changes to how Roles should work to allow them to be included in the safe-language of Safe Haskell. These are early discussions with no changes yet planned, but they'd love any feedback. The wiki page contains a wealth of information. Both the background and possible paths forward.

## Back-end and runtime system

- **CPU-specific optimizations** - Austin Seipp is currently investigating the implementation of CPU-specific optimisations for GHC, including new `-march` and `-mcpu` flags to adjust tuning for a particular processor. Right now, there is some preliminary work towards optimizing copies on later Intel machines. There's interest in expanding this further as well.

- **Changes to static closures for faster garbage collection** - Edward Yang is working on an overhaul of how static closures represented at runtime to eliminate some expensive memory dereferences in the GC hotpath. The initial results are encouraging: these changes can result in an up to 8% in the runtime of some GC heavy benchmarks, see #8199.

- **DWARF-based stack tracing** [\[DWARF](dwarf)\]. Peter Wortmann and Arash Rouhani (with support from the Simons) are working on enabling GHC to now use the DWARF debugging information it generates. This should allow us to obtain stack traces and do profiling without the need for instrumentation, directly from Haskell executables.

- **An Improved LLVM Backend** [\[ImprovedLLVMBackend](improved-llvm-backend)\] that ships with every major Tier 1 platform.

- **Native code generator for PowerPC 64-bit**  [\[PPC64-NCG](https://phabricator.haskell.org/D629)\].  Peter Trommler has been working on an extension of the PowerPC native code backend to support 64-bit Linux systems. There are two 64-bit ELF ABI versions. The implementation of ABI version 1, which is mostly used by big endian systems, is fairly stable and support for ABI version 2, which is used by systems with POWER8 processors running in little endian mode, is currently under testing. See #9863.

## Frontend, build-system, and miscellaneous changes

- TODO about Docbook.

- Shaking up GHC [\[Shake](building/shake)\].  Andrey Mokhov (with support from Neil Mitchell, Simon Marlow and Simon PJ) is working on a new `Shake`-based build system for GHC. The goal is to make it much more understandable, maintainable and convenient to use than the current `make`-based one. It is also expected that the new build system will be faster, because Shake allows to express build dependencies more accurately.

# References


- \[[ApiAnnotations](api-annotations)\] [https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations](https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations)
- \[[ApplicativeDo](applicative-do)\] [https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo)
- \[Backpack\] TODO FIXME
- \[[DistributedHaskell](distributed-haskell)\] [https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell](https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell)
- \[DWARF\] [https://ghc.haskell.org/trac/ghc/wiki/DWARF](https://ghc.haskell.org/trac/ghc/wiki/DWARF)
- [FCkinds](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf) System FC with explicit kind equality, Weirich, Hsu, and Eisenberg, ICFP 13. [ http://www.seas.upenn.edu/\~eir/papers/2013/fckinds/fckinds-extended.pdf](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf)
- \[[GhcApi](ghc-api)\] [https://ghc.haskell.org/trac/ghc/wiki/GhcApi](https://ghc.haskell.org/trac/ghc/wiki/GhcApi)
- [\[ImprovedLLVMBackend](improved-llvm-backend)\] [https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend](https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend)
- \[[InjectiveTypeFamilies](injective-type-families)\] [https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies)
- \[KindEqualities\] TODO FIXME
- [Records/OverloadedRecordFields](records/overloaded-record-fields) [https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields)
- \[[PartialTypeSignatures](partial-type-signatures)\] [https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures](https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures)
- [\[PPC64-NCG](https://phabricator.haskell.org/D629)\] [ https://phabricator.haskell.org/D629](https://phabricator.haskell.org/D629)
- [\[Prelude710](prelude710)\] [https://ghc.haskell.org/trac/ghc/wiki/Prelude710](https://ghc.haskell.org/trac/ghc/wiki/Prelude710)
- [\[Shake](building/shake)\] [https://ghc.haskell.org/trac/ghc/wiki/Building/Shake](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake)
- \[[StaticPointers](static-pointers)\] [https://ghc.haskell.org/trac/ghc/wiki/StaticPointers](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers)
- [\[TCPlugins](plugins/type-checker)\] [https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker](https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker)
- [\[TCSMT](https://github.com/yav/type-nat-solver)\] [ https://github.com/yav/type-nat-solver](https://github.com/yav/type-nat-solver)
- [\[TCSMT_paper](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)\] [ https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)
- [Typeable](typeable) [https://ghc.haskell.org/trac/ghc/wiki/Typeable](https://ghc.haskell.org/trac/ghc/wiki/Typeable)
