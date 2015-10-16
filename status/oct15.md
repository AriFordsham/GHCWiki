# GHC Status Report, October 2015


GHC development spurs on, with an exciting new announcement - the next release will be a super-major one, culminating in **GHC 8.0**... Lorem ipsum dolor sit amet, ...

## Major changes in GHC 8.0.1

### Already in HEAD

- Support for [implicit parameters providing callstacks/source locations](explicit-call-stack/implicit-locations), allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([ Phab:D861](https://phabricator.haskell.org/D861))

- Improved optimization diagnostics. The compiler is now more liberal about issues warnings of potentially non-firing rewrite rules

- Support for wildcards in data and type family instances ([ Phab:D1092](https://phabricator.haskell.org/D1092))

- Support for [Injective Type Families](injective-type-families), which allows you to specify type families which are injective, i.e. a one-to-one relationship. ([ Phab:D202](https://phabricator.haskell.org/D202))

- Support for [Applicative Do](applicative-do), allowing GHC to desugar do-notation to `Applicative` where possible. ([ Phab:D729](https://phabricator.haskell.org/D729))

- Support for deriving the `Lift` typeclass ([ Phab:D1168](https://phabricator.haskell.org/D1168))

- Support for PowerPC 64-bit native code generation on Linux in big endian and little endian mode. ([ Phab:D629](https://phabricator.haskell.org/D629))

- A beautiful new users guide, written in reStructured Text, and significantly improved output.

### Incoming work, likely in time for 8.0

- A new, type-indexed type representation, `data TTypeRep (a :: k)`. See [TypeableT](typeable-t).

- Visible type application ([ Phab:D1138](https://phabricator.haskell.org/D1138))

- Support for reasoning about kind equalities, which gives promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. There is some discussion in [DependentHaskell/Phase1](dependent-haskell/phase1), but that's very low-level. I (Richard) have no good user-oriented write-up yet, but there shouldn't be much in the way of new syntax -- just fewer type errors. ([ Phab:D808](https://phabricator.haskell.org/D808))

- Support for [Strict Haskell](strict-pragma) including both the `StrictData` and `Strict` language extensions

- Support for record pattern synonyms ([ Phab:D1152](https://phabricator.haskell.org/D1152))

- Implement the `MonadFail` proposal ([\#10751](https://gitlab.haskell.org//ghc/ghc/issues/10751))

- Support for [Overloaded Record Fields](overloaded-record-fields), allowing multiple uses of the same field name and a form of type-directed name resolution.

- A huge improvement to pattern matching (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [ their paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf).

- Backpack is chugging along; we have a new user-facing syntax which allows multiple modules to be defined a single file, and are hoping to release at least the ability to publish multiple "units" in a single Cabal file.

- Improved [DWARF based debugging support](dwarf) from Peter Wortmann, Arash Rouhani, and Ben Gamari with backtraces from Haskell code.

- An [Improved LLVM Backend](improved-llvm-backend) that ships with every major Tier 1 platform.

## After 8.0

- Support for **Type Signature Sections**, allowing you to write `(:: ty)` as a shorthand for `(\x -> x :: ty)`.
- A (possible) overhaul of GHC's build system to use **Shake** instead of Make.
- A `DEPRECATED` pragma for exports ([\#4879](https://gitlab.haskell.org//ghc/ghc/issues/4879))

# References

- \[[ApiAnnotations](api-annotations)\] [ https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations](https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations)
- \[[ApplicativeDo](applicative-do)\] [ https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo)
- \[Backpack\] TODOFIXME
- \[[DistributedHaskell](distributed-haskell)\] [ https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell](https://ghc.haskell.org/trac/ghc/wiki/DistributedHaskell)
- \[DWARF\] [ https://ghc.haskell.org/trac/ghc/wiki/DWARF](https://ghc.haskell.org/trac/ghc/wiki/DWARF)
- [ FCkinds](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf) System FC with explicit kind equality, Weirich, Hsu, and Eisenberg, ICFP 13. [ http://www.seas.upenn.edu/\~eir/papers/2013/fckinds/fckinds-extended.pdf](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf)
- \[[GhcApi](ghc-api)\] [ https://ghc.haskell.org/trac/ghc/wiki/GhcApi](https://ghc.haskell.org/trac/ghc/wiki/GhcApi)
- [\[ImprovedLLVMBackend](improved-llvm-backend)\] [ https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend](https://ghc.haskell.org/trac/ghc/wiki/ImprovedLLVMBackend)
- \[[InjectiveTypeFamilies](injective-type-families)\] [ https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies)
- \[KindEqualities\] TODOFIXME
- [Records/OverloadedRecordFields](records/overloaded-record-fields)[ https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields)
- \[[PartialTypeSignatures](partial-type-signatures)\] [ https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures](https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures)
- [ \[PPC64-NCG](https://phabricator.haskell.org/D629)\] [ https://phabricator.haskell.org/D629](https://phabricator.haskell.org/D629)
- [\[Prelude710](prelude710)\] [ https://ghc.haskell.org/trac/ghc/wiki/prelude710](https://ghc.haskell.org/trac/ghc/wiki/prelude710)
- [\[Shake](building/shake)\] [ https://ghc.haskell.org/trac/ghc/wiki/Building/Shake](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake)
- \[[StaticPointers](static-pointers)\] [ https://ghc.haskell.org/trac/ghc/wiki/StaticPointers](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers)
- [\[TCPlugins](plugins/type-checker)\] [ https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker](https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker)
- [ \[TCSMT](https://github.com/yav/type-nat-solver)\] [ https://github.com/yav/type-nat-solver](https://github.com/yav/type-nat-solver)
- [ \[TCSMT_paper](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)\] [ https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf](https://github.com/yav/type-nat-solver/raw/master/docs/paper.pdf)
- [Typeable](typeable)[ https://ghc.haskell.org/trac/ghc/wiki/Typeable](https://ghc.haskell.org/trac/ghc/wiki/Typeable)