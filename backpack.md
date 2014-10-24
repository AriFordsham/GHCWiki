[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for retrofitting Haskell with an applicative, mix-in module system. The theory of Backpack is developed in the paper and its accompanying technical appendix; we also have an [ in-depth implementation design document](http://web.mit.edu/~ezyang/Public/backpack-impl.pdf) (source: [docs/backpack](/trac/ghc/browser/ghc/docs/backpack)). The purpose of this page is to track implementation progress. (Wondering what happened to the old text? Check the history; it will be integrated into the implementation design doc eventually).


See also [CabalDependency](cabal-dependency)

## Reading

- [ Design document](https://git.haskell.org/ghc.git/blob/HEAD:/docs/backpack/backpack-impl.pdf): quite technical, but a lot of good information about how the system we are implementing differs from the Backpack paper
- [ Manual](https://git.haskell.org/ghc.git/blob/HEAD:/docs/backpack/backpack-manual.pdf): user facing specification of the system
- [ What's a module system good for anyway?](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/) Motivates \*why\* you might care about Backpack
- [ A taste of Cabalized Backpack](http://blog.ezyang.com/2014/08/a-taste-of-cabalized-backpack/): Tutorial style introduction to the Cabal-style syntax for writing Backpack modules
- The [commentary pages about packages](commentary/compiler/packages)
- Planned: blog post about thinning/renaming

## Implementing "A taste of Cabalized Backpack"


Many of the features described in [ A taste of Cabalized Backpack](http://blog.ezyang.com/2014/08/a-taste-of-cabalized-backpack/) are not implemented yet. Here's the blow-by-blow:

- GHC signature support is in progress at [ D130](https://phabricator.haskell.org/D130)

  - exposed-signatures in GhcPackage behaves the same as required-signatures (GHC needs to consider these modules exposed for the purpose of module lookup)
  - We don't support multiple backing implementations for a signature, as SPJ mentions in Phab. The good thing about this is if we implement this we will automatically get signature merging. One difficulty: making sure all of the definitions of the signature have been satisfied! Also need to adjust package key format.
- Cabal signature support is in progress at [ ezyang/cabal:ezyang-dev](https://github.com/ezyang/cabal/tree/ezyang-dev)

  - We don't support the "semicolon" syntax, which allows you to include a package multiple times (usually used to instantiate it differently.) Monoid plus should be replaced with semicolon operation.
  - If anyone wants it: hiding and bulk rename for thinning/renaming
  - Figure out if you can rename types, and if not, what to do
  - implements field is not implemented
  - reexported-modules may not work with signatures (untested)
  - Support type families in signatures
  - Carefully test our hole map algorithm to make sure we're actually canonicalizing things enough (we probably aren't...)
- Typechecking against signatures (the next frontier)
- Installing indefinite packages (reusing the installed package database as a place to stash source packages...)

## Backpack-related tickets


Backpack-related tickets are marked with keyword 'backpack'. If the ticket is assigned to ezyang, it means he's planning on working on it.

## Status: closed (41 matches)

<table><tr><th>Ticket (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#8407](https://gitlab.haskell.org//ghc/ghc/issues/8407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Module re-exports at the package level](https://gitlab.haskell.org//ghc/ghc/issues/8407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9243](https://gitlab.haskell.org//ghc/ghc/issues/9243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Recompilation avoidance doesn't work for -fno-code/-fwrite-interface](https://gitlab.haskell.org//ghc/ghc/issues/9243)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9265](https://gitlab.haskell.org//ghc/ghc/issues/9265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Create PackageKey to replace PackageId, including version dependency information](https://gitlab.haskell.org//ghc/ghc/issues/9265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12945](https://gitlab.haskell.org//ghc/ghc/issues/12945)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack signature matching doesn't pick up orphan instances](https://gitlab.haskell.org//ghc/ghc/issues/12945)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9245](https://gitlab.haskell.org//ghc/ghc/issues/9245)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[In absence of recursive imports, hs-boot files not checked for consistency](https://gitlab.haskell.org//ghc/ghc/issues/9245)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9252](https://gitlab.haskell.org//ghc/ghc/issues/9252)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Generalize hs-boot files to be more like module signatures](https://gitlab.haskell.org//ghc/ghc/issues/9252)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9256](https://gitlab.haskell.org//ghc/ghc/issues/9256)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Support automatic derivation of an hs-boot file from an hs file](https://gitlab.haskell.org//ghc/ghc/issues/9256)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9375](https://gitlab.haskell.org//ghc/ghc/issues/9375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Support for module thinning/renaming on command line](https://gitlab.haskell.org//ghc/ghc/issues/9375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9506](https://gitlab.haskell.org//ghc/ghc/issues/9506)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Name libraries (dll/so) separately from linker symbols](https://gitlab.haskell.org//ghc/ghc/issues/9506)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9507](https://gitlab.haskell.org//ghc/ghc/issues/9507)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[ghc-pkg mode to query by package-key](https://gitlab.haskell.org//ghc/ghc/issues/9507)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#9508](https://gitlab.haskell.org//ghc/ghc/issues/9508)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Rename package key](https://gitlab.haskell.org//ghc/ghc/issues/9508)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10252](https://gitlab.haskell.org//ghc/ghc/issues/10252)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Abstract newtype in hs-boot](https://gitlab.haskell.org//ghc/ghc/issues/10252)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10262](https://gitlab.haskell.org//ghc/ghc/issues/10262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Improve documentation of module signatures](https://gitlab.haskell.org//ghc/ghc/issues/10262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10622](https://gitlab.haskell.org//ghc/ghc/issues/10622)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Rename Backpack packages to units](https://gitlab.haskell.org//ghc/ghc/issues/10622)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10660](https://gitlab.haskell.org//ghc/ghc/issues/10660)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[.dyn_o isn't generated for .hsig files with -dynamic-too](https://gitlab.haskell.org//ghc/ghc/issues/10660)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>spinda</th></tr>
<tr><th>[\#10690](https://gitlab.haskell.org//ghc/ghc/issues/10690)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Save merged signatures to disk](https://gitlab.haskell.org//ghc/ghc/issues/10690)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10714](https://gitlab.haskell.org//ghc/ghc/issues/10714)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[After implementing new installed package ID (hash of sdist), get rid of package keys](https://gitlab.haskell.org//ghc/ghc/issues/10714)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10725](https://gitlab.haskell.org//ghc/ghc/issues/10725)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Figure out how to support type synonym implementions of abstract data](https://gitlab.haskell.org//ghc/ghc/issues/10725)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10798](https://gitlab.haskell.org//ghc/ghc/issues/10798)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Signatures with only types should not be included in unit keys](https://gitlab.haskell.org//ghc/ghc/issues/10798)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10838](https://gitlab.haskell.org//ghc/ghc/issues/10838)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hsig files don't have good enough error checking for wired-in types](https://gitlab.haskell.org//ghc/ghc/issues/10838)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12699](https://gitlab.haskell.org//ghc/ghc/issues/12699)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Suspicious treatment of renaming of field labels](https://gitlab.haskell.org//ghc/ghc/issues/12699)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12955](https://gitlab.haskell.org//ghc/ghc/issues/12955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Confusing error (module is not loaded) when more hsigs provided than -instantiated-with](https://gitlab.haskell.org//ghc/ghc/issues/12955)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12994](https://gitlab.haskell.org//ghc/ghc/issues/12994)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Module and export level signature thinning in Backpack](https://gitlab.haskell.org//ghc/ghc/issues/12994)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13041](https://gitlab.haskell.org//ghc/ghc/issues/13041)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type classes in Backpack signatures are dodgy](https://gitlab.haskell.org//ghc/ghc/issues/13041)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13066](https://gitlab.haskell.org//ghc/ghc/issues/13066)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack doesn't check for fixity consistency](https://gitlab.haskell.org//ghc/ghc/issues/13066)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13067](https://gitlab.haskell.org//ghc/ghc/issues/13067)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Abstract closed type families don't work with Backpack](https://gitlab.haskell.org//ghc/ghc/issues/13067)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13214](https://gitlab.haskell.org//ghc/ghc/issues/13214)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Orphan instances in Backpack signatures don't work](https://gitlab.haskell.org//ghc/ghc/issues/13214)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13250](https://gitlab.haskell.org//ghc/ghc/issues/13250)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack: matching newtype selectors doesn't work](https://gitlab.haskell.org//ghc/ghc/issues/13250)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13268](https://gitlab.haskell.org//ghc/ghc/issues/13268)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack doesn't work with Template Haskell, even when it should](https://gitlab.haskell.org//ghc/ghc/issues/13268)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13323](https://gitlab.haskell.org//ghc/ghc/issues/13323)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack doesn't work with DuplicateRecordFields](https://gitlab.haskell.org//ghc/ghc/issues/13323)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13335](https://gitlab.haskell.org//ghc/ghc/issues/13335)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Non-abstract types also have skolem nature](https://gitlab.haskell.org//ghc/ghc/issues/13335)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14304](https://gitlab.haskell.org//ghc/ghc/issues/14304)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Instantiated libraries (Backpack) don't get linked with enough deps](https://gitlab.haskell.org//ghc/ghc/issues/14304)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14525](https://gitlab.haskell.org//ghc/ghc/issues/14525)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack doesn't work with CPP](https://gitlab.haskell.org//ghc/ghc/issues/14525)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14674](https://gitlab.haskell.org//ghc/ghc/issues/14674)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Deferring more levity polymorphism checks in indefinite backpack modules](https://gitlab.haskell.org//ghc/ghc/issues/14674)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15041](https://gitlab.haskell.org//ghc/ghc/issues/15041)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[rnIfaceBndr implementation in RnModIface looks incorrect](https://gitlab.haskell.org//ghc/ghc/issues/15041)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15138](https://gitlab.haskell.org//ghc/ghc/issues/15138)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Unable to instantiate data members of kind Nat in backpack signatures.](https://gitlab.haskell.org//ghc/ghc/issues/15138)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15379](https://gitlab.haskell.org//ghc/ghc/issues/15379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Don't reject user-written instances of KnownNat and friends in hsig files](https://gitlab.haskell.org//ghc/ghc/issues/15379)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12679](https://gitlab.haskell.org//ghc/ghc/issues/12679)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Permit abstract data types in signatures that don't have kind \*](https://gitlab.haskell.org//ghc/ghc/issues/12679)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13068](https://gitlab.haskell.org//ghc/ghc/issues/13068)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC should not allow modules to define instances of abstract type classes](https://gitlab.haskell.org//ghc/ghc/issues/13068)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#10723](https://gitlab.haskell.org//ghc/ghc/issues/10723)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Make declarations in signatures "weakly bound" until they are used](https://gitlab.haskell.org//ghc/ghc/issues/10723)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12701](https://gitlab.haskell.org//ghc/ghc/issues/12701)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Eta reduce type synonyms when possible](https://gitlab.haskell.org//ghc/ghc/issues/12701)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>## Status: new (23 matches)

</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#1409](https://gitlab.haskell.org//ghc/ghc/issues/1409)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Allow recursively dependent modules transparently (without .hs-boot or anything)](https://gitlab.haskell.org//ghc/ghc/issues/1409)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9351](https://gitlab.haskell.org//ghc/ghc/issues/9351)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[add ability to version symbols .c for packages with C code](https://gitlab.haskell.org//ghc/ghc/issues/9351)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10749](https://gitlab.haskell.org//ghc/ghc/issues/10749)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Boot file instances should imply superclasses](https://gitlab.haskell.org//ghc/ghc/issues/10749)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10827](https://gitlab.haskell.org//ghc/ghc/issues/10827)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[GHCi should support interpeting multiple packages/units with separate DynFlags](https://gitlab.haskell.org//ghc/ghc/issues/10827)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12703](https://gitlab.haskell.org//ghc/ghc/issues/12703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Expand Backpack's signature matching relation beyond definitional equality](https://gitlab.haskell.org//ghc/ghc/issues/12703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13151](https://gitlab.haskell.org//ghc/ghc/issues/13151)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make all never-exported IfaceDecls implicit](https://gitlab.haskell.org//ghc/ghc/issues/13151)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#13266](https://gitlab.haskell.org//ghc/ghc/issues/13266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Source locations from signature merging/matching are bad](https://gitlab.haskell.org//ghc/ghc/issues/13266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13469](https://gitlab.haskell.org//ghc/ghc/issues/13469)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[-fdefer-type-errors for Backpack](https://gitlab.haskell.org//ghc/ghc/issues/13469)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14212](https://gitlab.haskell.org//ghc/ghc/issues/14212)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Give better error message with non-supported Backpack/TH use](https://gitlab.haskell.org//ghc/ghc/issues/14212)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14478](https://gitlab.haskell.org//ghc/ghc/issues/14478)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Abstract pattern synonyms (for hsig and hs-boot)](https://gitlab.haskell.org//ghc/ghc/issues/14478)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15391](https://gitlab.haskell.org//ghc/ghc/issues/15391)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Maybe ghc-pkg register should unregister packages with "incompatible" signatures](https://gitlab.haskell.org//ghc/ghc/issues/15391)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15984](https://gitlab.haskell.org//ghc/ghc/issues/15984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack accepts ill-kinded instantiations. Can cause GHC panic](https://gitlab.haskell.org//ghc/ghc/issues/15984)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16412](https://gitlab.haskell.org//ghc/ghc/issues/16412)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Type family signatures in indefinite modules](https://gitlab.haskell.org//ghc/ghc/issues/16412)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10266](https://gitlab.haskell.org//ghc/ghc/issues/10266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Split base for Backpack](https://gitlab.haskell.org//ghc/ghc/issues/10266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#10681](https://gitlab.haskell.org//ghc/ghc/issues/10681)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Teach GHC to interpret all hs files as two levels of hs-boot files (abstract types only/full types + values)](https://gitlab.haskell.org//ghc/ghc/issues/10681)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12680](https://gitlab.haskell.org//ghc/ghc/issues/12680)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Permit type equality instances in signatures](https://gitlab.haskell.org//ghc/ghc/issues/12680)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13149](https://gitlab.haskell.org//ghc/ghc/issues/13149)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Giving Backpack a Promotion](https://gitlab.haskell.org//ghc/ghc/issues/13149)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13262](https://gitlab.haskell.org//ghc/ghc/issues/13262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Allow type synonym family application in instance head if it has no free variables](https://gitlab.haskell.org//ghc/ghc/issues/13262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13361](https://gitlab.haskell.org//ghc/ghc/issues/13361)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Better type synonym merging/subtyping for Backpack](https://gitlab.haskell.org//ghc/ghc/issues/13361)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13765](https://gitlab.haskell.org//ghc/ghc/issues/13765)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC cannot parse valid Haskell98 whose first identifier is named signature](https://gitlab.haskell.org//ghc/ghc/issues/13765)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#14210](https://gitlab.haskell.org//ghc/ghc/issues/14210)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[bkp files cannot find TemplateHaskell symbols (even without Backpack features)](https://gitlab.haskell.org//ghc/ghc/issues/14210)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#10871](https://gitlab.haskell.org//ghc/ghc/issues/10871)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Implement "fat" interface files which can be directly compiled without source](https://gitlab.haskell.org//ghc/ghc/issues/10871)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#12717](https://gitlab.haskell.org//ghc/ghc/issues/12717)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Permit data types in signatures to be implemented with equivalent pattern synonyms (and vice versa)](https://gitlab.haskell.org//ghc/ghc/issues/12717)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>## Status: patch (2 matches)

</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: keywords: backpack, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#15594](https://gitlab.haskell.org//ghc/ghc/issues/15594)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[--abi-hash with Backpack incorrectly loads modules from dependent packages](https://gitlab.haskell.org//ghc/ghc/issues/15594)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16219](https://gitlab.haskell.org//ghc/ghc/issues/16219)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Backpack - TH+indefinite module interface file error](https://gitlab.haskell.org//ghc/ghc/issues/16219)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>