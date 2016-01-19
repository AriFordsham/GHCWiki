[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for retrofitting Haskell with an applicative, mix-in module system. The theory of Backpack is developed in the paper and its accompanying technical appendix. The purpose of this page is to track implementation progress.

## Motivation

### Large-scale modularity


Large-scale modularity refers to the modularization of software into libraries, which are built upon other libraries.  A package manager offers a limited degree of flexibility by permitting a library to built against varying \*versions\* of its dependencies.  Backpack seeks to solve the following problems related to programming with libraries:

1. **I want to write a library that works with ByteString, Text and String, but I only want to write it once.**  Today, I may have to maintain multiple versions of the package: `foo-bytestring`, `foo-text`, `foo-string`, each specialized against a specific string representation.  (TODO is this a good example, or do people want to write their library differently for these different types?) Similar situations occur when a library want to support multiple "backends". This problem is exacerbated when someone else writes another library which builds on top of `foo`; now they have to write three versions of the package. It is better if the library can be written once, and parametrized on a signature describing strings, allowing users to fill in their own string implementation.

  - Here are examples of libraries with "multiple drivers":

    - [ satchmo](http://hackage.haskell.org/package/satchmo) (satchmo-backends, satchmo-funsat, satchmo-toysat)
    - [ Chart](http://hackage.haskell.org/package/Chart) (Chart-cairo, Chart-diagrams , Chart-gtk).
    - [ FTGL](http://hackage.haskell.org/package/FTGL) (FTGL-bytestring)
    - [ HDBC](http://hackage.haskell.org/package/HDBC) (HDBC-mysql, HDBC-odbc, HDBC-postgresql, HDBC-session, HDBC-sqlite3)
    - [ MuCheck](http://hackage.haskell.org/package/MuCheck) (MuCheck-HUnit, MuCheck-Hspec, MuCheck-QuickCheck, MuCheck-SmallCheck)
    - [ Shellac](http://hackage.haskell.org/package/Shellac) (Shellac-compatline, Shellac-editline, Shellac-haskeline, Shellac-readline)
    - (I stopped after looking at all of the capitalized package names lol)
  - A close variant: I want to depend on some fancy full featured system, but I want people to be able to compile me against a simpler variant

    - [ Shake](https://hackage.haskell.org/package/shake) provides an interface for writing build systems. I may not want to be coupled to Shake specifically and be able to use whatever build system I want.
    - Test frameworks? (Though, it's hard to think of a case where you want to swap out components)
  - A common comment is, "Don't type classes work for this case?"  Common problems with using type classes:

    - Ambiguity: type class resolution must be done with respect to a type parameter, even when there is no natural one.  In some cases, a proxy type must be used when there is no natural type. This is exacerbated with multiparameter type classes, when some methods may not have enough types in their signature to uniquely identify an instance.
    - Newtype: type class resolution is done purely based on types; no way to parametrize over implementation. You must newtype in this situation.
    - Multiple parameters: without associated types, a multiparameter type class must be used when an interface requires multiple types. These types must be threaded through all functions which use this type signature; an annoying amount of plumbing.
    - Type classes infect call sites: if you have a data type from an type class associated type, and want to refer to it from another data type `T`, any function using T must also remember to add the type class constraint to their site.
    - Lack of specialization: type classes mostly must always be done in dictionary passing style (with inlining, sometimes the dictionary can be inlined, but don't count on it)
  - Why only at the package level? Bringing it down to the module level (and even finer) is the subject of small-scale modularity.

1. Is my library compatible against a given version of a dependency?  To determine this today, you must first install the library, and then build your code against it. With Backpack, you can write down precisely what interface you depend against, at which the compatibility check only involves testing if an implementation correctly implements the interface. Better yet, a library with explicit Backpack dependencies can be installed without installing any of its prerequisites. This information can be collected together in order to give accurate version dependencies. (TODO Interesting problem: Backpack says nothing about what should happen when someone generalizes a type signatures.  Conditional compilation suggests that there may need to be multiple interface sets that a package can compile against; variational programming but only with interfaces.) (TODO Right now, versions and instantiation are completely orthogonal, which sucks.)

1. Does anyone depend on this API?  If you want to make a backwards incompatible change to a library, it can be difficult to tell who will be affected. Explicit interfaces are *transmissible*; clients should be able to submit the slices of the interfaces they depend on to upstream, giving maintainers a view into what APIs are used.  This capability would be especially beneficial for packages with a large and organically grown API (e.g. the ghc package).  (TODO In what sense is an interface transmissible? Interface needs to be able to refer to other types which need to live somewhere. These are "subsidiary" in some sense; when checking for compatibility you don't care about these types.  Need to analyze this situation more carefully. See also [\#10798](https://gitlab.haskell.org//ghc/ghc/issues/10798).)

### Small-scale modularity


Today, functions and types in Haskell can be parametrized over types.  However, there is no direct way to parametrize over an implementation (type classes are an indirect method of parametrization based on type resolution), and parametrization over a type requires a type parameter to be explicitly plumbed through all use sites of the code.

## Implementation


Backpack consists of two major parts:

1. A way to write *signatures* (`hsig` files), which specify the interface of a module without providing an implementation. (Think of them as a generalization of `hs-boot` files.)


2.The frontend (implemented in a library) which implements *mix-in linking* to specify implementation of signatures.  Linking produces a build plan 


While it is hypothetically possible to use `hsig`s without the Backpack frontend, your user experience will be a lot more pleasant using the frontend.  We provide a few ways to use Backpack:

1. `ghc-backpack` is a minimal frontend that parses Backpack files, a compact syntax for Backpack use inspired by the Backpack paper, and then invokes GHC to compile them.  This frontend is primarily intended to be used for testing; it essentially is a convenient way to play around with Backpack files without having to write a full Cabal file / go through the Cabal tool.

1. `Cabal` knows how to translate Cabal files into the Backpack IR, and is able to compile any Backpack components for which it has source.  It is roughly analogous to `ghc-backpack`, except that it knows how to do proper, Cabalized builds.

1. `cabal-install`, via its dependency on the Cabal library, can translate Cabal files to the Backpack IR, and subsequently is able to compile Backpack components spread over multiple components.


The most up-to-date description for how `hsig` files are implemented can be found in `docs/backpack/algorithm.tex`, but here are some miscellaneous GHC-specific notes, including the full-stack description.

### The Backpack library frontend


The Backpack IR is a representation of the **pre-shape** of a Backpack component; the IR is sufficient to determine what instantiated components must be built, and how a component is instantiated.  (This is an abstraction of the package shape in the Backpack paper, which incorporates both module identities as well as the identities for entities defined in the modules.  Entity information is not needed for build planning, so the Backpack IR does not include it. You DON'T have to look at import declarations because signatures are per package, not per module.)


Here is the IR type:

```wiki
-- Basic data types. This is the NON-recursive UnitId formulation.
data ModuleName  = ModuleName  String
data ComponentId = ComponentId String
data Module = Module { moduleUnitId :: UnitId
                     , moduleName :: ModuleName }
data UnitId = UnitId { unitIdComponentId :: ComponentId
                     , unitIdInsts :: [(ModuleName, Module)] }
            | Hole

-- The intermediate representation of a component
data Component = Component {
  -- The UnitId of the component in question.  Invariant: every
  -- instantiation is H -> hole:H
  unitId :: UnitId,
  -- The direct dependencies of the component. They are the things
  -- that I need to build to build this.  These are a "renamed"
  -- version of includes.
  instantiatedDepends :: [UnitId],
  -- The modules from the includes which are "in scope".  Does not
  -- include requirements.
  -- TODO: Argue about whether or not it should be done this way.
  importedModules :: [(ModuleName, Module)],
  -- The exported modules of the component.  Local modules will
  -- have moduleUnitId == unitId, but there may also be reexports.
  -- Invariant: there are no HOLEs are in this list; you can determine
  -- what the holes of a package are by looking at unitId
  exposedModules :: [(ModuleName, Module)]
}
```


Invariant: every hole the non-unitId fields is *bound* by a hole in `unitId`. (An indefinite unit has the )

**Example.**


Consider some old-style Backpack files:

```wiki
package p where
  include q
  signature A
  module M
package q where
  signature B
  module Q
```


Let's consider `q` first.  Here's what the data structure gets filled as:

```wiki
Component {
  unitId = q(B -> hole:B),
  instantiatedDepends = [], -- no includes
  importedModules = [], -- nothing external in scope
  exposedModules = [(Q, q(B -> hole:B):Q)]
  -- Note this doesn't contain any of the holes. You can look
  -- at the unitId to get that information.
}
```


Now consider `p`:

```wiki
Component {
  unitId = p(A -> hole:A, B -> hole:B),
  instantiatedDepends = [q(B -> hole:B)],
  importedModules = [(Q, q(B -> hole:B):Q)]
}
```


Note that if instead of `include q`, we had `include q requires B as B2`, then the `instantiatedDepends` is `[q(B -> hole:B2)]`. Note that this is *bound* by the `unitId` field, which is now `p(A -> hole:A, B2 -> hole:B2)`.


On `importedModules`: it only shows things that were exposed by the included packages. We need to compute this when we do mix-in linking. (It could include `A`, but maybe not.) Why doesn't it have requirements?  Because you always know where the requirements are: for every requirement of a unit, we have to compile an `hsig` file for it, even if there is no local one: you have to make a blank one.

TODO SPJ, maybe we can pair the module names with where the include comes from. At the moment, there's no way to say, "Where it came form." (EZY: Mix-in linking, you want to glom them together.)

TODO Scott: I'd like to just be able to substitute, and have the form of `Component` stay the same.

TODO exposedModules is a bad name, because it isn't the full set of what things are brought into scope.

TODO Why not stuff the exposes into the `UnitId`? That lets us get rid of `importedModules` and `exposedModules`. Moving the data structure around. (EZY: Equality doesn't have the `exposedModules`). Is `exposedModules` a function of `unitIdComponentId` and `unitIdInsts`? No, you need the source code. It's not a function you want to compute lightly. (`Component` is derived off of `UnitId`.)

**How to compile?**


Intuitively, the algorithm for compiling a `UnitId` goes as follows:

1. Lookup the `Component` corresponding to the `ComponentId` of the `UnitId`.
1. Substitute according to `unitIdInsts` in all fields of `Component`. E.g., if you are instantiating `A -> base:A`, replace every occurrence of `hole:A` with `base:A`.
1. Recursively compile every `instantiatedDepends`.
1. Compile this `Component` with the correct flags derived from this data structure.

### The GHC interface


How do you compile a `Component`; that is to say, what flags are passed to GHC to compile an instantiated unit?  There are a few things that need to be passed:

1. The chosen STRING unit ID (which is to be used for linker symbols).  Done in old versions of GHC with `-package-name` (or more recently `-this-package-key` and `-this-unit-id`.
1. The full unit ID data structure.  My suggestion is that this is given in two parts: `-this-component-id` (a new flag) and `-sig-of` (shipped already in GHC 7.10)
1. The set of modules that are in scope, from `importModules`.  TODO There are two ways to do this: a series of `-package-id "p (A as B)"` flags (which mimic a source-level include declaration, or manually specifying each of the modules in scope (some new flag).  Besides possibly being quite long, the latter is attractive because the elaboration to the Backpack IR needs to compute the set of modules in scope (so it knows how to instantiate things.) The former is attractive because it works without modification on old versions of GHC.
1. The set of requirements that are in scope, from `instantiatedDepends`.  These indicate what other signatures need to be merged into the local `hsig`s.

### Cabal syntax

TODO More context


I think for now we should resurrect the original Cabal syntax.  So, we add the following fields:

```wiki
exposed-signatures: H1 H2
reexported-modules: X as H3 -- NB: Cabal must know what modules are in scope!
```


Primary complication is permitting a build-depends to be included twice, with different dependencies.  We could put this inline into build-depends but I think it's more wise to just add a new field `includes:` which accepts a newline separated list of includes with renamings:

```wiki
build-depends: foo >= 2.0
includes:
  foo requires (H as H1)
  foo requires (H as H2)
```


If a package `bar` is not explicitly mentioned in `includes`, it is assumed to have been included as `include bar`.

### Cabal changes

- No longer a one-to-one mapping between Cabal file components, and actually components to build.  Cabal must call into the Backpack library to figure out what all the local, instantiated components to build should be (and what the flags should be.)
- Must adjust register so that it can register multiple components (one for each internal one that was enabled for build)
- build command needs to be componentized, so that a client can request a SPECIFIC component be built

TODO This overlaps with Duncan's proposal, where Cabal "configures" everything fromt he getgo.

### cabal-install changes

- After version resolution, look at Cabal files, and use Backpack library to expand into a build-plan that also includes instantiated componetns. Then go and build them. (Benefit of having cabal-install call the Backpack library: no need for Setup executable to communicate to Cabal how it should go.)

## GHC specific notes

### Holes


The big new typechecking feature of Backpack is to allow a user to replace a concrete module (`hs`) with a signature (`hsig`), whose implementation can be filled in later.  Both `hs` and `hsig` files compile to an `hi` file, but interface files generated by `hsig` files are different in the following ways:

- `mi_sig_of` is non-empty, recording the **implementing module** of the interface in question.  For example, when you run `ghc -c A.hsig -sig-of other-pkg:A`, the produced `ModIface` has `main:A` for the `mi_module`, but `other-pkg:A` for `sem_mod`. (Normal interfaces have `Nothing` in `mi_sig_of`.)  When the implementing module is unknown, the recorded module is a fake module abscribed to the fictitious `hole` package; e.g. `hole:A`.

- `mi_hsc_src` is `HsigFile`.

- Like `hi-boot` files compiled from `hs-boot`, these `hi` files contain no unfoldings, can have abstract data types, etc.


Internally, we often refer to what is recorded in `mi_sig_of` as the "semantic module"; this is the module that is used for `Name`s that come to the module; `mi_module` is the "identity module" which uniquely identifies an interface file.

### Signature merging


Unlike regular modules, you cannot simply \*hide\* a hole: it is a requirement that always must be fulfilled before you can compile the module in question. Instead, if you define an `A.hsig` for an `A` which is already required by another unit we included, the requirements of that unit are \*merged\* with the requirements of this unit.


Presently, this merging process is carried out by `mergeRequirements`, which successively adds inherited required types/values from included units to the type/environment of the `hsig` file being compiled.  Any requirements for which we don't have a local `hsig` implicitly have a "blank" signature file which collects together the merged requirements.

TODO Unclear how to handle dependency cycles here.

### Lazy interface loading


In paper Backpack, unification and substitution is performed eagerly on the type environment as soon as we discover that some type equality holds (e.g. during shaping).  In GHC, the type environment is \*lazily\* loaded, and thus this substitution must be deferred until we actually load this interface.  This has two consequences for GHC's interface loading code:

1. We may want the types for a module `p(A -> HOLE:B):M`, but we only have an interface file for `p(A -> HOLE:A):M`. In this case, we have to rename the interface file from `A -> HOLE:A` to `A -> HOLE:B` before we typecheck and load it in.  This is handled by `computeInterface`, which calls `rnModIface` to apply the substitution.

1. Inside an interface file, we may refer to a Name `hole:A.T`.  During shaping, we may have discovered that this Name actually is unified with `impl:A.T`; so when we are typechecking the interface file, we must use the real name and not stodgily adhere to the old one.  The `eps_shape` data structure records these unifications, and a few data types (interface file renaming, interface type-checking, and even type-checking a signature) abide by this substitution.

### The database


The package database contains both entries for old-fashioned definite units/packages, and also entries for indefinite unit, which contain code but no objects.  Conventionally, the unit ID for an indefinite is simply the unit ID with all of the requirements filled in with `hole`s.

### Unit IDs


A unit ID is a recursive data structure which is defined to be a component ID (specified by Cabal) plus a mapping from module names to modules, where a module is a a unit ID plus a module name. In common parlance, components and units are the same, but we've adopted the convention that a "unit ID" also includes an instantiation, while a component does not.  The component ID represents "source code"; we call it a component and not a unit because it coincides with the preexisting Cabal notion of a component.


In some situations, we need serialize a unit ID into a compact, deterministic string, for use in linker symbols and file paths, as a full unit ID could be quite long (it is AT LEAST linear in the number of holes in a unit).  Ideally, the serialization format would be private to GHC (similarly to how z-encoding is GHC private), but the encoding leaks at least to the file path that GHC stores compilation results at... would be nice if there a way to avoid this problem.

## Reading

- [ The Backpack documentation directory](https://git.haskell.org/ghc.git/blob/HEAD:/docs/backpack/). You'll have to build them (just `make`), but here are the files of interest:

  - algorithm.tex specifies the abstract core Backpack algorithms for GHC.
  - backpack-manual.tex is a user-facing manual for using Backpack.
  - backpack-impl.tex is an old but interesting technical document describing many of the technical tradeoffs in our design.
- [ What's a module system good for anyway?](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/) Motivates *why* you might care about Backpack
- The [commentary pages about packages](commentary/compiler/packages), 


see also [CabalDependency](cabal-dependency)

## Discarded approaches

**Explicit signature visibility.** A requirement is not something you can hide: you must fulfill it at some point. But separately, you might also imagine controlling whether or not an identifier from a signature is importable or not.  However, this complicates the semantics of shaping (you have to keep track, for every identifier, whether or not it is visible or not, and there are a few edge cases which don't have a neat resolution), so it was abandoned. (Thanks Derek!)

**How are interfaces for signatures handled.** We've gone through three iterations of how signatures were implemented:

1. A signature `.hsig` compiled to an `.hi` file containing only the definitions from that `.hsig` file.  When module name `A` which is a requirement is imported, \*all\* of the signatures are imported as if there were multiple imports for each of them.  In this model, it's easy to hide signatures selectively (just don't import them), but the model for what happens when you import a module name is more complicated (it's a list of `Module` rather than a specific `Module`.)
1. A signature `.hs-boot` compiled to an `.hi-boot` file, which was subsequently merged (`ghc -merge-requirements`) into an `.hi` file that was to be imported. (This was beneficial because the merging didn't rely on being able to import entities, so it was as if it was done all at once. But this is kind of a hack.)
1. We got rid of the merge requirements step, so an `.hsig` file is compiled into an `.hi` file, and \*immediately\* merges all of the requirements in scope.  For signatures which are not in scope, the build system is responsible for creating a "fake" signature file so the correct requirement can be brought into scope. (The downside is that you really need proper recursive support to handle many cases.)

**Dealing with duplicate signatures.** One persistent complaint with signatures is that you have to repeat a type twice: once when you write the signature and then again when you actually implement it. You'd like some mechanism to say, "this type is from the signature."  Derek quote: "I have thought about this problem before, and didn't figure it out, and got tired of it." We don't have any solution for this. Similarly, if you want to define a non-abstract data type, it's tiresome to repeat it in the signature and the implementing module.

**Source-level signature inference.** At some point, I attempted to make a tool that took a library and inferred the signatures of the libraries it depended on, using the "usages" capacity. I found that it was quite difficult to correctly specify what the \*types\* are supposed to be, because signatures could refer to types which were never explicitly used! This is an instance of what's called the "avoidance" problem. The plan (not implemented) is to instead never syntactically write signatures down, and just infer them directly.

**Straight-line shaping.** In the original Backpack paper, declarations inside a Backpack package were processed line-by-line. This means that it would be an error to import a module before it was declared. At some point, we generalized things so that we computed an import graph, and then process in order. The pro is that order doesn't matter, and we only need to do a complicated shaping pass for cycles. The downside is that the interaction between includes and modules becomes more complicated.

**Fat interface files.** See [\#10871](https://gitlab.haskell.org//ghc/ghc/issues/10871)

**Not packages, components.** A package implies a unit of distribution, but that does not necessarily coincide with the unit of modularity (having to make a Cabal file for each Backpack unit would be terrible!)

**Uniform dependency across all Backpack units.** Not so much a removed feature as a removed restriction: brought about because we had components and units and it didn't make much sense to keep them distinct.

**No code for partially instantiated packages.** You only get interface files for the completely generalized unit, and a completely specialized unit (i.e., with code). It was not obvious to SPJ at the beginning that we could lazily create partially instantiated interface files, but at this point it's well understood.

**An infinite hierarchy of hs-boot files.** The idea here is described in [ https://wiki.haskell.org/HaskellImplementorsWorkshop/2015\#.22Look_Ma.2C_No_Signatures.21.22_Separate_modular_development_without_interfaces](https://wiki.haskell.org/HaskellImplementorsWorkshop/2015#.22Look_Ma.2C_No_Signatures.21.22_Separate_modular_development_without_interfaces)
SPJ pointed out that you should dispense with the infinite hierarchy and just compile all of the hs-boot files in one go, that solves cycles among hs-boot files. (Or even compile hi-boot from all the hs files in one go.)

**Backpack smarts directly in GHC.** (Obviously) it makes more sense to put it in a library, which both GHC and Cabal can use.  There are a lot of technical difficulties of making this actually work well, but it "makes the most sense."

## Backpack-related tickets


Backpack-related tickets are marked with keyword 'backpack'. If the ticket is assigned to ezyang, it means he's planning on working on it.

<table><tr><th>Ticket (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
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
<th></th></tr></table>