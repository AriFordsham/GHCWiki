[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for retrofitting Haskell with an applicative, mix-in module system. The theory of Backpack is developed in the paper and its accompanying technical appendix. The purpose of this page is to track implementation progress. (Wondering what happened to the old text? Check the history; it will be integrated into the implementation design doc eventually).

## Reading

- [ The Backpack documentation directory](https://git.haskell.org/ghc.git/blob/HEAD:/docs/backpack/). You'll have to build them (just `make`), but here are the files of interest:

  - algorithm.tex specifies the abstract core Backpack algorithms for GHC.
  - backpack-manual.tex is a user-facing manual for using Backpack.
  - backpack-impl.tex is an old but interesting technical document describing many of the technical tradeoffs in our design.
- [ What's a module system good for anyway?](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/) Motivates *why* you might care about Backpack
- The [commentary pages about packages](commentary/compiler/packages), 


see also [CabalDependency](cabal-dependency)

## Design philosophy

**Backpack files are a better way of writing command line flags.**

**Separation of concerns between GHC and Cabal.**

## Implementation notes


Backpack is a largely orthogonal extension to GHC; for example, there are no changes to the core type-checking or type-inference algorithm. Rather, the implementation of Backpack consists of:

1. A new language for describing "components" (called packages in the original Backpack paper). This language is written in `bkp` files, and looks like this:

```wiki
component p where
  include q (Q) requires (H)
  module A
  module B
```

1. A new frontend, `ghc --backpack` for processing `bkp` files, which can make (multiple) calls to `ghc --make` in order to typecheck and compile modules in a `bkp` file.

1. Infrastructure for handling *indefinite components*, i.e. components for which we have signatures but not implementations for some modules.  This includes machinery to do *shaping*.


The most up-to-date description for the frontend and shaping can be found in `docs/backpack/algorithm.tex`, but here are some miscellaneous GHC-specific notes.

### The frontend


A Backpack file can be thought of in two ways:

1. It is a "user-friendly" way of passing flags which would otherwise have to be manually specified to `ghc --make`. In particular, Cabal could generate a Backpack file for GHC to compile. This is the "large-scale modularity" perspective.

1. It contains source code written in the "Backpack module language" (analogous to ML's module language), and is the mechanism by which users write and compose modular programs. This is the "small-scale modularity" perspective.


Our goal is to eventually support \*both\* perspectives, but Backpack most natively supports large-scale modularity. We haven't yet started working on a design for small-scale modularity (which would likely involve additions to Haskell's surface syntax), except for a few small affordances (multiple units, inline modules) in Backpack which should make small-scale modular development possible (even if it is still a little unpleasant.)

**Typechecking an indefinite component.** A user wishing to compile a component with component ID `p-0.1-xxx` which depends on `base` and contains a signature `A` and a module `B` can setup their working directory with the files `p-0.1-xxx.bkp`, `A.hsig`, and `B.hs`. The contents of the Backpack file specify the modules, signatures, and includes of the component:

```wiki
component p-0.1-xxx where
  include base-4.9.0.0
  signature A
  module B
```


We can typecheck this component by invoking GHC with `ghc --backpack p-0.1-xxx.bkp`.  This will build interface files for each module and signature (`A.hi` and `B.hi`), object files if the component has no holes (not the case here), and a **unit file** (`p-0.1-xxx.unit`) describing the result of typechecking:

```wiki
id: p-0.1-xxx(A=hole:A)
instantiated-depends: base-4.9.0.0
exposed-modules: B
import-dirs: .
```


An external tool like Cabal may use this information to construct an entry in the installed package database for this unit.

TODO Implementation doesn't understand component IDs here yet.

**Instantiating and building a component.** To build an indefinite component, one has to explicitly specify how the holes are to be filled.  This can be done directly using the `-sig-of` flag.  For example, suppose we decide to fill `A` with `base-4.9.0.0:Data.IORef`. We can build this component using the command line `ghc --backpack p-0.1-xxx.bkp -sig-of "A=base-4.9.0.0:Data.IORef"`. This will build interface files (`A.hi`, `B.hi`), object files (`A.o`, `B.o`) and a digest file (`p-0.1-xxx.bkp`):

```wiki
id: p-0.1-xxx(A=base-4.9.0.0:Data.IORef)
instantiated-depends: base-4.9.0.0
exposed-modules: B
import-dirs: .
```

TODO Should `-sig-of` default to building interface files to the current directory, or `p-0.1-xxx-YYYYYYYY`?


But where do the parameters to `-sig-of` come from anyway? In Backpack, components get instantiated by mix-in linking.  Thus, the general mode of operation is that you typecheck a unit (`ghc --backpack q.bkp -fno-code`) in order to discover what instantiated units it needs. For example, the unit file for this unit:

```wiki
component q-2.0-yyy where
  include base-4.9.0.0 (Data.IORef as A)
  include p-0.1-xxx requires (A)
```


would be:

```wiki
id: q-2.0-yyy
instantiated-depends: p-0.1-xxx(A=base-4.9.0.0:Data.IORef)
```

**Component names.**  For hand-written Backpack files, it is not convenient to specify component IDs in the Backpack file, as they are machine-generated by Cabal and not generally known at source time.  Anywhere a component ID is valid (components and includes), a "component name" may be specified instead. E.g.,

```wiki
component p where
  include base
  signature A
  module B
```


Component names are resolved into component IDs as follows:

1. The component ID being built is specified with the `-this-component-id` flag
1. The component IDs of included packages are looked up based on the package names of exposed packages.  To select a specific version of a package, the `-package-id` flag can be used (as before).

TODO Do we need syntax for distinguishing ComponentId from ComponentName?

**Helper units.** Backpack files can also specify internal units which can be used for small-scale modularity. These are useful for several reasons:

1. It decouples the Backpack unit of modularity (component) from the Cabal unit of packaging (a package). This means you don't have to make a new Cabal package to make a instantiatiable Backpack component.
1. GHC can build all of the units in a single Backpack file at once, without having to go through Cabal (as the source is all available).
1. The ability for a single invocation of GHC to produce multiple units is a necessary capability for any other "modularity in the small" surface syntax.
1. It's really convenient for testing (ha ha)


Every Backpack file has a primary unit identified by the name of the bkp file; helper units occur before that unit in dependency order. For example, `r.bkp`:

```wiki
component internal where
  signature A
  module B
component r where
  include helper
  module A
```


The source code for a helper unit occurs in a subdirectory with the same component name, e.g. `internal/A.hsig`.  If component names are being used, GHC will automatically derive a component ID for the helper component based off of `-this-component-id`; e.g. if you pass `-component-id r-0.4-aaaa` then `internal` will be assigned `r-0.4-aaaa-internal`.


The other important thing to note is that the unit file will record a unit for each unit that is built. In this example, the unit file will be:

```wiki
id: r-0.4-aaaa-internal(A=hole:A)
instantiated-depends:
import-dirs: internal
----
id: r-0.4-aaaa-internal(A=r-0.4-aaaa:A)
instantiated-depends:
import-dirs: internal/r-0.4-aaaa-XXXXXXX
----
id: r-0.4-aaaa
instantiated-depends: r-0.4-aaaa-internal(A=r-0.4-aaaa:A)
import-dirs: .
```


The most notable thing is that the unit file records where the interface files for a unit are stored. In particular, the instantiated `r-0.4-aaaa-internal(A=r-0.4-aaaa:A)` has its interface files stored in the subdirectory `internal/r-0.4-aaaa-XXXXXXX`, where `XXXXXXX` is a deterministic, unique hash computed by GHC (which is otherwise private to GHC).

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

### Integration with Cabal


A Cabal library can be specified as Backpack enabled by using the `backpack-file` flag:

```wiki
library
  backpack-file: foo.bkp
```


This replaces any `exposed-modules`, etc. fields (which are now inferred from the Backpack file).

TODO SPJ points out that Cabal files support a more flexible format, with conditionals based on flags, and it is bad if we remove this capability.  If the Backpack file were generated by Cabal, this would not be a problem. Not quite sure what to do here yet...


A Backpack file is thus compiled with several steps:

1. Cabal calls `ghc --backpack foo.bkp -fno-code` and reads out the unit file to determine what instantiated units are necessary.  If any of these instantiated units are missing, it fails (or `cabal-install` will use this to arrange for the instantiated unit to be compiled)
1. Cabal calls `ghc --backpack foo.bkp` to actually build/typecheck the unit.
1. Cabal reads out the unit file, and creates an entry in the installed package database for each one.

### The database


The package database contains both entries for old-fashioned definite units/packages, and also entries for indefinite unit, which contain code but no objects.  Conventionally, the unit ID for an indefinite is simply the unit ID with all of the requirements filled in with `hole`s.

### Unit IDs


A unit ID is a recursive data structure which is defined to be a component ID (specified by Cabal) plus a mapping from module names to modules, where a module is a a unit ID plus a module name.


In some situations, we need serialize a unit ID into a compact, deterministic string, for use in linker symbols and file paths, as a full unit ID could be quite long (it is AT LEAST linear in the number of holes in a unit).  Ideally, the serialization format would be private to GHC (similarly to how z-encoding is GHC private), but the encoding leaks at least to the file path that GHC stores compilation results at... would be nice if there a way to avoid this problem.

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