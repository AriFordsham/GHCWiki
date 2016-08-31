[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for retrofitting Haskell with an applicative, mix-in module system. The theory of Backpack is developed in the paper and its accompanying technical appendix. The purpose of this page is to give pointers to where you can learn more about its design in GHC.


Here are the main documents:

- [ Backpack specification](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst). This is the full user-facing specification, which also describes the GHC interface (not intended to be used by end-users, but an important IR.)
- [ Backpack implementation guide](https://github.com/ezyang/ghc-proposals/blob/backpack-impl/proposals/0000-backpack-impl.rst). This is a half-baked guide to review the Backpack patchset.


You might also be interested in the [commentary pages about packages](commentary/compiler/packages) and [CabalDependency](cabal-dependency).

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

## Future things to think about


The language of signature files sets the stage for other possible features. Here are two fo them:

1. Is my library compatible against a given version of a dependency?  To determine this today, you must first install the library, and then build your code against it. With Backpack, you can write down precisely what interface you depend against, at which the compatibility check only involves testing if an implementation correctly implements the interface. Better yet, a library with explicit Backpack dependencies can be installed without installing any of its prerequisites. This information could be collected together in order to give accurate version dependencies. (TODO Interesting problem: Backpack says nothing about what should happen when someone generalizes a type signatures.  Conditional compilation suggests that there may need to be multiple interface sets that a package can compile against; variational programming but only with interfaces.) (TODO Right now, versions and instantiation are completely orthogonal, which sucks.)

1. Does anyone depend on this API?  If you want to make a backwards incompatible change to a library, it can be difficult to tell who will be affected. Explicit interfaces are *transmissible*; clients should be able to submit the slices of the interfaces they depend on to upstream, giving maintainers a view into what APIs are used.  This capability would be especially beneficial for packages with a large and organically grown API (e.g. the ghc package).  (TODO In what sense is an interface transmissible? Interface needs to be able to refer to other types which need to live somewhere. These are "subsidiary" in some sense; when checking for compatibility you don't care about these types.  Need to analyze this situation more carefully. See also [\#10798](https://gitlab.haskell.org//ghc/ghc/issues/10798).)
