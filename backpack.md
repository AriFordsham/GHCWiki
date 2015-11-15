[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for retrofitting Haskell with an applicative, mix-in module system. The theory of Backpack is developed in the paper and its accompanying technical appendix. The purpose of this page is to track implementation progress. (Wondering what happened to the old text? Check the history; it will be integrated into the implementation design doc eventually).


See also [CabalDependency](cabal-dependency)

## Reading

- [ The Backpack documentation directory](https://git.haskell.org/ghc.git/blob/HEAD:/docs/backpack/). You'll have to build them (just `make`), but here are the files of interest:

  - algorithm.tex specifies the abstract core Backpack algorithms for GHC.
  - backpack-manual.tex is a user-facing manual for using Backpack.
  - backpack-impl.tex is an old but interesting technical document describing many of the technical tradeoffs in our design.
- [ What's a module system good for anyway?](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/) Motivates \*why\* you might care about Backpack
- The [commentary pages about packages](commentary/compiler/packages)

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