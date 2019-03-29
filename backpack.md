# Backpack


This is the launch page for Backpack, actively maintained by Edward (as of Jan 2019). 


Backpack is a system for retrofitting Haskell with an applicative, mix-in module system. It has been implemented in GHC 8.2 and cabal-install 2.0, but it is [not supported by Stack](https://github.com/commercialhaskell/stack/issues/2540).


The documentation for how to use Backpack is a bit scattered about at this point, but here are useful, up-to-date (as of 2019-01-11) references:

- This pair of blog posts: Try Backpack, [ghc --backpack](http://blog.ezyang.com/2016/10/try-backpack-ghc-backpack/) and [ Cabal packages](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/) have up-to-date tutorials for using the main features of Backpack, with and without Cabal.

- The [GHC manual section on module signatures](https://downloads.haskell.org/~ghc/master/users-guide/separate_compilation.html#module-signatures) gives the gory details about how Backpack's signature files (hsig) work. A more user-friendly version of this can be found on [ Haskell wiki "Module signature"](https://wiki.haskell.org/Module_signature)

- There is not yet a manual entry in Cabal for how Cabal works. This section is under development.

- Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) contains detailed information about the specification and implementation of Backpack. We also have an older [ paper draft](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf) which was submitted to ICFP'16. History nuts can also read the original [ POPL paper](http://plv.mpi-sws.org/backpack/) but note that Backpack has changed dramatically since then.

- Hackage supports uploads of Backpack using packages. For an example, see [https://hackage.haskell.org/package/unpacked-containers](https://hackage.haskell.org/package/unpacked-containers)


You might find it useful to find some code using Backpack.  Here are the biggest examples worth looking at:

- [unpacked-containers](https://hackage.haskell.org/package/unpacked-containers) supplies unpacked sets and maps, using Backpack's ability to unpack through signatures.

- [backpack-str](https://github.com/haskell-backpack/backpack-str) defines a signature and implementations for strings. It is quite comprehensive.

- [coda](https://github.com/ekmett/coda) parametrizes over a few core data types including notions of "delta". It takes advantage of zero-cost abstraction, which lets it split into multiple data types, while still ensuring they are UNPACKed in the end.

- [streamy](https://github.com/danidiaz/streamy) defines a signature and implementations for "streaming" libraries (e.g., conduit, pipes and streaming).

- [haskell-opentracing](https://github.com/ocharles/haskell-opentracing) defines a signature for the OpenTracing standard, a middleware built on top of this signature, and (at the moment) a single backend to Jaeger.

- [reflex-backpack](https://github.com/ezyang/reflex-backpack) is a kind of crazy experiment at Backpack'ing Reflex.  Reflex uses a lot of advanced GHC features and it took some coaxing to get Backpack to handle it all, but handle it all it did!


Some more out-of-date documents:

- [Backpack specification](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst). This was subsumed by my thesis but once Backpack stabilizes it will be worth distilling the thesis PDF back into a more web-friendly format.

## Known gotchas

**Can I use this with Stack?** No, Backpack requires support from the package manager, and Stack integration has not been implemented yet.

**Can I use this with Template Haskell?** Yes. Note that there is currently one known issue about TH with Backpack, which you may be affected by: [https://github.com/haskell/cabal/issues/5634](https://github.com/haskell/cabal/issues/5634)

**Can I use this with the C preprocessor?** Yes; this used to be buggy but the fix is released in the latest version of GHC.

**Make sure cabal-version is recent enough.** ([\#4448](https://github.com/haskell/cabal/issues/4448)) If you set the `cabal-version` of your package too low, you may get this error:

```wiki
Error:
    Mix-in refers to non-existent package 'pkg'
    (did you forget to add the package to build-depends?)
    In the stanza 'executable myexe'
    In the inplace package 'pkg'
```


This is because internal libraries are feature-gated by the `cabal-version` of your package. Setting it to `cabal-version: >= 2.0` is enough to resolve the problem.

**You can't instantiate a dependency with a locally defined module.** Consider the following package:

```wiki
library
  other-modules: StrImpl
  build-depends: foo-indef
  mixins: foo-indef requires (Str as StrImpl)
```


This looks like it should work, but actually it will fail:

```wiki
Error:
    Non-library component has unfilled requirements: StrImpl
    In the stanza 'library'
    In the inplace package 'mypkg-1.2'
```


The reason for this is Backpack does not (currently) support instantiating a package with a locally defined module: since the module can turn around and \*import\* the mixed in `foo-indef`, which would result in mutual recursion (not presently supported.)


To solve this problem, just create a new library to define the implementing module. This library can be in the same package using the convenience library mechanism:

```wiki
library str-impl
  exposed-modules: StrImpl

library
  build-depends: str-impl, foo-indef
  mixins: foo-indef requires (Str as StrImpl)
```

**How can I declare that a module implements a signature?**  In traditional Haskell style, you can write `x :: Type` to specify that a value `x` should have some type.  In Backpack, specifying that a module implements a signature is done out-of-line; you must create a third component to link them together (e.g., a test suite):

```wiki
test-suite implements
  type:                exitcode-stdio-1.0
  main-is:             Main.hs
  build-depends:
    base,
    foo-implementation,
    foo-sig
  default-language:    Haskell2010
```


A few notes about this encoding:

- If you need to specify that a module implements multiple signatures, you include all of those signatures in the same implements test or create separate implements tests.

- Being a test suite, this requires you to create a dummy `Main.hs` file (`main = return ()` is sufficient) and add a `base` dependency.  So why do we pick a test suite?  A test suite will ensure that you have in fact filled all of the holes of a `foo-sig`, whereas a regular library will happily pass through any unfilled holes, making it easy for you to think that a check has occurred when it has not.

- You might wonder if you can skip defining an extra test-suite by mixing in the signature package from the implementation package. Unfortunately, this runs afoul the "you can't instantiate a dependency with a local module" restriction. Additionally, this adds an extra spurious dependency to your package which is not actually needed.

## Backpack-related tickets



Backpack-related tickets are marked with keyword 'backpack'. If the ticket is assigned to ezyang, it means he's planning on working on it.




  
  
  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)
      </th>
<th>
        
        Type (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)
      </th>
<th>
        
        Summary (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)
      </th>
<th>
        
        Priority (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)
      </th>
<th>
        
        Owner (Ticket query: keywords: backpack, status: new, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1409">#1409</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/1409">Allow recursively dependent modules transparently (without .hs-boot or anything)</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9351">#9351</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/9351">add ability to version symbols .c for packages with C code</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10749">#10749</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/10749">Boot file instances should imply superclasses</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10827">#10827</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/10827">GHCi should support interpeting multiple packages/units with separate DynFlags</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12703">#12703</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/12703">Expand Backpack&apos;s signature matching relation beyond definitional equality</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13151">#13151</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13151">Make all never-exported IfaceDecls implicit</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13266">#13266</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13266">Source locations from signature merging/matching are bad</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13469">#13469</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13469">-fdefer-type-errors for Backpack</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14212">#14212</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/14212">Give better error message with non-supported Backpack/TH use</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14478">#14478</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/14478">Abstract pattern synonyms (for hsig and hs-boot)</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15391">#15391</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/15391">Maybe ghc-pkg register should unregister packages with &quot;incompatible&quot; signatures</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15984">#15984</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/15984">Backpack accepts ill-kinded instantiations. Can cause GHC panic</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16412">#16412</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16412">Type family signatures in indefinite modules</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10266">#10266</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/10266">Split base for Backpack</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10681">#10681</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/10681">Teach GHC to interpret all hs files as two levels of hs-boot files (abstract types only/full types + values)</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12680">#12680</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/12680">Permit type equality instances in signatures</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13149">#13149</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13149">Giving Backpack a Promotion</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13262">#13262</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13262">Allow type synonym family application in instance head if it has no free variables</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13361">#13361</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13361">Better type synonym merging/subtyping for Backpack</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13765">#13765</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/13765">GHC cannot parse valid Haskell98 whose first identifier is named signature</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14210">#14210</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/14210">bkp files cannot find TemplateHaskell symbols (even without Backpack features)</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10871">#10871</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/10871">Implement &quot;fat&quot; interface files which can be directly compiled without source</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      ezyang
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12717">#12717</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/12717">Permit data types in signatures to be implemented with equivalent pattern synonyms (and vice versa)</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



