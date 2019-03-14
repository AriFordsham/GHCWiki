# Backpack


This is the launch page for Backpack, actively maintained by Edward (as of Jan 2019). 


Backpack is a system for retrofitting Haskell with an applicative, mix-in module system. It has been implemented in GHC 8.2 and cabal-install 2.0, but it is [not supported by Stack](https://github.com/commercialhaskell/stack/issues/2540).


The documentation for how to use Backpack is a bit scattered about at this point, but here are useful, up-to-date (as of 2019-01-11) references:

- This pair of blog posts: Try Backpack, [ghc --backpack](http://blog.ezyang.com/2016/10/try-backpack-ghc-backpack/) and [Cabal packages](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/) have up-to-date tutorials for using the main features of Backpack, with and without Cabal.

- The [GHC manual section on module signatures](https://downloads.haskell.org/~ghc/master/users-guide/separate_compilation.html#module-signatures) gives the gory details about how Backpack's signature files (hsig) work. A more user-friendly version of this can be found on [Haskell wiki "Module signature"](https://wiki.haskell.org/Module_signature)

- There is not yet a manual entry in Cabal for how Cabal works. This section is under development.

- Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) contains detailed information about the specification and implementation of Backpack. We also have an older [paper draft](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf) which was submitted to ICFP'16. History nuts can also read the original [POPL paper](http://plv.mpi-sws.org/backpack/) but note that Backpack has changed dramatically since then.

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

[Tickets labelled "backpack"](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=backpack)
