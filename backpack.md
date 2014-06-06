[ Backpack](http://plv.mpi-sws.org/backpack/) is a proposal for
retrofitting Haskell with an applicative, mix-in module system. The
theory of Backpack is developed in the paper and its accompanying
technical appendix; the purpose of this wikipage is to record some of
the more practical implementation considerations.

## Version ranges and signatures


In the current Haskell package ecosystem, version ranges on
build-depends are a way of indicating what libraries a package may build
with.  While the [ PVP](http://www.haskell.org/haskellwiki/Package_versioning_policy) is
intended to provide a way for library authors to communicate when API
changes occur.  However, in practice, downstream authors find it difficult
to correctly choose appropriate version ranges for their software (often
providing a bound that is far too low, or optimistically asserting that
they will be compatible against all future versions of a library).


In Backpack, the use of package signatures subsumes this application of
version numbers. (Version numbers are still useful, in case an
application needs to explicitly exclude a buggy version of a package).
In the Backpack papers, these signatures are explicitly recorded.


An important caveat is that for backwards-compatibility, there will
often be many packages which sport version ranges, but do not have
explicit signatures for their holes.  Furthermore, a conversion from
version ranges to signatures may be useful for bootstrapping explicit
signatures going forwards.  Assuming that, given a package, it is
possible to compute its signature, there are a few possible ways to go
about computing signatures based on version ranges:

- We could simply take the latest acceptable version of a package
  and use its signature.

- We could compute the “greatest common signature” for the specified
  version range in build-depends. This signature is the widest signature
  for which all of the versions are compatible with.  Depending on whether
  or not the build-depends range is correct, the signature could be buggy.
  (ezyang: When I merge two type signatures together, does Backpack require
  them to be identical? I think so, but it's not obvious from the paper.
  If this is the case, it could be annoying if someone generalized a
  function--but I don't think there is anything we can do here.)
  Note that while a signature built this way is guaranteed to be
  backwards-compatible, it may not be forwards-compatible, in the sense
  that a future module might not typecheck against the signature,
  but still be linkable against our library.

- For maximum backwards and forwards compatibility, the greatest common
  signature could be refined by finding the thinnest signature with
  which our package type checks with. This is the “ground truth” with
  regards to what our package relies on.  If this could be completely
  automatically calculated, version ranges in build-depends would be
  completely unnecessary; however, it is not possible to infer this
  information just from usage sites--thus it may be useful to have
  some "known to build against" versions to start the process (however,
  a full version range may not be necessary).

## Cabal versus Backpack


At a first glance, a Cabal package looks roughly equivalent to a
Backpack package.  However, the precise details of this correspondence
are tricky: there a few choices to be made that affect the design space.


As a running example, here is a simple Cabal file specifying a
package:

```wiki
name: abc
other-modules: Internal
exposed-modules: A B C
build-depends: base
```


The package is associated with a directory of named modules
corresponding to other-modules and exposed-modules.

### Cabal packages as a fully linked Backpack package


A simple model is to treat a Cabal package as a fully linked Backpack
package.  Build dependencies are unambiguous, because to Backpack, there
is only one implementation of any given package (selected by the
dependency solver).  This is roughly how Cabal packages work today, and
this encoding shares many of the same downsides (two versions of the
same package cannot coexist in the same unit, build-dependency
resolution has little relationship to whether or not a package can
actually build against a given version, etc).


The basic story is that named modules in a Cabal package implicitly
defines the module binding (where the module implementation is unnamed)
in the Backpack package, with non-exported modules thinned out.  Holes
are not used: the bindings of a Backpack package are conceptually part
of a global namespace, and any build dependencies are simply dumped into
this namespace (using include).  Here is the translation of our example:

```wiki
package abc (A, B, C) where
  include base
  Internal = "Internal.hs"
  A = "A.hs"
  B = "B.hs"
  C = "C.hs"
```


Where the quoted strings indicate the file that a physical moudle
inclusion lives in. (Strictly speaking, we should ignore the module
name that lives in that file.)


An executable is just a package that contains a special Main module,
which will be used as the entry point for the executable (executables
are, naturally, mutually exclusive of one another.) But see
[ https://github.com/haskell/cabal/issues/1847](https://github.com/haskell/cabal/issues/1847)


There are a few subtleties here:

- The list of includes does not need to be topologically sorted, since
  every depended upon package manages its own dependencies.

- The applicative semantics of Backpack mean that Backpack can identify
  all includes of the same module as physically identical, so there aren't
  any unification problems.

- Because the logical name always coincides with the physical name,
  we can define the physical identity of modules as just their logical name.

### Cabal packages as Backpack packages with holes


While the previous model essentially faithfully preserves the previous
semantics of Cabal packages, we might be interested in a more flexible
semantics, which provides some of the advantages of Backpack but doesn't
require Cabal packages to be rewritten.  For example, we may want to
give users more flexibility in how the build-dependencies of packages
are fulfilled: instead of treating Cabal packages as fully linked
Backpack packages, we want to treat them as Backpack packages with
holes.


This requires two main changes:

- The logical namespace of modules should no longer be considered a
  global namespace.  This makes it possible to deal with name clashes,
  since if two packages have a module named A, a third package that
  depends on both of them can rename one instance to avoid a clash.
  (Unfortunatey, logical and physical names no longer coincide, so
  things need to be renamed.)  A killer use-case for this functionality
  is seamless backwards compatibility packages for old versions of
  libraries.

- Dependency resolution should not be considered as some magical process
  by which fully-linked Backpack packages are created, but rather, a
  mechanism for automatically linking packages with holes (the Cabal
  packages) together, whose mechanism could be overridden by
  the user.


In this universe, our Backpack translation looks more like this:

```wiki
package base-sig where
  Prelude :: ???
  Data.Bool :: ???
  ...

package abc (A, B, C) where
  include base-sig
  Internal = "Internal.hs"
  A = "A.hs"
  B = "B.hs"
  C = "C.hs"
```


The crux of the matter is providing the "signature package" associated
with base, which contains signatures for all of its modules.  In a
new-world order, we might expect packages to actually provide signature
packages (more on this later), but for the vast majority of old
packages, we'll have to compute this automatically, as described in
"Version ranges and signatures".


At this point in time, the design for the extended linking mechanism
is not well specified, however, it might include the following:

- Full Backpack packages can explicitly include an upstream Cabal
  package with holes and explicitly fill in dependencies. This
  is "full manual".

### The next-generation of Cabal-Backpack packages


To take full advantage of Backpack features,

---


More importantly, Backpack offers a tantalizing story for managing different versions of packages, alluded to in the paper, but not elaborated on. In Cabal, the version number range of a build-depends implicitly defines a "signature" which we depend on. There are a few ways this signature could be computed:

-   We could throw our hands up in the air and say the signature is just whatever Cabal computed. This does not lead to very reproducible builds, but it is the current status quo.

-   We could compute the “greatest common signature” for the specified version range. This signature is the widest signature for which all of the versions are compatible with. This can be used to determine if there is a buggy version range; if the greatest common signature isn’t enough to compile the package, there must exist some version that is listed as compatible, but isn’t actually. For unbounded version ranges, this can be a bit dodgy, but the PVP suggests that if you’re not unbounded over too many version numbers, you’ll be OK

-   We could further refine the greatest common signature, by finding the thinnest signature with which our package type checks with. This is the “ground truth” with regards to what our package relies on, although maintaining a signature like this could be pretty annoying, on the order of annoyance of having to maintain explicit import lists (and type signatures) for everything that you use. If this is automatically calculated, we could do away with version dependencies and just see if signatures are satisfied. This could have performance problems, and calculating this requires a bit of work.


By the way, this means that naively implementing the suggestion in the Backpack paper where modules provide signature files is quite dodgy, because the signature file is likely to contain too much “stuff”, and in any case, needs to be referred to in a way that is stable across versions. On the other hand, one could easily manage partitioning signatures into “latest and greatest” versus “well, this is pretty stable for most people”; differently, one could pin to a signature for a version, and as long as the package doesn’t break backwards compatibility that signature will work for a while.

### Interlude: Cabal-specific features


Cabal is not just a packaging mechanism, but it also handles a number of
concerns that Backpack does not talk about:

1. Cabal packages specify some out-of-band information, such as package


metadata (copyright, author, etc), preprocessor dependencies, data
files, documentation, test suites, how to build the package (if not
standard), external library dependencies, Haskell executables which
specify programs.

1. Cabal packages can include C code and other foreign language code,


whereas Backpack is only Haskell code, and

1. Cabal packages have a conditional flags mechanism, by which various


"options" can be tweaked at compile time. (Needless to say, this can
cause very bad problems for modularity!)


In the fully-linked setting, none of these features need to be treated
specially.  However, to improve over Cabal, Backpack needs to accomodate
(2) and (3).

## More features

### Inference for recursive module bindings


Scott thinks there is a way to automatically infer the necessary hs-boot signatures for recursive module bindings. His proposal is here: [ http://www.reddit.com/r/haskell/comments/1id0p7/backpack_retrofitting_haskell_with_interfaces/cb42m8l](http://www.reddit.com/r/haskell/comments/1id0p7/backpack_retrofitting_haskell_with_interfaces/cb42m8l)

---

[ https://ghc.haskell.org/trac/ghc/wiki/Commentary/GSoCMultipleInstances](https://ghc.haskell.org/trac/ghc/wiki/Commentary/GSoCMultipleInstances)