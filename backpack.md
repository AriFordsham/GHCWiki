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
is not well specified.  In Backpack cookbook below, we describe a number
of common patterns where a Backpack package can be used to manually
link a package in a special way.

### The next-generation of Cabal-Backpack packages


In this model, users write their code specifically to use the Backpack
module system; however, they continue to distribute their code on Hackage
and may be interested in concurrently supporting users who are not
interested in using Backpack.


The purpose of this section is to describe what we imagine the best
practices for Backpack package construction to be.


(TODO write section)

## Backpack cookbook


Here are a few common tasks which show up when managing package
dependencies, and how to resolve them using Backpack.  It might be a
good idea to support some of these directly with nice surface syntax.
Many of these recipes are based with one of the most annoying
situations when using Cabal: your package fails to build because
of some sort of dependency problem, and you don't really want to have
to go and patch upstream to fix the problem.


Note that if you are depending against tight signatures, instead of
version ranges, we expect to obviate many of these measures.

### Hiding non-exported dependencies


In this situation, your application is using two separate libraries
which have identically named holes for two different versions of the
same upstream library.  However, one of these libraries uses the
upstream library in a strictly non-exported way: informally, use of this
dependency is strictly an implementation detail.


In Backpack, we can arrange for a non-exported dependency by linking a
hole with the appropriate implementation and then thinning the
resulting module definition out.  Here is a worked example:

```wiki
-- Upstream library with multiple, backwards-incompatible versions
package arrays-1.x-sig where
    Array :: ...
package arrays-1.0 where
    Array = ...

package arrays-2.x-sig where
    Array :: ...
package arrays-2.0 where
    Array = ...

-- Upstream (Cabal) package which is only compatible with arrays-1.x
package graph where
    include arrays-1.x-sig -- In Cabal, this signature is calculated automatically
    Graph = ...

-- (**) Package which does not export Array at all
package graph-noexport-array (Graph) where
    include arrays-1.0
    include graph

-- Application
package application where
    include graph-noexport-array
    include arrays-2.0
    Main = [
        import Array -- uses arrays-2.0 implementation
    ]
```


Intuitively, the reason this works is that only one Array implementation
is visible from application, so no linking (which would fail) between
the array-1.0 and array-2.0 occurs.  It would also work to rename Array
to another logical name, which would also prevent the linking process.

```wiki
-- (**) Package which exports array as a fresh name
package graph-renamed-array (Graph) where
    include arrays-1.0
    include graph (Array as PrivateArray)
```


ezyang: Close study of this example reveals an additions to the surface
language which would be quite useful.  It's easy to imagine the export
list for graph-noexport-array becoming unwieldy when graph defines a lot
of modules.  While it is reasonable for graph to explicitly provide a
module list (current Cabal best-practice), it's less reasonable if we
have to repeat the list later.  Two obvious ways of dealing with this
are to allow signature-level exports/hiding.  Here, we simply collect up
all of the modules mentioned in a signature and export/hide those.
Hiding may be more convenient, since you have to include not only the
signature of the API that was implemented, but any other types which
live in other signatures which are exported.

### Injecting a backwards compatibility shim

---

## Interlude: Cabal-specific features


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