# Package Compatibility


In GHC 6.8.1 we reorganised some of the contents of the packages we ship with GHC, see [\#710](https://gitlab.haskell.org//ghc/ghc/issues/710).  The idea was to lessen the problem caused by the base package being essentially static between GHC major releases.  By separating modules out of base and putting them into separate packages, it is possible to updgrade these modules independently of GHC.


The reorganisations unfortunately exposed some problems with our package infrastructure, in particular most packages that compiled with 6.6 do not compile with 6.8.1 because they don't depend on the new packages.  Some instructions for upgrading packages are here: [ Upgrading packages](http://haskell.org/haskellwiki/Upgrading_packages).


We anticipated the problem to some extent, adding "configurations" to Cabal to make it possible to write conditional package specifications that work with multiple sets of dependencies.  We are still left with the problem that the `.cabal` files for all packages need to be updated for GHC 6.8.1.  This seems like the wrong way around: the change we made to a few packages has to be propagated everywhere, when there should be a way to confine it locally, at least for the purposes of continued compatibility with existing source code.  In many cases, the underlying APIs are still available, just from a different place.  (in general this may not be true - modifications to packages may make changes to APIs which require real changes to dependent packages).


Some of the problems that contributed to this situation can be addressed.  We wrote the [ Package Versioning Policy](http://haskell.org/haskellwiki/Package_versioning_policy) so that packages can start using versions that reflect API changes, and so that dependencies can start being precise about which dependencies they work with. If we follow these guidelines, then 

- failures will be more predictable
- failures will be more informative


because dependencies and API changes are better documented.  However, we have no fewer failures than before, in fact we have more because packages cannot now "accidentally work" by specifying loose dependency ranges.


So the big question is, what changes do we need to make in the future to either prevent this happening, or to reduce the pain when it does happen?  Below are collected various proposals.  If the proposals get too long we can separate them out into new pages.

## 1. Don't reorganise packages


We could do this, but that just hides the problem and we're still left with a monolithic base package.  We still have to contend with API changes causing breakage.

## 2. Provide older version(s) of base with a new GHC release


We could fork the base package for each new release, and keep compiling the old one(s).  Unfortunately we would then have to compile every other package two (or more) times, once against each version of base.  And if we were to give the same treatment to any other library, we end up with exponential blowup in the number of copies.


The GHC build gets slower, and the testing surface increases for each release.


Furthermore, the package database cannot currently understand multiple packages compiled against different versions of dependencies.  One workaround is to have multiple package databases, but that's not too convenient.

## 4. Allow packages to re-export modules


Packages currently cannot re-export modules from other packages.  Well, that's not strictly true, it is possible to do this but it currently requires an extra package and two stub modules per module to be re-exported (see [ http://www.haskell.org/pipermail/haskell-cafe/2007-October/033141.html](http://www.haskell.org/pipermail/haskell-cafe/2007-October/033141.html)).


This could be made easier.  Suppose you could write this:

```wiki
module Data.Maybe (module Old.Data.Maybe) where
import "base-2.0" Data.Maybe as Old.Data.Maybe
```


to construct a module called `Data.Maybe` that re-exports the module `Data.Maybe` from package `base-2.0`.  This extension to the import syntax was proposed in [PackageImports](package-imports).


Using this extension, we can construct packages that re-export modules using only one stub module per re-exported module, and Cabal could generate the stubs for us given a suitable addition to the `.cabal` file syntax.


Package re-exports are useful for 

- Constructing packages that are backwards-compatible with old packages by re-exporting parts of the new API.
- Providing a single wrapper for choosing one of several underlying providers

## 4.1 Provide backwards-compatible versions of base


So using re-exports we can construct a backwards-compatible version of base (`base-2.0` that re-exports `base-3.0` and the other packages that were split from it).  We can do this for other packages that have changed, too.  This is good because:

- Code is shared between the two versions of the package
- Multiple versions of each package can coexist in the same program easily (unlike in proposal 2)


However, this approach runs into problems when types or classes, rather than just functions, change.  Suppose in `base-3.0` we changed a type somewhere; for example, we remove a constructor from the `Exception` type.  Now `base-2.0` has to provide the old `Exception` type.  It can do this, but the `Exception` type in `base-2.0` is now incompatible with the `Exception` type in `base-3.0`, so every function that refers to `Exception` must be copied into `base-2.0`.  At this point we start to need to recompile other packages against `base-2.0` too, and before long we're back in the state of proposal (2) above.


This approach therefore doesn't scale to API changes that include types and classes, but it can cope with changes to functions only.

## 4.2 Split base from underneath


This requires the re-exporting functionality described above.  When splitting base, we would create several new packages and additionally a backwards-compatibility wrapper called `base` that re-exports all of them.


Advantages:

- Updates to existing packages are much easier (no configurations required)
- Doesn't fall into the trap of trying to maintain a completely backwards-compatible version of the old API, as in 4.1


Disadvantages:

- All packages still break when the base API changes (if they are using precise dependencies on base, which they should be)
- Backwards compatibility cruft in the form of the `base` wrapper will be hard to get rid of; there's no
  incentive for packages to stop using it.  Perhaps we need a deprecation marker on packages.

## 5. Do some kind of provides/requires interface in Cabal


To do... someone please fill in a proposal here.

## 6. Distributions at the Hackage level


The idea here is to group packages into "distributions" in Hackage, with the property that all packages within a distribution are mutually compatible.  Todo... expand.

## 7. Allow package overlaps


This is not a solution to the problem of splitting a package but helps in the case that we want to use a new package that provides an updated version of some modules in an existing package. An example of this is the bytestring and base package. The base-2.0 package included Data.ByteString but it was split off into a bytestring package and not included in base-3.0. At the moment ghc allows local .hs files to provide modules that can shadow modules from a package but does not packages to shadow each other.


So an extension that would help this case would be to let packages shadow each other. The user would need to specify an ordering on packages so ghc knows which way round the shadowing should go. This could be specified by the order of the -package flags on the command line, which is equivalent to the order in which they are listed in the build-depends field in a .cabal file. This would be a relatively easy extension to implement.


Note that it only solves the problem of backporting packages to be used on top of older versions of the package they were split from. It also provides a way for people to experiment with packages that provide alternative implementations of standard modules. 


There is potential for confusion if this is used too heavily however. For example two packages built against standard and replacement modules may not be able to be used together because they will re-export different types.

## The problem of lax version dependencies


Supposing that we used solution 2 above and had a base-2.x and a base-3.x. If we take an old package and build it against base-2.x then it will work and if we build it against base-3.x then it'll fail because it uses modules from the split out packages like directory, bytestring etc. So obviously Cabal should select base-2.x, but how is this decision actually supposed to be made automatically? From a quick survey of the packages on hackage we find that 85% specify unversioned dependencies on the base package and none of them specify upper bounds for the version of base. So presented with a package that says:

```wiki
build-depends: base
```


how are we to know if we should use base-2.x or base-3.x. It may be that this package has been updated to work with base-3.x or that it only ever used the parts of base-2.x that were not split off. This dependency does not provide us with enough information to know which to choose. So we are still left with the situation that every package must be updated to specify an api version of base.


One possible remedy would be to call version 3 something other than base.  Any dependency on 'base' would then refer to the set of modules that comprise base-2.x (this is (4.2) above, incedentally).
