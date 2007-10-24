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

## 3. Provide older versions of base that re-export the new version


To do..

## 4. Do some kind of provides/requires interface in Cabal


To do..

## 5. Allow package overlaps


This is not a solution to the problem of splitting a package but helps in the case that we want to use a new package that provides an updated version of some modules in an existing package. An example of this is the bytestring and base package. The base-2.0 package included Data.ByteString but it was split off into a bytestring package and not included in base-3.0. At the moment ghc allows local .hs files to provide modules that can shadow modules from a package but does not packages to shadow each other.


So an extension that would help this case would be to let packages shadow each other. The user would need to specify an ordering on packages so ghc knows which way round the shadowing should go. This could be specified by the order of the -package flags on the command line, which is equivalent to the order in which they are listed in the build-depends field in a .cabal file. This would be a relatively easy extension to implement.


Note that it only solves the problem of backporting packages to be used on top of older versions of the package they were split from. It also provides a way for people to experiment with packages that provide alternative implementations of standard modules. 


There is potential for confusion if this is used too heavily however. For example two packages built against standard and replacement modules may not be able to be used together because they will re-export different types.

## The problem of lax version dependencies


Supposing that we used solution 3 above and had a base-2.x and a base-3.x. If we take an old package and build it against base-2.x then it will work and if we build it against base-3.x then it'll fail because it uses modules from the split out packages like directory, bytestring etc. So obviously Cabal should select base-2.x, but how is this decision actually supposed to be made automatically? From a quick survey of the packages on hackage we find that 85% specify unversioned dependencies on the base package and none of them specify upper bounds for the version of base. So presented with a package that says:

```wiki
build-depends: base
```


how are we to know if we should use base-2.x or base-3.x. It may be that this package has been updated to work with base-3.x or that it only ever used the parts of base-2.x that were not split off. This dependency does not provide us with enough information to know which to choose. So we are still left with the situation that every package must be updated to specify an api version of base.
