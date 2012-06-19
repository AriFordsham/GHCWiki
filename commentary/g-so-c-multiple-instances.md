## Introduction


Cabal and GHC do not support multiple instances of the same package version installed at the same time. If a second instance of a package version is installed it is overwritten on the file system as well as in the PackageDB. This causes packages that depended upon the overwritten instance to break. The idea is to never overwrite an installed package. As already discussed in [ http://hackage.haskell.org/trac/ghc/wiki/Commentary/Packages/MultiInstances](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Packages/MultiInstances) the following changes need to be made:

- Cabal should install packages to a location that does not just depend on name and version,
- ghc-pkg should always add instances to the PackageDB and never overwrite them,
- ghc --make, ghci, and the configure phase of Cabal should select suitable instances according to some rule of thumb (similar to the current resolution technique),
- we want to be able to make more fine-grained distinctions between package instances than currently possible, for example by distinguishing different build flavours or "ways" (profiling, etc.)
- cabal-install should still find an InstallPlan, and still avoid unnecessarily rebuilding packages whenever it makes sense
- some form of garbage collection should be offered to have a chance to reduce the amount of installed packages

## Install location of installed Cabal packages


Currently the library part of packages is installed to $prefix/lib/$pkgid/$compiler. For example the GLUT package of version 2.3.0.0 when compiled with GHC 7.4.1 when installed globally lands in /usr/local/lib/GLUT-2.3.0.0/ghc-7.4.1/. This is the default path. It is completely customizable by the user. In order to allow multiple instances of this package to coexist we need to change the install location to a path that is unique for each instance. Several ways to accomplish this have been discussed:

1. Use a hash to uniquely identify package instances and make the hash part of both the InstalledPackageId and the installation path.


The ABI hash currently being used by GHC is not suitable for unique identification of a package, because it is nondeterministic and not necessarily unique. In contrast, the proposed Cabal hash should be based on all the information needed to build a package.


This approach requires that we know the hash prior to building the package, because 
there is a data directory (per default under $prefix/share/$pkgid/) that is baked into Paths_foo.hs in preparation of the build process.

1. Use a unique number as part of the installation path.


A unique number could be the number of packages installed, or the number of instances of this package version already installed, or a random number. It is important that the numbers are guaranteed to be unique system-wide, so the counter-based approaches are somewhat tricky.


The advantage over using a hash is that this approach should be very simple to implement. On the other hand, identifying installed packages (see below) could possibly become more difficult, and migrating packages to other systems is only possible if the chance of collisions is reasonably low (for example, if random numbers are being used).


2a. The unique number is also part of the installed package id.


2b. We can use another unique identifier (for example, a Cabal hash) to identify installed packages. In this case, that identifier would be allowed to depend on the output of a package build.

## ghc-pkg


ghc-pkg currently identifies each package by means of an InstalledPackageId. At the moment, this id has to be unique per package DB and is thereby limiting the amount of package instances that can be installed in a single package DB at one point in time.


In the future, we want the InstalledPackageId to still uniquely identify installed packages, but in addition to be unique among all package instances that could possibly be installed on a system. There's still the option that one InstalledPackageId occurs in several package DBs at the same time, but in this case, the associated packages should really be completely interchangeable.


Even though, as discussed above, the ABI hash is not suitable for use as the InstalledPackageId given these changed requirements, we will need to keep the ABI hash as an essential piece of information for ghc itself.


ghc-pkg is responsible for storing all information we have about installed packages. Depending on design decisions about the solver and the Cabal hash, further information may be required in ghc-pkg's description format (see below).

## Simplistic dependency resolution


The best tool for determining suitable package instances to use as build inputs is cabal-install. However, in practice there will be many situations where users will probably not have the full cabal-install functionality available:

1. invoking GHCi from the command line,
1. invoking GHC directly from the command line,
1. invoking the configure phase of Cabal (without using cabal-install).


In these cases, we have to come up with a suitable selection of package instances, and the only info we have available are the package DBs plus potential command line flags. Cabal will additionally take into account the local constraints of the package it is being invoked for, whereas GHC will only consider command-line flags, but not modules it has been invoked with.


Currently if GHC is invoked by the user it does some adhoc form of dependency resolution. The most common case of this is using ghci. If there are multiple instances of the same package in the PackageDBStack the policy used to select a single one prefers DBs higher in the stack. It then prefers packages with a higher version. Once we allow package instances with the same version within a single package DB, we need to refine the algorithm. Options are:

- pick a random / unspecified instances
- use the time of installation
- user-specified priorities
- use the order in the PackageDB
- look at the transitive closure of dependencies and their versions
- build a complex solver into GHC


Picking a random version is a last resort. A combination of installation time and priorities seems rather feasible. It makes conflicts unlikely, and allows to persistently change the priorities of installed packages. Using the order in the package DB is difficult if directories are being used as DBs. Looking at the transitive closure of dependencies makes it hard to define a total ordering of package instances. Adding a complex solver is unattractive unless we find a way to reuse cabal-install's functionality within GHC, but probably we do not want to tie the two projects together in this way.

## Build flavours


Once we distinguish several package instances with the same version, we have a design decision how precise we want that distinction to be.


The minimal approach would be to just take the transitive dependencies into account. However, we might also want to include additional information about builds such as Cabal flag settings, compiler options, profiling, documentation, build tool versions, external (OS) dependencies, and more.

### The Cabal hash


We hash the build configuration of a package that is to be built. cabal-install uses this hash to check if a package is already installed.


A build configuration consists of the following:


The Cabal hashes of all the package instances that are actually used for compilation. This is the environment. It is available in the installedPkgs field of LocalBuildInfo which is available in every step after configuration. It can also be extracted from an InstallPlan after dependency resolution.


The compiler, its version and its arguments and the tools and their version and their arguments. Available from LocalBuildInfo also. More specifically: compiler, withPrograms, withVanillaLib, withProfLib, withSharedLib, withDynExe, withProfExe, withOptimization, withGHCiLib, splitObjs, stripExes. And a lot more. \[Like what?\]


The source code. This is necessary because if the source code changes the result of compilation changes. For released packages i would assume that the version number uniquely identifies the source code. A hash of the source code should be available from hackage to avoid downloading the source code. For an unreleased package we need to find all the source files that are needed for building it. Including non-haskell source files. One way is to ask a source tarball to be built as if the package was released and then hash all the sources included in that.


OS dependencies are not taken into account because i think it would be very hard.

### Released and Unreleased packages


If we cabal install a package that is released on hackage we call this a clean install. If we cabal install an unreleased package we call this a dirty install. Clean installs are mainly used to bring a package into scope for ghci and to install applications. While they can be used to satisfy dependencies this is discouraged. For released packages the set of source files needed for compilation is known. For unreleased packages this is currently not the case.

## Dependency resolution in cabal-install


There are two general options for communicating knowledge about build flavors to the solver:


(1) "the direct way": 
i.e., all info is available to ghc-pkg and can be communicated back to Cabal and therefore the solver
the solver can therefore figure out if a particular package is suitable to use or not, in advance


(2) "the agnostic way"
this is based on the idea that the solver at first doesn't consider installed packages at all. it'll just do resolution on the source packages available.
taking all build parameters into account, Cabal hashes will be computed.
these can then be compared to hashes of installed packages.
reusing installed packages instead of rebuilding them is then an optimization of the install plan.
this doesn't require that ghc-pkg is actually directly aware of all the build parameters, as long as the hash computation is robust." -- kosmikus


The options are to support either both by putting all info into InstalledPackageInfo or to support only (2) by just putting a hash into InstalledPackageInfo. The disadvantage of supporting both is that InstalledPackageInfo would have to change more often. This could be fixed by making InstalledPackageInfo extensible. The advantages are that the additional info might be useful for other tools and that more complex rules for compatibility are possible for example non-profiling libs can depend on profiling libs. It would also be better for showing the user how two instances differ. The disadvantage of going for only (2) is that it is a big change and might cause problems with other Haskell implementations. Also if a package only exists installed and not in source form it is completely ignored. 

## Garbage Collection


It should be possible to have a garbage collection remove unneeded packages. It has to be interactive because there might be dependencies not known to Cabal and ghc-pkg. Sandboxes are useful for the user to keep track of what should be removable without causing too much damage.

## Separating storage and selection of packages


Currently the two concepts of storing package instances (cabal store) and selecting package instances for building (environment) are conflated into a PackageDB. Sandboxes are used as a workaround to create multiple different environments. But they also create multiple places to store installed packages. The disadvantages of this are disk usage, compilation time and one might lose the overview. Also if the multi-instance restriction is not lifted sandboxes will eventually suffer from the same unintended breakage of packages as non-sandboxed PackageDBs.
There should be a separation between the set of all installed packages called the cabal store and a subset of these called an environment. While the cabal store can contain multiple instances of the same package version an environment needs to be consistent. An environment is consistent if for every package version it contains only one instance of that package version.

## First class environments


It would be nice if we had some explicit notion of an environment.

## Other


The ABI hash becomes a field of InstalledPackageInfo. Some code in GHC needs to be adjusted to use this new field instead. \[You mean this is a change in behaviour? What about packages that don't have one?\]


What about builtin packages like ghc-prim, base, rts and so on?


This assumption does not hold in a multi user environment with multiple local package databases. This is a problem when building.

## Open Questions


Inplace Registration?


Who has assumptions about the directory layout of installed packages?


Executables?


Haddock?


Installation Planner?


Custom Builds and BuildHooks?


Other Compilers, backwards compatibility?


What is ComponentLocalBuildInfo for?
