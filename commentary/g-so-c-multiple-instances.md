## Overview


It is a problem that cabal and ghc do not support multiple instances of the same package version installed at the same time. This is called the multi-instace restriction. Instead of installing them next to each other cabal overwrites the previous instance. This causes packages that depended upon the overwritten instance to break. The solution is to never overwrite an installed package.


Building a package is a function that maps a build configuration to a built package. Installing a package should mean memoizing this function to avoid rebuilding this package if possible. There should be a separation between the set of all installed packages called the cabal store and a subset of these called an environment. While the cabal store can contain multiple instances of the same package version an environment needs to be consistent. An environment is consistent if for every package it contains all dependencies which target that package have the same version.


Currently the two concepts of storing package instances (cabal store) and selecting package instances for building (environment) are conflated into a PackageDB. Sandboxes are used as a workaround to create multiple different environments. But they also create multiple places to store installed packages. The disadvantages of this are disk usage, compilation time and one might lose the overview. Also if the multi-instance restriction is not lifted sandboxes will eventually suffer from the same unintended breakage of packages as non-sandboxed PackageDBs.

## Dependency resolution


The dependency resolver currently comes up with an install plan. An install plan is similar to an environment. Like an environment it is a set of installed packages. But it may also contain configurations for packages that are not installed yet. Like and environment it needs to be consistent.


"An installation plan is a set of packages that are going to be used together. It will consist of a mixture of installed packages and source packages along with their exact version dependencies." -- InstallPlan documentation


The dependencies of a package version are what is currently listed under dependencies in a cabal file. A list of packages with version contraints that may be used to build this package version.


There should be at least two modes for dependency resolution.


The dependency resolver uses the dependencies of all possible source packages to find a set of package configurations. This is already an install plan. It then in this set replaces configurations for already installed packages by the installed package to make it more efficient.


The other mode is what is currently done. The set of installed packages is taken into account. This might avoid more rebuilding but people might not always get the latest packages.


Also there might be installed packages for which the source is not available. If the source is not available and installed packages are ignored those packages can not appear in the install plan.

## Using Cabal without cabal-install


Currently if Cabal is asked to configure a package from a Setup.hs script without using cabal-install some adhoc dependency resolution that only takes into account the installed packages takes place. It is a dependency resolution because it takes the set of installed packages and creates an environment. If cabal-install figures out the environment we will want to supply this step with it.

## Using GHC without Cabal and cabal-install


Currently if GHC is invoked by the user it does some adhoc form of dependency resolution. The most common case of this is using ghci. If there are multiple instances of the same package in the PackageDBStack this needs to be adjusted.

## Inplace Registration


We try to never overwrite the files of an installed package. In the case of inplace registration this is impossible because the overwriting has already happened. I feel that inplace registration should be discouraged.

## Released and Unreleased packages


If we cabal install a package that is released on hackage we call this a clean install. If we cabal install an unreleased package we call this a dirty install. Clean installs are mainly used to bring a package into scope for ghci and to install applications. While they can be used to satisfy dependencies this is discouraged. For released packages the set of source files needed for compilation is known. For unreleased packages this is currently not the case.

## The cabal hash


Cabal needs to have a function to hash a build configuration. cabal-install uses the hash to check if a package needs to be built or if it is already installed. Cabal needs the hash during installation because the directory of an installed package contains the hash. It is $libdir/$pkgid/$installedpackageid. It also needs the hash during registration because a package is identified by it in the PackageDB. The hash is the new InstalledPackageId (Probably not the hash alone, but also the package name (and also still the ABI hash?)).


A build configuration consists of the following:


The hashes of all the package instances that are actually used for compilation. This is the environment. It is available in the installedPkgs field of LocalBuildInfo which is available in every step after configuration. It can also be extracted from an InstallPlan after dependency resolution.


The compiler, its version and its arguments and the tools and their version and their arguments. Available from LocalBuildInfo also. More specifically: compiler, withPrograms, withVanillaLib, withProfLib, withSharedLib, withDynExe, withProfExe, withOptimization, withGHCiLib, splitObjs, stripExes. And a lot more. \[Like what?\]


The source code. This is necessary because if the source code changes the result of compilation changes. For released packages i would assume that the version number uniquely identifies the source code. A hash of the source code should be available from hackage to avoid downloading the source code. For an unreleased package we need to find all the source files that are needed for building it. Including non-haskell source files. One way is to ask a source tarball to be built as if the package was released and then hash all the sources included in that.


Can a dirty install ever have the same hash as a clean install? No, because if it had it would have to use the same source code. But this source code is released so by definition it would be a clean install. Even if the source code was downloaded manually and cabal install was invoked in that directory.


OS dependencies are not taken into account because i think it would be very hard.

## Other


The ABI hash becomes a field of InstalledPackageInfo. Some code in GHC needs to be adjusted to use this new field instead. \[You mean this is a change in behaviour? What about packages that don't have one?\]


What about builtin packages like ghc-prim, base, rts and so on?

## Open Questions


Who has assumptions about the directory layout of installed packages?


Executables?


Haddock?


Garbage Collection of unused packages?


Installation Planner?


Custom Builds and BuildHooks?


Other Compilers, backwards compatibility?


What is ComponentLocalBuildInfo for?
