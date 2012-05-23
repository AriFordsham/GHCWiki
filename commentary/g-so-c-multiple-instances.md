## Overview


It is a problem that cabal does not support multiple instances of the same package version installed at the same time. \[Mainly ghc, not cabal. Cabal's non-support is a consequence.\] Instead of installing them next to each other it overwrites the previous instance. This causes packages that depended upon the overwritten instance to break. The solution is to never overwrite an installed package. In the case of inplace registrations the overwriting has already taken place which is a problem. [?](commentary/g-so-c-multiple-instances?)


Relating this to how Nix works. Cabal stores potentially every instance of every package possible. Lets call this the cabal store. \[Are you talking about Nix or about Cabal? Does the store really contain all packages, even packages not ever built?\] There might at least be a global and a local one. \[Might be?\] Shadowing doesn't matter because if two packages have the same hash they should be interchangeable. \[Define shadowing.\]


The dependency resolver comes up with an install plan. \[Is this part of the current situation or part of the solution?\] In this install plan all packages have completely fixed dependencies based on the dependencies specified in the cabal file. Same of them are already present in the cabal store and some aren't. They are a subset of all possible package instances. This corresponds to a profile in Nix as well as a sandbox. We call this an environment. \[I still don't get this definition of environment. In particular, I fail to see how an environment is related to the solver.\]

## Dependency resolution


The dependency resolver takes into account which packages are already installed and tries to reuse them. \[Is this part of the current situation or part of the solution?\] Another option would be for the resolver to ignore which packages are already installed. It then computes the hashes for the packages it needs for compilation. Then those that aren't already present in the cabal store are built. \[Tradeoffs?\]

## Released and Unreleased packages


If we cabal install a package that is released on hackage we call this a clean install. Those should not be used to satisfy dependencies but rather to bring a package into scope in ghci to play with it. \[Or to install an application? Why not phrase this positively? Also, why not first give the definitions completely, then discuss the differences.\] If we cabal install an unreleased package we call this a dirty install. I assume that the source code for a released package is uniquely identified by its version number. \[Why is this important?\] For unreleased packages this is not the case.

## The cabal hash


The idea is to identify installed packages by a hash of the information needed to build them. This hash is the new InstalledPackageId. \[Probably not the hash alone, but also the package name (and also still the ABI hash?).\] The new installation directory for each instance is $libdir/$pkgid/$installedpackageid. \[Do we need to know the path at configure time, or build time, or only after build time?\] The hash is computed during installation in GHC.installLib as well as during registration in Register.generateRegistrationInfo. \[So it's computed by Cabal. At what stage? Where's the hash stored (if at all)?\]


The hash contains the following information:


The hashes of all the package instances that are actually used for compilation. This is called the environment. Those are available in the installedPkgs field of LocalBuildInfo. \[When? Where?\]


The compiler, its version and its arguments and the tools and their version and their arguments. Available from LocalBuildInfo also. More specifically: compiler, withPrograms, withVanillaLib, withProfLib, withSharedLib, withDynExe, withProfExe, withOptimization, withGHCiLib, splitObjs, stripExes. And a lot more. \[Like what?\]


The source code. This is necessary because if the source code changes the result of compilation changes. For released packages i would assume that the version number uniquely identifies the source code and only hash that but what about unreleased packages? \[Again, why is it important? Because we don't want to download the tarballs for hash computation? But then, do we just use the version? Can a dirty install ever have the same hash as a clean install?\] From the PackageDescription's library field the exposedModules can be extracted. Also from PackageDescription extraSrcFiles can be extracted. What about the Other Modules? We should also make sure that GHC used/uses only the files we ware hashing for compilation.


Or we first ask a source tarball to be built as if the package was released and then this one is hashed. \[Or? What's the difference? The compression?\]


OS dependencies are not taken into account. \[Why not?\]


What is ComponentLocalBuildInfo for?

## Other


The ABI hash becomes a field of InstalledPackageInfo. \[You mean this is a change in behaviour? What about packages that don't have one?\]


For inplace package registration any packages with the same location must be unregistered. For that you must ask for all installed packages, find the one that is installed to that location and unregister it. \[I don't understand this.\]


What about builtin packages like ghc-prim, base, rts and so on?

## Open Questions


Who has assumptions about the directory layout of installed packages?


Executables?


Haddock?


Garbage Collection of unused packages?


Installation Planner?


Custom Builds and BuildHooks?


Other Compilers, backwards compatibility?
