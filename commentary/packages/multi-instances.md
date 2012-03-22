# Multi-instance packages


This page is about how to change the package system to allow multiple instances of a package to be installed at the same time.  There are two reasons we want to be able to do this:

- To be able to track the different "ways" in which a package is available: e.g. profiling, dynamic.  At the moment, the package database doesn't track this information, with the result that the user has to reinstall packages with `--enable-profiling` on a trial-and-error basis in order to get profiling support for packages they have already installed. The same holds, in principle, for different flag settings of a package. It would be nice if ghc-pkg could track these as well.

- To make installing new packages more robust.  When installing a new package, we sometimes need to upgrade packages that are already installed to new versions, which may require recompiling other packages against the new version.  For example, if we have P1 installed, Q1 depends on P (any version), and we need to install R that depends on both P2 and Q1.  We need to build P2, rebuild Q1 against P2, and finally build R against P2 and the new Q1.  We would like to do this without removing P1 or the old Q1 from the package database, because other packages may be depending on the old Q1, and we don't want to break those packages (which is what currently happens with GHC 7.0).

## ToDo list

- ghc-pkg: do not overwrite previous instances in the package DB

  - but we need to think about the case where it's the "same" package being re-registered, e.g. it's actually installed in the same file system location as the old instance, and the old files are no longer. This worry applies to the current `ghc-pkg update` command. (kosmikus: I'm not sure I understand this. Are you referring to the situation where you re-register an unchanged package, or where you really overwrite an existing package on the file system. The latter should be a user error, I'm not sure if ghc-pkg should be able to handle it. New ghc versions should depend on a Cabal version that handles this correctly. Most people don't install packages by hand, I guess.) 

- GHC: discard conflicting instances during its shadowing phase

  - SDM: GHC will currently do \*something\* here, but it might end up with a result that the user didn't want/expect.  One way to improve things is to prioritise packages that were installed more recently.
  - Andres suggests that GHC should be much cleverer, and look at the actual dependencies of the modules being compiled before deciding which packages to enable.  This would almost certainly result in more things working and possibly less surprising behaviour sometimes, but Simon thinks that (a) it is too hard, (b) if users need this, they should use Cabal and its dependency resolver, which will do a good job, (c) you can often resolve problems by adding `-package X`, and (d) eventually we will want a system where users manage separate sessions, so they can set up an environment in which the packages they want are available.  This has a lot in common with `cabal-dev` and sandboxes, so the mechanisms (and concepts) should be shared. (kosmikus: perhaps an alternative is to force the user to make an active decision in case of conflicts, i.e., to create a sandbox that exposes a consistent package set).

- GHC: allow specifying a package instance in the -package flags

  - SDM: already done

- instances of packages must install in a different location

  - install directory includes hash?
  - SDM: not done yet.  One problem is that we don't know the hash until the package is built, but we need to know the install locations earlier because we bake them into `Paths_foo.hs`.
  - Simon and Andres discussed that one option is to let Cabal compute its own hash. However, then we'd have two hashes to deal with. Only using the Cabal-computed hash isn't an option either according to Simon, because apparently GHC's ABI hash computation is non-deterministic, so we might end up with situations where Cabal's hash is stable, but GHC computes an ABI-incompatible version. This is somewhat worrying ...

- Cabal: will the dependency solver work correctly in the presence of multiple package instances?

  - Andres claims it will using the new solver. (There is now no point in updating the old solver, though it'd be technically possible.) A little bit more detail: the modular solver has no concept of shadowing, only of preference. So if several instances are provided by one or more package DBs, they'll all be valid choices.

- ghc-pkg cleanup: remove old/unused instances of packages

  - how can we tell when something is unnecessary? This is actually rather hard because unlike Nix we do not track every random executable that the user compiles.

## Next step: dealing with ways

- Add the "way" to InstalledPackageInfo, include the way in the hash

- GHC: slice the package DB during startup according to the correct way

- Cabal: fix up the dep resolver (kosmikus: anything still needed there?)

- Cabal: ways? (this would be really easy, if we could get more information about installed packages back from ghc-pkg)
