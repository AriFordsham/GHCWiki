# Multi-instance packages


This page is about how to change the package system to allow multiple instances of a package to be installed at the same time.  There are two reasons we want to be able to do this:

- To be able to track the different "ways" in which a package is available: e.g. profiling, dynamic.  At the moment, the package database doesn't track this information, with the result that the user has to reinstall packages with `--enable-profiling` on a trial-and-error basis in order to get profiling support for packages they have already installed.

- To make installing new packages more robust.  When installing a new package, we sometimes need to upgrade packages that are already installed to new versions, which may require recompiling other packages against the new version.  For example, if we have P1 installed, Q1 depends on P (any version), and we need to install R that depends on both P2 and Q1.  We need to build P2, rebuild Q1 against P2, and finally build R against P2 and the new Q1.  We would like to do this without removing P1 or the old Q1 from the package database, because other packages may be depending on the old Q1, and we don't want to break those packages (which is what currently happens with GHC 7.0).

## ToDo list

- ghc-pkg: do not overwrite previous instances in the package DB

- GHC: discard conflicting instances during its shadowing phase

- GHC: allow specifying a package instance in the -package flags

- instances of packages must install in a different location

  - install directory includes hash?

- ghc-pkg cleanup: remove old/unused instances of packages

  - how can we tell when something is unnecessary?

- Add the "way" to InstalledPackageInfo, include the way in the hash

- GHC: slice the package DB during startup according to the correct way

- Cabal: fix up the dep resolver

- Cabal: ways?
