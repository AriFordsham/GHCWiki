# Multi-instance packages


This page is about how to change the package system to allow multiple instances of a package to be installed at the same time.  There are two reasons we want to be able to do this:

- To be able to track the different "ways" in which a package is available: e.g. profiling, dynamic.  At the moment, the package database doesn't track this information, with the result that the user has to reinstall packages with `--enable-profiling` on a trial-and-error basis in order to get profiling support for packages they have already installed.
  The same holds, in principle, for different flag settings or other configuration variations of a package.

- To make installing new packages more robust.  When installing a new package, we sometimes need to upgrade packages that are already installed to new versions, which may require recompiling other packages against the new version.  For example, if we have P1 installed, Q1 depends on P (any version), and we need to install R that depends on both P2 and Q1.  We need to build P2, rebuild Q1 against P2, and finally build R against P2 and the new Q1.  We would like to do this without removing P1 or the old Q1 from the package database, because other packages may be depending on the old Q1, and we don't want to break those packages (which is what currently happens with GHC 7.0).


See also

- [Commentary pages about packages](commentary/packages)
- Philipp Schuster's GSoC project [proposal (DEAD)](http://www.google-melange.com/gsoc/proposal/review/google/gsoc2012/phischu/1), [ GSoC project page (DEAD)](http://www.google-melange.com/gsoc/project/google/gsoc2012/phischu/19001),  [Trac wiki page](commentary/g-so-c-multiple-instances), [ git repo](https://github.com/phischu/cabal), and [ video](https://www.youtube.com/watch?v=h4QmkyN28Qs).
- [Mikhail's post](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html) about Cabal sandboxes. 
- Mailing list stuff [here](http://comments.gmane.org/gmane.comp.lang.haskell.ghc.devel/443) and [ here](http://markmail.org/message/4qvegvx32lhlo66g#query:+page:1+mid:bwdgykv4g2hzqg5t+state:results).

## ToDo list

- ghc-pkg: do not overwrite previous instances in the package DB

  - but we need to think about the case where we overwrite an existing package on the file system and re-register. This will happen with local (or in-place) package registration that occurs when building a bunch of related components. In this case the tool should know it's doing that and unregister the old instance first (though reliably tracking that state may be tricky, since users can make clean etc). We should check make it a checked error to re-register in the same filesystem location with new package id, without unregistering the old one first. Perhaps we can identify some key file.

- GHC: discard conflicting instances during its shadowing phase

  - SDM: GHC will currently do \*something\* here, but it might end up with a result that the user didn't want/expect.  One way to improve things is to prioritise packages that were installed more recently.
  - Andres suggests that GHC should be much cleverer, and look at the actual dependencies of the modules being compiled before deciding which packages to enable.  This would almost certainly result in more things working and possibly less surprising behaviour sometimes, but Simon thinks that (a) it is too hard, (b) if users need this, they should use Cabal and its dependency resolver, which will do a good job, (c) you can often resolve problems by adding `-package X`, and (d) eventually we will want a system where users manage separate sessions, so they can set up an environment in which the packages they want are available.  This has a lot in common with `cabal-dev` and sandboxes, so the mechanisms (and concepts) should be shared. (kosmikus: perhaps an alternative is to force the user to make an active decision in case of conflicts, i.e., to create a sandbox that exposes a consistent package set).

- GHC: allow specifying a package instance in the -package flags

  - SDM: already done (-package-id flag)
  - DC: already used by Cabal

- Cabal: allow specifying a package instance when doing Setup.hs configure

  - DC: currently only == version constraints can be used, not installed package id. Shouldn't be too hard to add however.
  - JT: Done according to DC.

- instances of packages must install in a different location

  - install directory includes hash?
  - SDM: not done yet.  One problem is that we don't know the hash until the package is built, but we need to know the install locations earlier because we bake them into `Paths_foo.hs`.
  - Simon and Andres discussed that one option is to let Cabal compute its own hash. However, then we'd have two hashes to deal with. Only using the Cabal-computed hash isn't an option either according to Simon, because apparently GHC's ABI hash computation is non-deterministic, so we might end up with situations where Cabal's hash is stable, but GHC computes an ABI-incompatible version. This is somewhat worrying ... 
  - Duncan thinks that we should store both a package identity and a package ABI hash. Currently we form the package id from the name, version and ABI hash. We should store the ABI hash separately anyway because eventually we will want to know it, to know which packages are ABI compatible. So Cabal can compute a package Id in advance, however is sensible, and the ABI hash is calculated as now, after the build. The installation directory follows the package Id.

- Cabal: will the dependency solver work correctly in the presence of multiple package instances?

  - Andres claims it will using the new solver. (There is now no point in updating the old solver, though it'd be technically possible.) A little bit more detail: the modular solver has no concept of shadowing, only of preference. So if several instances are provided by one or more package DBs, they'll all be valid choices.

- ghc-pkg cleanup: remove old/unused instances of packages

  - how can we tell when something is unnecessary? This is actually rather hard because unlike Nix we do not track every random executable that the user compiles.

## Next step: dealing with ways

- Add the "way" to InstalledPackageInfo, include the way in the hash

- GHC: slice the package DB during startup according to the correct way

- Cabal: fix up the dep resolver (kosmikus: anything still needed there?)

- Cabal: ways? (this would be really easy, if we could get more information about installed packages back from ghc-pkg)

- To handle flags and other config, add two new fields to InstalledPackageInfo: `install-agent: {agent-id}` which identifies cabal/rpm/etc and then `configuration: {free text}`. The interpretation of the configuration string depends on the installation agent, and need be known only to that agent. This way, agents can see if it was them that installed a package, and so they should know how to interpret the config string. For cabal this would include config flags etc. It should make it possible to reproduce a package, e.g. if we have to rebuild for some reason, or to get the profiling equiv of a normal instance.
