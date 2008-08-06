# Converting from Darcs

## Conversion status


Done:

- `.darcs-boring` -\> `.gitignore`
- `darcs-all`, `push-all` -\> `sync-all`
- `aclocal.m4`


Pending:

- The buildbot scripts
- Deploy post-receive-email ([ http://darcs.haskell.org/ghc.git/hooks/post-receive](http://darcs.haskell.org/ghc.git/hooks/post-receive) and /usr/share/doc/git-core/contrib/hooks/post-receive-email on d.h.o)
- Deploy GitPlugin for Trac ([ http://trac-hacks.org/wiki/GitPlugin](http://trac-hacks.org/wiki/GitPlugin))
- Deploy gitweb ([ http://git.or.cz/gitwiki/InterfacesFrontendsAndTools\#head-1dbe0dba1fdab64e839b2c4acd882446742e572e](http://git.or.cz/gitwiki/InterfacesFrontendsAndTools#head-1dbe0dba1fdab64e839b2c4acd882446742e572e))
- Start a git server on darcs.haskell.org?

## Dependencies on darcs


The following is intended to be a complete list of the things that would need to change if we were to switch away from darcs, in addition to the conversion of the repository itself, which I am assuming can be automatically converted using available tools.


The following code/scripts would need to be adapted or replaced:

- The `darcs-all` script
- The `push-all` script
- The `aclocal.m4` code that attempts to determine the source tree date
- `.darcs-boring`
- The buildbot scripts
- checkin email script: `/home/darcs/bin/commit-messages-split.sh`
- Trac integration (the GHC Trac does not currently integrate with darcs, however)
- darcsweb (use whatever alternative is available)


The following documentation would need to change:

- `README`
- [Building/GettingTheSources](building/getting-the-sources)
- [Building/Windows](building/windows)
- [Building/QuickStart](building/quick-start)
- [Building/Rebuilding](building/rebuilding)
- [Building/RunningNoFib](building/running-no-fib)
- [DarcsRepositories](darcs-repositories) (inc. the sidebar)
- [WorkingConventions](working-conventions)
- [WorkingConventions/Darcs](working-conventions/darcs)
- [WorkingConventions/FixingBugs](working-conventions/fixing-bugs)
- [WorkingConventions/AddingFeatures](working-conventions/adding-features)
- [GettingStarted](getting-started)

- [TestingPatches](testing-patches)
- [BuildBot](build-bot)