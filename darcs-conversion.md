# Converting from Darcs

## Conversion status =


Done:

- `.darcs-boring` -\> \`.gitignore'

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