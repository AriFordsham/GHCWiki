# Building and Porting GHC


This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.

## Contents


Building GHC:

- [Setting up your system for building GHC](building/preparation)
- [Getting the sources](building/getting-the-sources)
- [Just building and installing GHC](building/quick-start)
- [Platforms that GHC currently supports](platforms)
- [The GHC Builder, which coordinates distributed nightly builds](builder)


Getting started for developers:

- [Getting started with the build system](building/hacking)
- [How do I re-build after updating or changing GHC?](building/rebuilding)
- [Working Conventions (Git workflows, bug tracker, coding style etc.) ](working-conventions)


More detailed information about using the build system:

- [Using the build system](building/using)
- [Standard targets](building/standard-targets)
- [Making GHC's source code searchable with Hoogle](building/hoogle)
- [Building the documentation](building/docs)
- [Installing GHC from a build](building/installing)
- [Unregisterised builds](building/unregisterised)


Platform-specific building notes (see also [Setting up your system for building GHC](building/preparation))

- [Building under MacOS X](building/mac-osx)
- [Building under Solaris](building/solaris)


How to test and benchmark changes to GHC:

- [Validating changes](testing-patches)
- [Running the GHC test framework](building/running-tests)
- [The NoFib benchmark suite](building/running-no-fib)
- [Testing GHC against all of Hackage](hackage-testing)
- [Debugging and performance-tuning for GHC](debugging)
- [Installing extra packages for your in-place GHC](debugging/installing-packages-inplace)


More detailed information about how the build system works:

- [Overview of files and directories](commentary/source-tree)
- [Architecture of the build system](building/architecture)
- [Modifying the build system](building/modifying)
- [The libraries on which GHC depends](commentary/libraries)


Porting GHC

- [Cross-compilation (the NEW way to port GHC)](cross-compilation)
- [Porting GHC to a new platform (the OLD way to port GHC)](building/porting)


Troubleshooting:

- [FAQ: Solving common problems with building GHC](building/troubleshooting)

## Contributed documentation


Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

- [SonyPS3](sony-p-s3) : Hints for building on the Sony PS3
