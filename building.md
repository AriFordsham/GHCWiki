# Building and Porting GHC


This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.

## Contents


Building GHC

- [Setting up your system for building GHC](building/preparation)
- [Getting the sources](building/getting-the-sources)
- [Just building and installing GHC](building/quick-start)
- [Platforms that GHC currently supports](platforms)


Getting started for developers

- [Getting started with the build system](building/hacking)
- [How do I re-build after updating or changing GHC?](building/rebuilding)
- FAQ: [Solving common problems with building GHC](building/troubleshooting). If anything goes wrong, look here first.


More detailed information about using the build system

- [Using the build system](building/using)
- [Validating changes](testing-patches)
- [Running the GHC test framework](building/running-tests)
- [The NoFib benchmark suite](building/running-no-fib)

- [Building the documentation](building/docs)
- [Installing GHC from a build](building/installing)
- [Unregisterised builds](building/unregisterised)
- [Porting GHC to a new platform](building/porting)

- [Debugging and performance-tuning for GHC](debugging)


More detailed information about how the build system works

- [Overview of files and directories](commentary/source-tree)
- [Architecture of the build system](building/architecture)
- [Modifying the build system](building/modifying)


Platform-specific building instructions:

- [Building under Windows](building/windows)
- [Building under MacOS X](building/mac-osx)
- [Building under Solaris](building/solaris)

## Contributed documentation


Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

- [ProblemsCompilingGhc](problems-compiling-ghc): Hints about building GHC on Windows platforms (somewhat obsolete)
- [SonyPS3](sony-p-s3) : Hints for building on the Sony PS3
- [Code coverage of our testsuite](ghc-coverage)