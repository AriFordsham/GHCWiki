# Building and Porting GHC


This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.

## Contents

**Getting started**

- [Setting up your system for building GHC](building/preparation)
- [Getting the sources](building/getting-the-sources)
- [Quick Start to just building and installing GHC](building/quick-start)
- [Quick Start to using the build system as a developer](building/hacking)

**More detailed information about using the build system**

- [Comprehensive overview of using the build system](building/using)
- [Standard targets](building/standard-targets)
- [Making GHC's source code searchable with Hoogle](building/hoogle)
- [Building the documentation](building/docs)
- [Installing GHC from a build](building/installing)
- [Unregisterised builds](building/unregisterised)

**Platform-specific building notes**

- [Platforms that GHC currently supports](platforms)
- [Building under MacOS X](building/mac-osx)
- [Building under Solaris](building/solaris)
- [Building under FreeBSD](building/preparation/free-bsd)
- [Building under Windows](windows-ghc)

**How to test and benchmark changes to GHC**

- [Validating changes](testing-patches)
- [Running the GHC test framework](building/running-tests)
- [The NoFib benchmark suite](building/running-no-fib)
- [Testing GHC against all of Hackage](hackage-testing)
- [Debugging and performance-tuning for GHC](debugging)

**Information about libraries (= packages)**

- [Installing extra libraries for your in-place GHC](debugging/installing-packages-inplace)
- [The libraries on which GHC depends](commentary/libraries)

**More detailed information about how the build system works**

- [Overview of files and directories](commentary/source-tree)
- [Architecture of the build system](building/architecture)
- [Modifying the build system](building/modifying)

**Porting GHC and building cross compilers**

- [Cross-compilation](building/cross-compiling)
- [Compiling for 32 bits on 64 bits](building/compiling32on64)

**Troubleshooting**

- [FAQ: Solving common problems with building GHC](building/troubleshooting)

## Contributed documentation


Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

- [SonyPS3](sony-p-s3) : Hints for building on the Sony PS3
