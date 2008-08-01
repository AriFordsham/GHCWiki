# Building and Porting GHC


This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.

## Contents


Quick starts

- [Getting the sources](building/getting-the-sources)
- [Just building and installing GHC](building/quick-start)
- [Layout of important files and directories](building/files) (new)
- [Useful workflows and makefile targets](building/targets) (new)
- [General FAQ for building GHC](building/faq)


More detailed information about using the system

- [What tools you need](building/prerequisites)
- [Quick start for developers](building/hacking)
- [How do I re-build after updating or changing GHC?](building/rebuilding)
- [Building the documentation](building/docs)
- [Unregisterised builds](building/unregisterised)
- [Porting GHC](building/porting)
- [Running the GHC test framework](building/running-tests)
- [The NoFib benchmark suite](building/running-no-fib)


How the build system works

- [Using the build system](building/using)
- Architecture of the build system?


Platform-specific guidance

- [Platforms that GHC currently supports](platforms)
- [Platforms, scripts, and file names](building/platforms-scripts-file-names)
- [Building under Windows](building/windows)
- [Building under MacOS X](building/mac-osx)

## Contributed documentation


Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

- [ProblemsCompilingGhc](problems-compiling-ghc): Hints about building GHC on Windows platforms  
- [SonyPS3](sony-p-s3) : Hints for building on the Sony PS3
- [Code coverage of our testsuite](ghc-coverage)