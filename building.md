# Building and Porting GHC


This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.


The documentation here will eventually replace the
[Building Guide](http://www.haskell.org/ghc/docs/latest/html/building/index.html).  Text
from that manual is gradually being incorporated here.

## Contents

- [Getting the sources](building/getting-the-sources)
- [Platforms that GHC currently supports](platforms)
- [What tools you need](building/prerequisites)
- [Quick start: just building and installing GHC](building/quick-start)
- [Quick start for developers](building/hacking)
- [Unregisterised builds](building/unregisterised)
- [How do re-build after updating or changing GHC?](building/rebuilding)
- [Using the build system](building/using)
- Architecture of the build system?
- [Building the documentation](building/docs)
- [Porting GHC](building/porting)
- [Known pitfalls in building GHC](building/known-problems)
- [Running the GHC test framework](building/running-tests)
- [Running the nofib benchmarks](building/running-no-fib)
- Platform-specific build issues

  - [Platforms, scripts, and file names](building/platforms-scripts-file-names)
  - [Building under Windows](building/windows)
  - [Building under MacOS X](building/mac-osx)

## Contributed documentation


Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

- [ProblemsCompilingGhc](problems-compiling-ghc): Hints about building GHC on Windows platforms  

## OLD documentation

- [Building Guide](http://www.haskell.org/ghc/docs/latest/html/building/index.html)