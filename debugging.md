# Debugging


This part of the wiki collects all the information related to debugging GHC: that includes debugging the compiler itself, the libraries, the runtime system, the code generator, or the build system.

- **Reproducing the test case**.  You may need to install some packages to reproduce the test case, and that may take a little care: [Debugging/InstallingPackagesInplace](debugging/installing-packages-inplace).

- **Debugging the compiler itself**.  If you're debugging a compiler panic or some problem in GHC itself, then go to [Debugging/Compiler](debugging/compiler)

- **Debugging a compiled program**. If the compiled program crashes or panics, then go to [Debugging/CompiledCode](debugging/compiled-code)

- **Debugging the runtime system**.  See [Debugging/RuntimeSystem](debugging/runtime-system)

- **Performance debugging**. 

  - [Debugging/ProfilingGhc](debugging/profiling-ghc): Profiling the compiler itself.  
  - [Debugging/TickyTicky](debugging/ticky-ticky): for debugging performance-related issues in compiled code.  Typically for performance debugging of the Simplifier and Core-level optimisations.
  - [Debugging/LowLevelProfiling](debugging/low-level-profiling): way to investigate low-level performance, typically for performance debugging of the code generator or RTS.

- **Build failures**.  If you're trying to debug a build failure, then you probably want to look at

  - [Building/Troubleshooting](building/troubleshooting): Fixing common problems in a GHC build
  - [Building/Modifying](building/modifying#debugging): Debugging the build system
