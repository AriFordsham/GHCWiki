# Debugging


This part of the wiki collects all the information related to debugging GHC: that includes debugging the compiler itself, the libraries, the runtime system, the code generator, or the build system.  We've split up these various scenarios into separate pages:

- [Debugging/Compiler](debugging/compiler): Debugging the compiler itself
- [Debugging/CompiledCode](debugging/compiled-code): Debugging a compiled program, perhaps to find a bug in the code generator or runtime system
- [Debugging/TickyTicky](debugging/ticky-ticky): Ticky-ticky profiling, for performance debugging


If you're trying to debug a build failure, then you probably want to look at

- [Building/Troubleshooting](building/troubleshooting): Fixing common problems in a GHC build
- [Building/Modifying](building/modifying#debugging): Debugging the build system
