# Debugging


This part of the wiki collects all the information related to debugging GHC: that includes debugging the compiler itself, the libraries, the runtime system, the code generator, or the build system.

- **Debugging the compiler itself**.  If you're debugging a compiler panic or some problem in GHC itself, then go to [Debugging/Compiler](debugging/compiler)

- **Debugging a compiled program**. If the compiled program crashes or panics, then go to [Debugging/CompiledCode](debugging/compiled-code)

- **Performance debugging**. If you are debugging performance-related issues in compiled code, try [Debugging/TickyTicky](debugging/ticky-ticky).

- **Build failures**.  If you're trying to debug a build failure, then you probably want to look at

  - [Building/Troubleshooting](building/troubleshooting): Fixing common problems in a GHC build
  - [Building/Modifying](building/modifying#debugging): Debugging the build system
