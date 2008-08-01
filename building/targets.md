# Useful building workflows


This is intended as a short summary of how to do common tasks.  See also [Building/Using](building/using#standard-targets), although that may not be fully up to date. 

### Build or clean everything


Do these things in the `$(TOP)` directory.

- `make`.  This should make everything: the support utilities, stage1 compiler, libraries, and stage2 compiler.

>
> The build system does not track cross-package dependencies, so it's possible that you could recompile one library, but another dependent library isn't recompiled, and you get link errors.

- `make clean`, `make distclean`: various levels of cleanery.

### Build just the compiler


Do these things in the `$(TOP)/compiler` directory.

- `make rebuild`, `make rebuild stage=2`.  This just builds the stage1 or stage2 compiler respectively.


How to clean selectively?  

### Build libraries


Do these things in `$(TOP)/libraries` directory.

- Build all libraries
- Build just one library
- Clean all libraries
- Clean just one library


Do you do the selective work in `libraries/` or in `libraries/haskell98/` (say)?


Disabling a library you don't want to build.
