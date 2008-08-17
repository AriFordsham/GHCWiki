# Useful workflows and makefile targets


This is intended as a short summary of how to do common tasks.  See also [Building/Using](building/using#standard-targets), although that may not be fully up to date. 

### Build or clean everything


Do these things in the `$(TOP)` directory.

- **Initialisation**: `sh boot; ./configure`, create `build.mk`.  See [the details](building/using#getting-the-build-you-want), and more about [controlling the build](building/hacking).

- **`make`**.  This should make everything: the support utilities, stage1 compiler, libraries, and stage2 compiler.

>
> The build system does not track cross-package dependencies, so it's possible that you could recompile one library, but another dependent library isn't recompiled, and you get link errors.

- **`make clean`**, **`make distclean`**: various levels of cleanery.

### Clean and rebuild just the compiler


Do these things in the `$(TOP)/compiler` directory.

- `make clean stage=2`, `make boot stage=2`, `make stage=2`. This cleans, boots and builds the stage 2 compiler. You can do the same for the other stages. Note the first command is rarely necessary, and you normally don't need the second one either.

### Build libraries


Do these things in `$(TOP)/libraries` directory.

- Build all libraries: `make`
- Build just the `foo` library: `make make.library.foo`. You need to `make remake.library.foo` if you need clean or reconfigure first, e.g. if you changed the module imports.
- Clean all libraries: `make clean`
- Clean just the `foo` library: `make clean.library.foo`


It's not possible to stop the build system from trying to build a boot library, other than fiddling with SUBDIRS in `libraries/Makefile`.

### Testing

- Perform [validation before committing changes](testing-patches)
- Run the [test suite](building/running-tests)
- Run the [nofib suite](building/running-no-fib)