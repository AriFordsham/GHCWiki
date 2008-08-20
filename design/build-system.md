# Regaining the build system


The basic plan is 

- Use our own build system for the work of actually
  building packages from source to .a/.so, including
  preprocessing (hsc2hs et. al.).

- Use Cabal to preprocess the .cabal file and generate
  metadata in the form of Makefile bindings for our
  build system to use.

- Use Cabal to generate the InstalledPackageInfo and
  do registration.

- Use Cabal for Haddocking, installing, and anything else
  we need to do.


The advantages of this are:

- The critical parts of the build system are under our
  control, and are easily modifiable.

- development is easier, because 'make' will preprocess files
  too.  Right now if you modify a .y or .hsc file, you need
  to tell Cabal to preprocess again before saying 'make'
  (this is a regression from pre-Cabal).

- We can make improvements that would be hard in Cabal, such
  as making libraries depend on each other.

- It ought to be easier to reinstate HC bootstrapping,
  since we rely less on Cabal to get us to a .a file.

- Compared to the pre-Cabal build system, we're not
  duplicating the package metadata or the code that processes it,
  only the build rules.


Here's a more detailed plan:

- Modify cabal-bin.hs with a new command to generate the
  Makefile bindings for a package, into a file e.g. 
  ghc-build.mk

- libraries/Makefile puts a GNUmakefile into each library
  subdir, with identical contents, something like

  ```wiki
  TOP=../..
  -include ghc-build.mk
  include $(TOP)/cabal-package.mk
  ```

- In each subdir we support various make targets, e.g.

  - `make configure`, configures the package and generates ghc-build.mk
  - `make all`, builds the .a, and registers.  Builds dependencies autoamtically (or perhaps not: calculating dependencies
    in GHC takes a while, and traditionally we've done this on demand only).
  - `make install`

- libraries/Makefile just invokes make in the subdirs in the 
  appropriate order.


Improvements for later

- we want dependencies from every object file on the .a files of the
  packages that it depends on.  This way we can make it possible to
  modify a library module and say 'make' and have everything rebuilt 
  that needs to be rebuilt (including the stage2 compiler).  Note that
  we need to know about indirect as well as direct package dependencies.

- build multiple libraries in parallel

- remove the makefile code from Cabal
