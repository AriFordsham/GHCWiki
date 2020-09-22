We would like to make the RTS reinstallable like any other Haskell package.

## Issue 1

GHC has to know about field offsets and struct sizes of some C types defined in RTS headers (.h). These are not wired in the compiler because they depend on the toolchain used to build the RTS (C compiler, etc.).

What is done instead is that utils/deriveConstants is a tool that generates C codes using these headers, that builds them as objects (.o) using the same toolchain (given as command-line parameters), that analyzes these objects using objdump/nm (also given as command-line parameters). Finally it generates 3 files:

* DerivedConstants.h: define constants, imported by Cmm code (via Cmm.h). Bundled in rts package.
* compiler/GHC/Platform/Constants.hs: define "PlatformConstants" datatype (with a Read instance), used in the compiler.
* platformConstants: PlatformConstants value (that can be read via PlatformConstants' Read instance). Installed in GHC's libdir and read at runtime.

## Issue 2

Other generated headers in the RTS:
* ghcautoconf.h
* ghcplatform.h
* ghcversion.h
* ffi.h /ffitarget.h headers

These depend on the target platform and on some other things inferred by the top-level "configure".


## What we should do

platformConstants and DerivedConstants.h are almost the same thing. They should live at the same place.

1. Approach 1: I've tried to put platformConstants into the rts package
  * Cabal's "data-files" isn't suitable as GHC doesn't have access to this field
  * we can add platformConstants into the "install-includes" list in rts.cabal, then GHC has access to it via "unitIncludeDirs"
    * but GHC reads packages quite late compared to settings/platform currently. Some refactoring is needed

2. Approach 2: put generated headers into GHC's libdir
  * we will need to have one directly per supported platform anyway (for #14335), so we can implement it now
  * step 1: put package database and platform constants into: $libdir/platforms/default/{platformConstants,package.conf.d} (default target platform)
  * step 2: put generated headers into: $libdir/platforms/default/includes/
  * step 3: make GHC include this directory on gcc invocations