# Linking in Haskell: Proposal for a Redesign

## Terms


Unfortunately, in computer industry different names are used for the same thing and linking is no exception. Define the following terms:

- **dynamic library**: DLL (Windows), shared object (ELF, Linux, BSD and other UNIX systems)

## Static, dynamic, and hybrid packages


Some Haskell packages use libraries implemented in other languages (for short: C libraries). Linking against C libraries there are three cases:

1. both dynamic
1. both static
1. Haskell static, C dynamic. Call this hybrid.

## Linking with GHC Design

### Both dynamic


Linking against the Haskell package is sufficient. The user of a Haskell package `foo` does not need to know which C libraries and other Haskell packages package `foo` depends on and there is not need to link against those libraries.

### Both static


The linker must be passed the transitive closure of all dependent C libraries and Haskell libraries.

### Hybrid


All Haskell libraries and all directly dependent C libraries must be passed to the linker. Currently, we cannot tell from the package information whether a C library is a direct dependency or a dependency of another C library.

### Conclusion

TODO
Linking the transitive closure as in the static case works for the other cases too. Is it worth the bother to treat those cases differently?

## GHCi with System Runtime Linker and Loader (RTLD)


An object file must be compiled as position independent code (`-fPIC`). Then a temporary dynamic library is produced by `ld` with no other inputs. Undefined symbols are ignored.


Link all temporary dynamic libraries and all packages loaded and all command line libraries into one large dummy dynamic library. The order on the link command line must be observed so it is possible to override symbols defined in a library loaded earlier.


Should it be possible to override a symbol defined in a Haskell package?


Do we need to support static libraries in a dynamic GHCi?


Theoretically, in ELF there should be no difference between a statically linked and a dynamically linked GHCi.

## GHCi with Haskell runtime linker (RTS)

TODO

## Cabal support?

TODO