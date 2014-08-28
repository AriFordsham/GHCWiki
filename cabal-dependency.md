
This page describes how GHC depends on and makes use of Cabal.

# General


GHC uses Cabal in a few ways

- GHC ships with the Cabal library pre-installed. This is as a convenience to users, and as asked for in the original Cabal specification.
- The GHC build system makes use of the Cabal library. See [Building/Architecture/Idiom/Cabal](building/architecture/idiom/cabal)
- The representation for installed packages in the installed package database manipulated by ghc-pkg is conceptually defined by the Cabal specification, and in practice defined by the Cabal library (with types, parsers etc).
- The ghc-pkg program depends on the Cabal library for the types, parsers etc of installed package information.
- The bin-package-db library defines a binary serialization format for the package database read by GHC.
- Historically the GHC library also depended on Cabal (both directly, and indirectly through bin-package-db) for the types of installed packages (for its in-memory representation of the package database). This is no longer the case.

# Removal of the GHC library dependency on the Cabal library


See ticket [\#8244](https://gitlab.haskell.org//ghc/ghc/issues/8244)


The GHC library used to depend on the Cabal library directly, for the representation of installed packages. This was convenient for implementation but had a number of drawbacks:

- Any package making use of the GHC library would be forced to use the same version of Cabal as GHC used. This was annoying because while the parts of Cabal that GHC used were not very fast moving, other parts of the library are, and so other packages did want to use a different version of Cabal.
- Given the existing limitations and inconveniences of installing multiple versions of the same package, the GHC dependency on Cabal made it hard to upgrade Cabal separately. Of course this is really more of a limitation of the packaging side of things.
- The fact that GHC depended directly on Cabal placed limitations on the implementation of Cabal. GHC must be very careful about which packages it needs to be able to build (so called boot packages). Because Cabal was a boot package, it could itself only depend on other boot packages. In particular, Cabal needs a decent parser combinator library, but no such library is available as a boot package (and GHC developers were understandably reluctant to add dependencies on parsec, mtl, text etc as would be required).

## Design of GHC-library's non-dependency on Cabal


Under the new approach, we have the following dependency structure for Cabal, ghc-pkg, GHC and bin-package-db:

```wiki

  +--------------+  +------------+  +-----------+
  |    cabal     |  |  ghc-pkg   |  |   GHC     |
  |  executable  |  | executable |  | executable|
  +---+----------+  +------------+  +-+---------+
          |                           |
          |                     +-----v-------+
     .....|...>                 |     GHC     |
     .    |                     |   package   |
     .    |      |      |       +-+-----------+
     .    |      |      |         |
     .  +-v------v-+  +-v---------v----+
     .  |   Cabal  |  | bin-package-db |
     .  |  package |  |    package     |
     .  +----------+  +----------------+
     .     .
     .......
     executes (an "up-dependency")
```


Cabal has a `InstalledPackageInfo` type, defined in the Cabal package, which defines a representation for installed packages as per the Cabal specification; however, now `bin-package-db` defines a new variant of the type which contains \*only\* the fields that GHC relies on. (Call this GHC's type.) ghc-pkg depends on both Cabal and bin-package-db, and is responsible for converting Cabal's types to GHC's types, as well as writing these contents to a binary database, as before. (Cabal invokes ghc-pkg in order to register packages in the installed package database, and as before doesn't directly know about this format.)


Now that there are two types for installed packages, what is the format of the database that bin-package-db writes? The ghc-pkg tool (as required by the Cabal spec) must consume, and regurgitate package descriptions in an external representation defined by the Cabal spec. Thus, the binary package database must contain all the information as per \*Cabal's\* type; better yet, it would be best if we directly used Cabal's library for this (so that we don't have to keep two representations in sync). However, doing this directly is a bit troublesome for GHC, which doesn't want to know anything about Cabal's types, and only wants its subset of the installed package info (GHC's type).

TODO(ezyang): I don't understand why Cabal's type needs to be put in the binary format


We employ a trick in the binary database to support both cases: it contains all the packages in two different representations, once using Cabal types and once using GHC's types. These are contained in two sections of the package.cache binary file inside each package database directory. One section contains the Cabal representation. This section is read back by ghc-pkg when reading the package database. The other section contains the GHC representation. This section is read by GHC.The length of Cabal's section is explicitly recorded in the file, so GHC does not need to know anything about the internal contents of the other section to be able to read its own section. The ghc-pkg tool knows about the representation of both sections and writes both.


Notes

- Cabal only reads/writes the binary package db via the `ghc-pkg` executable.
- GHC reads the binary package db, via `bin-package-db` library.

## Technical details


The ghc-pkg file format is defined by a library shared between GHC and ghc-pkg. It defines GHC package type and provides functions to read each section, and to write both sections:

```wiki
data InstalledPackageInfo   -- same name as used by Cabal, but simpler type

readPackageDbForGhc :: FilePath -> IO [InstalledPackageInfo]

readPackageDbForGhcPkg :: Binary pkgs => FilePath -> IO pkgs

writePackageDb :: FilePath -> [InstalledPackageInfo] -> pkgs -> IO ()
```


Note here that the concrete type of ghc-pkg's representation of packages is not fixed, it simply has to be an instance of `Binary`. This trick means this library does not have to depend on Cabal (which is vital because GHC depends on it), but allows ghc-pkg to instantiate using Cabal's types.


The above types are a slight simplification, the `InstalledPackageInfo` is actually has a number of type parameters, which are used in the fields, e.g.:

```wiki
data InstalledPackageInfo instpkgid srcpkgid srcpkgname pkgkey modulename
   = InstalledPackageInfo {
       ...
       depends            :: [instpkgid],
       ...
       exposedModules     :: [modulename],
       ...
     }
```


The reason for this is purely technical: GHC would prefer `InstalledPackageInfo` to contain types like `ModuleName`, which are defined inside GHC proper; but we do not want to move their definition into `bin-package-db`. (Indeed, some of these types are based on exotic base types like `FastString` which cannot be so easily extricated.)


So instead, we keep the type generic. Inside GHC we specialise this type like so

```wiki
type PackageConfig = InstalledPackageInfo
                       InstalledPackageId
                       SourcePackageId
                       PackageName
                       Module.PackageKey
                       Module.ModuleName
```


However, in ghc-pkg, where we don't have access to any of these types, we just instantiate it with `String` for all parameters. How do we convert between the two types? We never do this directly: ghc-pkg (using bin-package-db) serializes `String` into a UTF-8 encoded stream of bytes stored on disk; then when GHC (also using bin-package-db) reads out the bytes on disk, it deserializes them into its desired data types. This is managed using a little type class:

```wiki
class BinaryStringRep a where
  fromStringRep :: BS.ByteString -> a
  toStringRep   :: a -> BS.ByteString
```


Where there are instances for `FastString`, `String`, etc. The `readPackageDbForGhc` above is then actually:

```wiki
readPackageDbForGhc :: (BinaryStringRep a, BinaryStringRep b, BinaryStringRep c,
                        BinaryStringRep d, BinaryStringRep e) =>
                       FilePath -> IO [InstalledPackageInfo a b c d e]
```


It uses the class to convert to/from the on disk UTF8 representation, and the internal representation (`String` for ghc-pkg, and things like newtype'd `FastString`s in GHC).
