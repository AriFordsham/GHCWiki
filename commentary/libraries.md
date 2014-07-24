# GHC Commentary: Libraries


All GHC build trees contain a set of libraries, called the **Boot Packages**.  These are the libraries that GHC's source code imports.  Obviously you need the boot packages to build GHC at all.  The boot packages are those packages in the file [packages](/trac/ghc/browser/packages)[](/trac/ghc/export/HEAD/ghc/packages) that have a `-` in the "tag" column.


The repository structure of a GHC source tree is described in [Repositories](repositories).


You can see exactly which versions of what packages GHC depends on by looking in [compiler/ghc.cabal.in](/trac/ghc/browser/compiler/ghc.cabal.in)[](/trac/ghc/export/HEAD/ghc/compiler/ghc.cabal.in).

# Building packages that GHC doesn't depend on


You can make the build system build extra packages, on which GHC doesn't strictly depend, by adding them to the `$(TOP)/packages` file, with an `extra` tag.


It should be exceptional, but you can make the build system provide per-package compiler flags, by adding some definitions in `$(TOP)/ghc.mk`, just below the comment

```wiki
# Per-package compiler flags
# 
# If you want to add per-package compiler flags, this 
# is the place to do it.  Do it like this for package <pkg>
#   
#   libraries/<pkg>_dist-boot_HC_OPTS += -Wwarn
#   libraries/<pkg>_dist-install_HC_OPTS += -Wwarn
```

---

# Classifying boot packages


Boot packages can be classified in three different ways:

- Independent/Coupled/Specific
- Zero-boot/not zero-boot
- Installed/not installed


These distinctions are described in the following sub-sections.

## Coupling to GHC


An important classification of the boot packages is as follows:

- **SPECIFIC**: Totally specific to GHC.  At the moment these are:

  - ghc-prim
  - template-haskell
  - DPH

- **COUPLED**: Tightly coupled to GHC.  At the moment there is just one of these:

  - base

- **INDEPENDENT**: Independently maintained.  There are quite a few of these, such as `containers`, `binary`, `haskeline` and so on.  Indeed most boot libraries are INDEPENDENT.  


INDEPENDENT libraries have a master repository somewhere separate from the GHC repositories.  Whenever we release GHC, we ensure that the INDEPENDENT boot libraries that come with GHC are precisely sync'd with a particular released version of that library.

## Zero-boot packages


Since GHC's source code imports the boot packages, *even the bootstrap compiler must have the boot packages available*.  (Or, more precisely, all the types and values that are imported must be available from some package in the bootstrap compiler; the exact set of packages does not need to be identical.)


For the most part we simply assume that the bootstrap compiler already has the boot packages installed.  The **Zero-boot Packages** are a set of packages for which this assumption does not hold. Two reasons dominate:

- For certain fast-moving boot packages (e.g. `cabal`), we don't want to rely on the user having installed a bang-up-to-date version of the package.
- The only packages that we can "assume that the bootstrap compiler already has" are those packages that come with GHC itself; i.e. the installed boot packages.  So non-installed boot packages are also zero-boot packages.  Example: `bin-package-db` or `hoopl`.


So we begin the entire build process by installing the zero-boot packages in the bootstrap compiler.  (This installation is purely local to the build tree.)  


As time goes on, a Zero-boot package may become an ordinary boot package, because the bootstrap compiler is expected to have (a sufficiently up to date) version of the package already.  Remember that we support bootstrapping with two previous versions of GHC.


To find out which packages are currently zero-boot packages, do the following in a GHC build:

```wiki
$ make show VALUE=BOOT_PKGS
```


Some Zero-boot packages are **maintained by other people**. In order to avoid GHC being exposed to day-by-day changes in these packages, we maintain a "lagging" Git repository for each that we occasionally sync with the master repository.  We never push patches to lagging repository; rather we push to the master (in discussion with the package maintainer), and pull the patches into the lagging repo.  The current Zero-boot packages of this kind are:

- `Cabal`: we frequently update Cabal and GHC in sync
- `binary` (renamed to `ghc-binary` in the 6.12 branch): required by `bin-package-db`.


Other Zero-boot packages are **maintained by us**.  There is just one Git repo for each, the master.  When we make a GHC release, we simultaneously tag and release each of these packages.  They are:

- `hpc`
- `extensible-exceptions`: this is a shim that provides an API to older versions of GHC that is compatible with what the current `base` package now exports.  So, unusually, `extensible-exceptions` is a zero-boot package, but not a boot package.
- `bin-package-db`: a GHC-specific package that provides binary serialisation of the package database, use by `ghc-pkg` and GHC itself.

## Installation


When we build a distribution of GHC, it includes at least some libraries, otherwise it would be utterly useless.  Since GHC is part of the Haskell Platform, any library that is installed with GHC is necessarily part of the Haskell Platform, so we have to be a bit careful what we include.  


Alas, since the `ghc` package (implementing the GHC API) is certainly an installed package, all the packages on which it depends must also be installed, and hence willy-nilly become part of the Haskell Platform.  In practice that means that almost all the Boot Packages are installed.  In some cases that is unfortunate.  For example, we currently have a special version of the `binary` library, which we don't really expect Haskell users to use; in this case, we call it `ghc-binary`, and informally discourage its use.


Currently the Boot Packages that are not installed are `haskelline`, `mtl`, and `terminfo`; these are needed to build the GHC front-end, but not to build the `ghc`*package*.

---

# Boot packages dependencies

- At the root of the hierarchy we have **`ghc-prim`**. As the name implies, this package contains the most primitive types and functions. It only contains a handful of modules, including `GHC.Prim` (which contains `Int#`, `+#`, etc) and `GHC.Bool`, containing the `Bool` datatype.  See "WARNING: pattern matching" below.

- Above `ghc-prim` are the packages

  - `integer-gmp`
  - `integer-simple`

>
> The two have the same interface, and only one of the two is used. (When we want to be vague about which one, we call it `integer-impl`.)  They provide a definition of the `Integer` type (on top of the C `gmp` library, or in plain Haskell, respectively). Which functionality is provided in `ghc-prim` is mostly driven by what functionality the `integer-impl` packages need. By default `integer-gmp` is used; to use `integer-simple` define `INTEGER_LIBRARY=integer-simple` in `mk/build.mk`.

> >
> > See "WARNING: pattern matching" below.

- Next is the **`base`** package. This contains a large number of modules, many of which are in one big cyclic import knot, mostly due to the `Exception` type.

- On top of base are a number of other, more specialised packages, whose purpose is generally clear from their name. If not, you can get more detail from the descriptions in their Cabal files.  The up-to-date list of packages can be found in the file [packages](/trac/ghc/browser/packages)[](/trac/ghc/export/HEAD/ghc/packages).


The `haskell98`, `old-time`, `old-locale` and `random` packages are mostly only needed for Haskell 98 support, although `dph` currently uses `random` too.

## WARNING: Pattern matching in `ghc-prim`, `integer-simple`, and `integer-gmp`


Note that `ghc-prim` and `integer-impl` are below the dependency chain from Exception (in `base`), which means they must not raise generate code to raise an exception (it's not enough that this code will never run). One particularly subtle case of GHC exception-raising code is in the case of (complete!) pattern matches. Consider the unboxed form of Integers, which has the constructor S\# or J\#.

```wiki
f (S# _) (S# _) = ...
f x (S# _) = ...
f (S# _) y = ...
f (J# _ _) (J# _ _) = ...
```


GHC will incorrectly generate core that pattern matches against the second argument twice, the second match being a partial one with (dead) exception raising code. When compiled with optimizations, the dead code is eliminated. However, this breaks with -O0, thus:

```wiki
Loading package integer-simple ... linking ... 
ghc: /usr/local/ghc/7.2.0.20110728/lib/ghc-7.2.0.20110728/integer-simple-0.1.0.0/HSinteger-simple-0.1.0.0.o: 
unknown symbol `base_ControlziExceptionziBase_patError_info'
```


The fix is to explicitly spell out the constructor in the second and third line, so that GHC does not generate calls to `patError`:

```wiki
f (S# _) (S# _) = ...
f (J# _ _) (S# _) = ...
f (S# _) (J# _ _) = ...
f (J# _ _) (J# _ _) = ...
```

# Repositories


The list of repository locations has moved to [Repositories](repositories).
