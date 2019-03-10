# GHC Commentary: Libraries


All GHC build trees contain a set of libraries, called the **Boot Packages**.  These are the libraries that GHC's source code imports.  Obviously you need the boot packages to build GHC at all.


The Boot Packages, along with the other subcomponents of the GHC build system, are listed in the file `$(TOP)/packages` in a GHC tree. 


All boot packages have a Git repo in [ http://darcs.haskell.org/packages](http://darcs.haskell.org/packages):

- Having all the repos in one place makes it easy and uniform for GHC developers to get all the packages.

- In a build tree, these packages each occupy a sub-directory of `$(TOP)/libraries`.

- For INDEPENDENT packages (see "Coupling to GHC", below), the Git repo in [ http://darcs.haskell.org/packages](http://darcs.haskell.org/packages) is a **lagging repo**. That means

  - Don't push to it.
  - Update it from the package's master repo at convenient intervals.
    In this way GHC developers are not exposed to package upgrades, except when we want.


To find out which packages are currently zero-boot packages, do the following in a GHC build:

```wiki
$ make show VALUE=PACKAGES
```


(The `PACKAGES` variable is set in `$(TOP)/`[ghc.mk](/trac/ghc/browser/ghc/ghc.mk).)
You can see exactly which versions of what packages GHC depends on by looking in `$(TOP)/`[compiler/ghc.cabal.in](/trac/ghc/browser/ghc/compiler/ghc.cabal.in).

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


For the most part we simply assume that the bootstrap compiler already has the boot packages installed.  The **Zero-boot Packages** are a set of packages for which this assumption does not hold.  For example, for certain fast-moving boot packages (eg Cabal), we don't want to rely on the user having installed a bang-up-to-date version of the package.  


So we begin the entire build process by installing the zero-boot packages in the bootstrap compiler.  (This installation is purely local to the build tree.)  


As time goes on, a Zero-boot package may become an ordinary boot package, because the bootstrap compiler is expected to have (a sufficiently up to date) version of the package already.


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

- At the root of the hierarchy we have **`ghc-prim`**. As the name implies, this package contains the most primitive types and functions. It only contains a handful of modules, including `GHC.Prim` (which contains `Int#`, `+#`, etc) and `GHC.Bool`, containing the `Bool` datatype.

- Above `ghc-prim` is the **`integer-impl`** package, where `impl` is one of `gmp` and `simple`, which provides a definition of the `Integer` type (on top of the C `gmp` library, or in plain Haskell, respectively). Which functionality is provided in `ghc-prim` is mostly driven by what functionality the `integer-impl` packages need. By default `integer-gmp` is used; to use `integer-simple` define `INTEGER_LIBRARY=integer-simple` in `mk/build.mk`.

- Next is the **`base`** package. This contains a large number of modules, many of which are in one big cyclic import knot, mostly due to the `Exception` type.

- On top of base are a number of other, more specialised packages, whose purpose is generally clear from their name. If not, you can get more detail from the descriptions in their Cabal files.  Currently these packages are are:

  - `array`
  - `bytestring`
  - `Cabal`
  - `containers`
  - `directory`
  - `extensible-exceptions`
  - `filepath`
  - `haskeline`
  - `haskell98`
  - `hpc`
  - `mtl`
  - `old-locale`
  - `old-time`
  - `packedstring`
  - `pretty`
  - `process`
  - `random`
  - `syb`
  - `template-haskell`
  - `terminfo`
  - `unix`
  - `utf8-string`
  - `Win32`


The `haskell98`, `old-time` and `random` packages are mostly only needed for Haskell 98 support, although `dph` currently uses `random` too.


Note that `ghc-prim` and `integer-impl` are below the dependency chain from Exception (in `base`), which means they must not raise generate code to raise an exception (it's not enough that this code will never run). One particularly subtle case of GHC exception-raising code is in the case of (complete!) pattern matches. Consider the unboxed form of Integers, which has the constructor S\# or J\#.

```wiki
f (S# _) (S# _) = ...
f x (S# _) = ...
f (S# _) y = ...
f (J# _ _) (J# _ _) = ...
```


GHC will incorrectly generate core that pattern matches against the second argument twice, the second match being a partial one with (dead) exception raising code. When compiled with optimizations, the dead code is eliminated. However, this breaks with -O0, so the fix was to explicitly spell out the constructor in the second and third line:

```wiki
f (S# _) (S# _) = ...
f (J# _ _) (S# _) = ...
f (S# _) (J# _ _) = ...
f (J# _ _) (J# _ _) = ...
```

# Repository locations


Many of the libraries in a GHC tree are actually maintained by someone else. They therefore have a separate upstream repository, from which we need to pull. That repository may be either a darcs or a git repository; in the darcs case, we also need to convert to a git repository for use in a GHC tree. This diagram shows how changes migrate from one repo to another:

[](/trac/ghc/attachment/wiki/Commentary/Libraries/repos.png)


This means that when making changes needed in GHC to one of these libraries, we first need to put the changes in the upstream repository. For example, to make a change to Cabal:

- First push the change as a darcs patch to the upstream Cabal repository, [ http://code.haskell.org/Cabal/](http://code.haskell.org/Cabal/)
- The patch will be mirrored and converted to git by the mirror script, in the repo [ http://darcs.haskell.org/git-mirrors/Cabal/.git/](http://darcs.haskell.org/git-mirrors/Cabal/.git/)
- You then need to pull from [ http://darcs.haskell.org/git-mirrors/Cabal/.git/](http://darcs.haskell.org/git-mirrors/Cabal/.git/) into `libraries/Cabal` in a regular GHC tree, validate, and push to the GHC Cabal repo, [ http://darcs.haskell.org/packages/Cabal.git/](http://darcs.haskell.org/packages/Cabal.git/)

This table shows, for each repository in a GHC tree, where the central repository is, and what mirrors there are.

<table><tr><th>darcs</th>
<th>git</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>darcs upstream</th>
<th>darcs mirror</th>
<th>git upstream</th>
<th>git mirror</th>
<th>ghc (validated) repo</th>
<th>in-tree</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/ghc.git/</th>
<th></th>
<th>http://darcs.haskell.org/ghc.git/</th>
<th>.</th>
<th>ghc</th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/ghc-tarballs.git/</th>
<th></th>
<th>http://darcs.haskell.org/ghc-tarballs.git/</th>
<th>ghc-tarballs</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/utils/hsc2hs.git/</th>
<th></th>
<th>http://darcs.haskell.org/utils/hsc2hs.git/</th>
<th>utils/hsc2hs</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/haddock.git</th>
<th></th>
<th>http://darcs.haskell.org/haddock.git</th>
<th>utils/haddock</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/array.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/array.git/</th>
<th>libraries/array</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/base.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/base.git/</th>
<th>libraries/base</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/kolmodin/binary.git</th>
<th>http://darcs.haskell.org/git-mirrors/binary/binary.git/</th>
<th>http://darcs.haskell.org/packages/.git/</th>
<th>libraries/binary</th>
<th></th></tr>
<tr><th>http://code.haskell.org/bytestring/</th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/bytestring/.git/</th>
<th>http://darcs.haskell.org/packages/bytestring.git/</th>
<th>libraries/bytestring</th>
<th></th></tr>
<tr><th>http://code.haskell.org/Cabal/</th>
<th></th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/Cabal/.git/</th>
<th>http://darcs.haskell.org/packages/Cabal.git/</th>
<th>libraries/Cabal</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://github.com/haskell/containers.git</th>
<th>http://darcs.haskell.org/git-mirrors/containers/.git/</th>
<th>http://darcs.haskell.org/packages/containers.git/</th>
<th>libraries/containers</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/directory.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/directory.git/</th>
<th>libraries/directory</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/extensible-exceptions.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/extensible-exceptions.git/</th>
<th>libraries/extensible-exceptions</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/filepath.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/filepath.git/</th>
<th>libraries/filepath</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/ghc-prim.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/ghc-prim.git/</th>
<th>libraries/ghc-prim</th>
<th></th></tr>
<tr><th>http://code.haskell.org/haskeline/</th>
<th>http://darcs.haskell.org/darcs-mirrors/haskeline/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/haskeline/.git/</th>
<th>http://darcs.haskell.org/packages/haskeline.git/</th>
<th>libraries/haskeline</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell98.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell98.git/</th>
<th>libraries/haskell98</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell2010.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/haskell2010.git/</th>
<th>libraries/haskell2010</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>git://code.eecs.tufts.edu/hoopl/hoopl.git</th>
<th>http://darcs.haskell.org/git-mirrors/hoopl/</th>
<th>http://darcs.haskell.org/packages/hoopl.git/</th>
<th>libraries/hoopl</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/hpc.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/hpc.git/</th>
<th>libraries/hpc</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-gmp.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-gmp.git/</th>
<th>libraries/integer-gmp</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-simple.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/integer-simple.git/</th>
<th>libraries/integer-simple</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/mtl.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/mtl.git/</th>
<th>libraries/mtl</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/old-locale.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/old-locale.git/</th>
<th>libraries/old-locale</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/old-time.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/old-time.git/</th>
<th>libraries/old-time</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/pretty.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/pretty.git/</th>
<th>libraries/pretty</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/process.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/process.git/</th>
<th>libraries/process</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th>
<th>git://github.com/rrnewton/haskell_stdlib_random.git</th>
<th>http://darcs.haskell.org/packages/random.git/</th>
<th>libraries/random</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/template-haskell.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/template-haskell.git/</th>
<th>libraries/template-haskell</th>
<th></th></tr>
<tr><th>http://code.haskell.org/terminfo/</th>
<th>http://darcs.haskell.org/darcs-mirrors/terminfo/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/terminfo/.git/</th>
<th>http://darcs.haskell.org/packages/terminfo.git/</th>
<th>libraries/terminfo</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/unix.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/unix.git/</th>
<th>libraries/unix</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>https://github.com/glguy/utf8-string.git</th>
<th></th>
<th>http://darcs.haskell.org/packages/utf8-string.git/</th>
<th>libraries/utf8-string</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/Win32.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/Win32.git/</th>
<th>libraries/Win32</th>
<th></th></tr>
<tr><th>http://darcs.haskell.org/packages/xhtml/</th>
<th>http://darcs.haskell.org/darcs-mirrors/xhtml/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/xhtml/.git/</th>
<th>http://darcs.haskell.org/packages/xhtml.git/</th>
<th>libraries/xhtml</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/testsuite.git/</th>
<th></th>
<th>http://darcs.haskell.org/testsuite.git/</th>
<th>testsuite</th>
<th>testsuite</th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/nofib.git</th>
<th></th>
<th>http://darcs.haskell.org/nofib.git</th>
<th>nofib</th>
<th>nofib</th></tr>
<tr><th>http://code.haskell.org/primitive/</th>
<th>http://darcs.haskell.org/darcs-mirrors/primitive/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/primitive/.git/</th>
<th>http://darcs.haskell.org/packages/primitive.git/</th>
<th>libraries/primitive</th>
<th>dph</th></tr>
<tr><th>http://code.haskell.org/vector/</th>
<th>http://darcs.haskell.org/darcs-mirrors/vector/</th>
<th></th>
<th>http://darcs.haskell.org/git-mirrors/vector/.git/</th>
<th>http://darcs.haskell.org/packages/vector.git/</th>
<th>libraries/vector</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/dph.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/dph.git/</th>
<th>libraries/dph</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/deepseq.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/deepseq.git/</th>
<th>libraries/deepseq</th>
<th>extra</th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/parallel.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/parallel.git/</th>
<th>libraries/parallel</th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>http://darcs.haskell.org/packages/stm.git/</th>
<th></th>
<th>http://darcs.haskell.org/packages/stm.git/</th>
<th>libraries/stm</th>
<th></th></tr></table>