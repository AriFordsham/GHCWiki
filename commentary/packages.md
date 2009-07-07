# Commentary: The Package System

## Architecture


GHC maintains a package database, that is basically a list of `InstalledPackageInfo`.  The `InstalledPackageInfo` type is defined in `Distribution.InstalledPackageInfo` in Cabal, and both `ghc-pkg` and GHC itself import it directly from there.


There are four main components of the package system:

<table><tr><th>Cabal</th>
<td>
Cabal is a Haskell library, which provides basic datatypes for the package system, and support for building,
configuring, and installing packages.
</td></tr></table>

<table><tr><th>GHC itself</th>
<td>
GHC reads the package database(s), understands the flags `-package`, `-hide-package`, etc., and uses the package database
to find `.hi` files and library files for packages.  GHC imports modules from Cabal.
</td></tr></table>

<table><tr><th>`ghc-pkg`</th>
<td>
The `ghc-pkg` tool manages the package database, including registering/unregistering packages, queries, and
checking consistency.  `ghc-pkg ` also imports modules from Cabal.
</td></tr></table>

<table><tr><th>`cabal-install`</th>
<td>
A tool built on top of Cabal, which adds support for downloading packages from Hackage, and building and installing
multiple packages with a single command.
</td></tr></table>


For the purposes of this commentary, we are mostly concerned with GHC and `ghc-pkg`.

## Identifying Packages

<table><tr><th>`PackageName`</th>
<td>
A string, e.g. "base".  Defined in `Distribution.Package`.  Does not uniquely identify a package: the package
database can contain several packages with the same name.
</td></tr></table>

<table><tr><th>`PackageIdentifier`</th>
<td>
A `PackageName` plus a `Version`.  Does uniquely identify a package, but only by convention (we may lift
this restriction in the future).  `InstalledPackageInfo` contains the field `package :: PackageIdentifier`.
</td></tr></table>

<table><tr><th>`InstalledPackageId`</th>
<td>
An opaque string.  Each package is uniquely identified by its `InstalledPackageId`.  Dependencies
between installed packages are also identified by the `InstalledPackageId`.
</td></tr></table>

<table><tr><th>`PackageId`</th>
<td>
Inside GHC, we use the type `PackageId`, which is a `FastString` representation of `InstalledPackageId`.
The (Z-encoding of) `PackageId` prefixes each external symbol in the generated code, so that the modules of one package do
not clash with those of another package, even when the module names overlap.
</td></tr></table>


The tools do not currently support having multiple packages with the same name and version.  When re-installing an existing package, the new package should have a different `InstalledPackageId` from the previous version, even if the `PackageIdentifiers` are the same.  In this way, we can detect when a package is broken because one of its dependencies has been recompiled and re-installed.

## Design constraints

1. We want RecompilationAvoidance to work.  So that means symbol names should not contain any information that varies too often, such as the ABI hash of the module, or the package.

1. We want to be able to compile a package that is compatible with another package; i.e. exports the same ABI.  Right now it isn't possible to do this, but we hope to be able to do it in the future, and we should design the system with that in mind.

1. When a package is recompiled and installed, packages that depended on the old version should now be detectably broken (unless the newly compiled version is really compatible with the old one).


(3) means that dependencies in the package database should mention something unique about a package installation that changes when the package is installed.  However, (1) means that we don't want to put such unique things in symbol names.
