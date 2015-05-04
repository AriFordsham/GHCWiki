
In GHC 7.10, we changed the internal representation of names to be based on package keys (`base_XXXXXX`) rather than package IDs (`base-4.7.0.1`), however, we forgot to update the Template Haskell API to track these changes.  This lead to some bugs in TH code which was synthesizing names by using package name and version directly, e.g. [\#10279](https://gitlab.haskell.org//ghc/ghc/issues/10279)


We now propose the following changes to the TH API in order to track these changes:

## Rename PkgName to PkgKey


Currently, a TH `NameG` contains a `PkgName`, defined as:

```wiki
newtype PkgName = PkgName String
```


This is badly misleading, even in the old world order, since these needed version numbers as well (except for wired-in packages, which did not have a version number). We propose that this be renamed to `PkgKey`:

```wiki
newtype PkgKey = PkgKey String
mkPackageKey :: String -> PackageKey
mkPackageKey = PkgKey
```

## Querying about packages


Package keys are somewhat hard to synthesize, so we also offer an API for querying the package database of the GHC which is compiling your code for information about packages.  So, we introduce a new abstract data type:

```wiki
data Package
packageKey :: Package -> PkgKey
```


and some functions for getting packages:

```wiki
searchPackage :: String -- Package name
              -> String -- Version
              -> Q [Package]

reifyPackage :: PkgKey -> Q Package
```


We could add other functions (e.g., return all packages with a package name).

## Retrieving the current package key


Commonly, a user wants to get the package key of the current package.  Following Simon's suggestion, this will be done by augmenting `ModuleInfo`:

```wiki
 data ModuleInfo =
     ModuleInfo { mi_this_mod :: Module -- new
                , mi_imports :: [Module] }
```


We'll also add a function for accessing the module package key:

```wiki
modulePackageKey :: Module -> PkgKey
```


And a convenience function for accessing the current module:

```wiki
 thisPackageKey :: Q PkgKey
 thisPackageKey = fmap (modulePackageKey . mi_this_mod) qReifyModule

 thisPackage :: Q Package
 thisPackage = reifyPackage =<< thisPackageKey
```