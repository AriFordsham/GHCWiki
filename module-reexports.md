## Module reexports


(Trac’ed as [\#8407](https://gitlab.haskell.org//ghc/ghc/issues/8407)).

### Goal


This proposal aims to introduce re-exports at the package/module level (similar to the symbol re-exporting at the module/symbol level), to make reorganization of the package structure, such as moving a module from package-a to package-b, easier on the users of these packages.

### Motivation


Occasionally, a whole module `Data.Foo` needs to be moved from one package (`package-a`) to another (`package-b`), keeping its name. There are two possibilities:

- The module is provided by both packages (possibly with its symbols re-exported by one of them). Then a user cannot easily (i.e. without [PackageImports](package-imports)) depend on both packages.
- The module is removed in `package-a`. This then requires a major API bump and downstream packages likely have to update their dependencies. Also, if they did not use `package-b` before, they’d have to do that now.


One use-case in particular would be to turn `base` into a pure module-rexporting package, exporting a selection of modules from other packages. In that case, this proposal would allow the other package to provide additional (less commonly used, less stable or internal modules) and those users who need these can build-depend on `base` and the implementing package, and can still use the re-exported modules without further ado.

### Semantics


To combine the best of both possibilities, it will be possible for package-a to explicitly re-export `Data.Foo` from `package-b`. This information is noted in the package data base, and has the following effects

- a module using "`import Data.Foo`", "`import "package-a" Data.Foo`" or "`import "package-b" Data.Foo`" imports the real `package-b:Data.Foo`, if `package-a` or `package-b` is in scope.
- even if both `package-a` and `package-b` are in scope, the compiler does not complain about an ambiguous import.


Furthermore, a re-export can be annotated with a deprecation message. In that case, if `Data.Foo` is imported without package-b being in scope, the warning is printed. It is not printed if the module is imported only via `package-b`, or if both are in scope.

### Syntax


The re-export will need to be given in the `.cabal` file:

```wiki
name: package-a
...

library:
  build-depends:
    package-b
  exposed-modules:
    Data.Bar
  reexported-modules:
    Data.Foo
```


If, for some reason, the reexport itself needs to be qualified, then a package name can be given using the [PackageImports](package-imports) syntax (but without an extra flag). An annotation can be appended as a quoted string:

```wiki
name: package-a
...

library:
  build-depends:
    package-b
  exposed-modules:
    Data.Bar
  reexported-modules:
    "package-b" Data.Foo "Importing Data.Foo via package-a is deprecated, please build-depend on package-b"
```

### InstalledPackageInfo


When we discussed this feature with SPJ during Backpack design discussion, Simon was keen on making it very easy to determine where a module actually lives during the lookup process. So while "reexported-modules" might be convenient surface syntax for specifying what modules to reexport, internally, we probably just want to generalize "exposed-modules" to be a tuple: exported module name, source InstalledPackageId, source module name. (Why exported module name? Because that lets us rename modules...)

### Steps to take


The implementation will have to touch (probably incomplete list):

- `Cabal`
- `ghc-pkg`
- The module lookup code in `GHC`
- `haddock`