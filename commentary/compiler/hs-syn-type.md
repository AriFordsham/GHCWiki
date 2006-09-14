
\[ Up: [Commentary/Compiler/HscMain](commentary/compiler/hsc-main) \]

# The `HsSyn` types


The program is initially parsed into "**`HsSyn`**", a collection of data types that describe the full abstract syntax of Haskell.  `HsSyn` is a pretty big collection of types: there are 52 data types when I last counted.  Many are pretty trivial, but a few have a lot of constructors (`HsExpr` has 40).  `HsSyn` represents Haskell its full glory, complete with all syntactic sugar.


The `HsSyn` modules live in the [compiler/hsSyn](/trac/ghc/browser/ghc/compiler/hsSyn) directory.  Each module declares a related group of declarations, *and* gives their pretty-printer.

- [compiler/hsSyn/HsSyn.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.lhs): the root module.  It exports everything you need, and it's generally what you should import.
- [compiler/hsSyn/HsBinds.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsBinds.lhs): bindings.
- [compiler/hsSyn/HsImpExp.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsImpExp.lhs): imports and exports.
- [compiler/hsSyn/HsDecls.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsDecls.lhs): top-level declarations.
- [compiler/hsSyn/HsExpr.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsExpr.lhs): expressions, match expressions, comprehensions.
- [compiler/hsSyn/HsLit.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsLit.lhs): literals.
- [compiler/hsSyn/HsPat.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsPat.lhs): patterns.
- [compiler/hsSyn/HsTypes.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsTypes.lhs): types.
- [compiler/hsSyn/HsUtils.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsUtils.lhs): utility functions (no data types).


There is significant mutual recursion between modules, and hence a couple of `lhs-boot` files. Look at [ModuleDependencies](module-dependencies) to see the dependencies.


The type checker adds type information to the syntax tree, otherwise leaving it as undisturbed as possible.  This is done in two ways:

- Some constructors have a field of type `PostTcType`, which is just a synonym for `Type`. For example:

  ```wiki
  data HsExpr id = ... | ExplicitList PostTcType [LHsExpr id] | ...

  type PostTcType = Type

  placeHolderType :: PostTcType
  placeHolderType = panic "Evaluated the place holder for a PostTcType"
  ```

  An `ExplicitList` represents the explicit list construct in Haskell (e.g. "`[2, 4, 1]`"). The parser fills the `PostTcType` field with an error thunk `HsTypes.placeHolderType`; and the renamer does not touch it.  The typechecker figures out the type, and fills in the value.  So until the type checker, we cannot examine or print the `PostTcType` fields.

- In a few cases, the typechecker moves from one constructor to another.  Example:

  ```wiki
  data HsPat id
    = ...
    | ConPatIn	(Located id)
  		(HsConDetails id (LPat id))

    | ConPatOut	(Located DataCon)
  		[TyVar]			-- Existentially bound type variables
  		[id]			-- Ditto dictionaries
  		(DictBinds id)		-- Bindings involving those dictionaries
  		(HsConDetails id (LPat id))
  		Type    		-- The type of the pattern
    ...
  ```

  The parser and renamer use `ConPatIn`; the typechecker generates a `ConPatOut`. This naming convention is used consistently.

- There are a few constructors added by type checker (rather than replacing an input constructor), particularly:

  - `HsCoerce`, in the `HsExpr` type.
  - `AbsBinds`, in the `HsBinds` type.
    ToDo: say more
