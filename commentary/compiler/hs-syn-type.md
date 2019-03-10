# The `HsSyn` types


The program is initially parsed into "**`HsSyn`**", a collection of data types that describe the full abstract syntax of Haskell.  `HsSyn` is a pretty big collection of types: there are 52 data types at last count.  Many are pretty trivial, but a few have a lot of constructors (`HsExpr` has 40).  `HsSyn` represents Haskell its full glory, complete with all syntactic sugar.


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

## Decorating `HsSyn` with type information


The type checker adds type information to the syntax tree, otherwise leaving it as undisturbed as possible.  This is done in two ways:

- Some constructors have a field of type `PostTcType`, which is just a synonym for `Type`. For example:

  ```wiki
  data HsExpr id = ... | ExplicitList PostTcType [LHsExpr id] | ...

  type PostTcType = Type

  placeHolderType :: PostTcType
  placeHolderType = panic "Evaluated the place holder for a PostTcType"
  ```

  An `ExplicitList` represents the explicit list construct in Haskell (e.g. "`[2, 4, 1]`"). The parser fills the `PostTcType` field with an error thunk `HsTypes.placeHolderType`; and the renamer does not touch it.  The typechecker figures out the type, and fills in the value.  So until the type checker, we cannot examine or print the `PostTcType` fields.

>
> The error thunks mean that we can't conveniently pretty-print the `PostTcType` fields, because the pretty-printer would poke the error thunks when run on pre-typchecked code.  We could have defined `PostTcType` to be `Maybe Type`, but that would have meant unwrapping lots of `Just` constructors, which is messy.  It would be nicer to parameterise `HsSyn` over the `PostTcType` fields.  Thus:
>
> ```wiki
>   type RnHsBinds = HsBinds Name ()   -- After renaming
>   type TcHsBines = HsBinds Id Type   -- After type checking
> ```
>
>
> This would be a Good Thing to do.

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

  - `HsWrap`, in the `HsExpr` type.
  - `AbsBinds`, in the `HsBinds` type.

>
> These are invariably to do with type abstraction and application, since Haskell source is implicitly generalized and instantiated, whereas GHC's intermediate form is explicitly generalized and instantiated.

## Source Locations

`HsSyn` makes heavy use of the `Located` type ([compiler/BasicTypes/SrcLoc](/trac/ghc/browser/ghc/compiler/BasicTypes/SrcLoc)):

```wiki
data Located e = L SrcSpan e
```


A `Located t` is just a pair of a `SrcSpan` (which describes the source location of `t`) and a syntax tree `t`.  The module `SrcLoc` defines two other types:

- `SrcLoc` specifies a particular source location: (filename, line number, character position)
- `SrcSpan` specifes a range of source locations: (filename, start line number and character position, end line number and character position)


More details in [compiler/BasicTypes/SrcLoc](/trac/ghc/browser/ghc/compiler/BasicTypes/SrcLoc).


Naming convention within the code: "`LHs`" means located Haskell, e.g.

```wiki
type LHsBinds n = Located (HsBinds n)
```