# [OverloadedRecordFields](records/overloaded-record-fields) implementation notes

**This page gives implementation details of the [OverloadedRecordFields](records/overloaded-record-fields) extension.  It is targeted at GHC hackers.**

- See the [redesign page](records/overloaded-record-fields/redesign) for a more gentle introduction.
- See [ Adam's post on the Well-Typed blog](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)


Following the 2015 redesign, we have three separate components to implement, which will be described (and implemented) in order:

- the `DuplicateRecordFields` extension, which permits the same field name to be used multiple times in the same module, but only where unambiguous;
- the `OverloadedLabels` extension, to enable the `#x` syntax;
- the `HasField` and `FieldUpdate` typeclasses, with special-purpose constraint solving behaviour, which do not require a language extension.


As of March 2015, work is progressing on the implementation. Part 1 (the `DuplicateRecordFields` extension) is available at [ Phab:D761](https://phabricator.haskell.org/D761) and being reviewed. Part 2 (the `OverloadedLabels` extension) is nearly ready, and part 3 will be implemented after the first two parts have been completed. Note that all the parts are useful in isolation.

# 1. The `DuplicateRecordFields` extension


When this extension is enabled, typechecking a record datatype still generates record selectors, but their `Name`s have a `$sel` prefix and end with the name of their type. For example,

```wiki
data T = MkT { x :: Int }
```


generates

```wiki
$sel:x:MkT :: T -> Int -- record selector (used to be called `x`)
$sel:x:MkT (MkT x) = x
```


This allows the same field label to occur multiple times in the same module, but with distinct `Name`s.


Turning on `DuplicateRecordFields` automatically enables `DisambiguateRecordFields`, because it strictly generalises it.

## The naming of cats

### `FieldLabel`


A field is represented by the following datatype, parameterised by the representation of names:

```wiki
data FieldLbl a = FieldLabel {
      flLabel        :: FieldLabelString, -- ^ Label of the field
      flIsOverloaded :: Bool, -- ^ DuplicateRecordFields enabled at definition site?
      flSelector     :: a,    -- ^ Record selector function
    }

type FieldLabelString = FastString
type FieldLabel = FieldLbl Name
```


For this purpose, a field "is overloaded" if it was defined in a module with `DuplicateRecordFields` enabled, so its selector name differs from its label. That is, it is irrelevant whether there are actually multiple identical field labels in the module. Every field has a label (`FastString`) and selector name. The `dcFields` field of `DataCon` stores a list of `FieldLabel`. In interface files, the `ifConFields` field of `IfaceConDecl` stores a list of `IfaceTopBndr`s for selectors, and `IfaceConDecls` for datatypes/newtypes stores the field labels.

### `AvailInfo` and `IE`


The new definition of `AvailInfo` is:

```wiki
data AvailInfo      = Avail Name | AvailTC Name [Name] AvailFields
type AvailFlds name = [AvailFld name]
type AvailFld name  = (name, Maybe FieldLabelString)
type AvailFields    = AvailFlds Name
type AvailField     = AvailFld Name
```


The `AvailTC` constructor represents a type and its pieces that are in scope. Record fields are now stored separately in the third argument. If a field is not overloaded, we store only its selector name (the second component of the pair is `Nothing`), whereas if it is overloaded, we store the label as well. The `IEThingWith name [name] (AvailFlds name)` constructor of `IE` represents a thing that can be imported or exported, and also has a separate argument for fields.


Note that a `FieldLabelString` and parent is not enough to uniquely identify a selector, because of data families: if we have

```wiki
module M ( F (..) ) where
  data family F a
  data instance F Int { foo :: Int }

module N ( F (..) ) where
  import M ( F(..) )
  data instance F Char { foo :: Char }
```


then `N` exports two different selectors with the `FieldLabelString``"foo"`. Similar tricks can be used to generate parents that have a mixture of overloaded and non-overloaded fields as children.

### `Parent` and `GlobalRdrElt`


The `Parent` type has an extra constructor `FldParent Name (Maybe FieldLabelString)` that stores the parent `Name` and the field label. The `GlobalRdrElt` (`GRE`) for a field stores the selector name directly, and uses the `FldParent` constructor to store the field. Thus a field `x` of type `T` gives rise this entry in the `GlobalRdrEnv`:

```wiki
x |->  GRE $sel:x:MkT (FldParent T (Just x)) LocalDef
```


Note that the `OccName` used when adding a GRE to the environment (`greOccName`) now depends on the parent field: for `FldParent` it is the field label, if present, rather than the selector name.

## Source expressions


An occurrence of a field is represented by the new datatype

```wiki
data FieldOcc name = FieldOcc RdrName (PostRn name name)
```


where the first component is the field name as written by the user (hence `RdrName`), and the second component (filled in by the renamer) is the name of the selector function.


The `HsExpr` type has an extra constructor `HsSingleRecFld (FieldOcc id)`. Regardless of whether `DuplicateRecordFields` is enabled, when `rnExpr` encounters `HsVar "x"` where `x` refers to an unambiguous record field `foo`, it replaces it with `HsSingleRecFld (FieldOcc "foo" $sel:foo:MkT)`. The point of this constructor is so we can pretty-print the field name, but store the selector name for typechecking.


Where an AST representation type (e.g. `HsRecField` or `ConDeclField`) contained an argument of type `Located id` for a field, it now stores a `Located (FieldOcc id)` aka `LFieldOcc id`. The new definition of `ConDeclField` (used in types) is:

```wiki
data ConDeclField name
  = ConDeclField { cd_fld_names :: [LFieldOcc name]
                   cd_fld_type  :: LBangType name, 
                   cd_fld_doc   :: Maybe LHsDocString }
```


The new definition of `HsRecField` is:

```wiki
data HsRecField id arg = HsRecField {
        hsRecFieldLbl :: LFieldOcc id,
        hsRecFieldArg :: arg,
        hsRecPun      :: Bool }
```


Rather than using this type for record construction/pattern-matching and update, a separate representation is used for updates, namely

```wiki
data HsRecUpdField id = HsRecUpdField {
        hsRecUpdFieldLbl :: Located RdrName,
        hsRecUpdFieldSel :: PostRn id [id],
        hsRecUpdFieldArg :: LHsExpr id,
        hsRecUpdPun      :: Bool
  }
```


We still have the `RdrName` as written by the user, but the individual selector may be ambiguous, and hence is represented by a list of possible selector names. The typechecker (`tcExpr`) tries three ways to disambiguate the update:

1. Perhaps only one type has all the fields that are being updated.

1. Use the type being pushed in, if it is already a `TyConApp`. 

1. Use the type signature of the record expression, if it exists and is a `TyConApp`.

TODO it would be nice if we could enforce in the types that ambiguous fields occur only between the renamer and the typechecker. For now, `hsRecUpdFieldSel` is guaranteed to contain a singleton list after the typechecker.

## Deprecated field names


Deprecations and fixity declarations look for a top-level name, so they cannot be applied to overloaded record fields. Perhaps this should change. Deprecations actually work by `OccName`, so we could make

```wiki
{-# DEPRECATED foo "Don't use foo" #-}
```


apply to all the `foo` fields in a module, but there are difficulties in deciding when a deprecated field has been used similar to those for [unused imports](records/overloaded-record-fields/implementation#unused-imports).

## Data families


Consider the following:

```wiki
data family F (a :: *) :: *
data instance F Int  = MkF1 { foo :: Int }
data instance F Bool = MkF2 { foo :: Bool }
```


This is perfectly sensible, and gives rise to two \*different\* record selectors `foo`. Thus we use the name of the first data constructor, rather than the type constructor, when naming the record selectors: we get `$sel:foo:R:MkF1` and `$sel:foo:R:MkF2`. Lexically (in the `GlobalRdrEnv`) the selectors still have the family tycon are their parent.

## Mangling selector names


We could mangle selector names (using `$sel:foo:MkT` instead of `foo`) even when the extension is disabled, but we decided not to because the selectors really should be in scope with their original names, and doing otherwise leads to:

- Trouble with import/export
- Trouble with deriving instances in GHC.Generics (makes up un-renamed syntax using field `RdrName`s)
- Boot files that export record selectors not working


Note that Template Haskell will see the mangled selector names instead of the original field labels, when looking at a datatype declared in a module with `DuplicateRecordFields` enabled. This isn't ideal - should we change the TH representation type so it can get access to both the label and selector?


In the new design, we could perhaps consider only mangling selector names when there is actually a name conflict, but this has not been investigated in any detail.

## GHC API changes

- The `minf_exports` field of `ModuleInfo` is now of type `[AvailInfo]` rather than `NameSet`, as this provides accurate export information. An extra function `modInfoExportsWithSelectors` gives a list of the exported names including overloaded record selectors (whereas `modInfoExports` includes only non-mangled selectors).
- The `HsExpr`, `hsRecField` and `ConDeclField` AST types have changed as described above.
- TODO are there any more changes?

## 2. The `OverloadedLabels` extension


This part is new in the 2015 redesign  The implementation is fairly straightforward and close to (but simpler than) the existing `ImplicitParameters` extension. The key points:

- We extend the lexer to treat `#x` as a single lexeme (only when `OverloadedLabels` is enabled) and parse it into a new constructor `HsOverLabel "x"` of `HsSyn`.

- A new module `GHC.OverloadedLabels` defines the (renamed) `IV` class

  ```wiki
  class OverloadedLabel (x :: Symbol) a where
    overloadedLabel :: a
  ```

- When the typechecker sees `HsOverLabel "x"`, it emits a new wanted constraint `OverloadedLabel "x" alpha`, just like `HsIPVar`.

## 3. The magic type classes


The `HasField` and `FieldUpdate` classes, and `FieldType` and `UpdatedRecordType` type families, will be defined in the module `GHC.Records` in the `base` package.  Contrary to the previous design, we will not generate any dfuns/axioms for these classes \*at all\*.  Instead, the typechecker will implicitly create evidence as required.  This gets rid of a whole lot of complexity.


The only additional things that need to be generated at datatype declarations are updater functions (one per field), which correspond to the selector functions that are already generated.  So for example

```wiki
data T = MkT { x, y :: Int }
```


will generate

```wiki
$sel:x:T :: T -> Int
$sel:x:T (MkT x _) = x

$upd:x:T :: T -> Int -> T
$upd:x:T (MkT _ y) x = MkT x y
```


The updater function will always have a name prefixed with `$upd:`, regardless of whether `OverloadedRecordFields` is enabled.

## GADT record updates


Consider the example

```wiki
data W a where
    MkW :: a ~ b => { x :: a, y :: b } -> W (a, b)
```


It would be nice to generate

```wiki
-- $upd:x:W :: W (a, b) -> a -> W (a, b)
$upd:x:W s e = s { x = e }
```


but this record update is rejected by the typechecker, even though it is perfectly sensible, because of [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595). The currently implemented workaround is instead to generate the explicit update

```wiki
$upd:x:W (MkW _ y) x = MkW x y
```


which is fine, but rather long-winded if there are many constructors or fields. Essentially this is doing the job of the desugarer for record updates.


Note that `W` does not admit type-changing single update for either field, because of the `a ~ b` constraint. Without it, though, type-changing update should be allowed.

## Unused imports


Unused imports and generation of the minimal import list (`RnNames.warnUnusedImportDecls`) use a map from selector names to labels, in order to print fields correctly. Moreover, consider the following:

```wiki
module A where
  data T = MkT { x,y:Int }

module B where
  data S = MkS { x,y::Bool }

module C where
  import A( T(x) )
  import B( S(x) )

  foo :: T -> Int
  foo r = #x r + 2
```


Now, do we expect to report the `import B( S(x) )` as unused? Only the typechecker will eventually know that. To record this, a new field `tcg_used_selectors :: TcRef NameSet` in the `TcGblEnv` records the selector names for fields that are encountered during typechecking (when looking up a `HasField` instance etc.). This set is used to calculate the import usage and unused top-level bindings. Thus a field will be counted as used if it is needed by the typechecker, regardless of whether any definitions it appears in are themselves used.


Unused local bindings are trickier, as the following example illustrates:

```wiki
module M (f)
  data S = MkS { foo :: Int }
  data T = MkT { foo :: Int }

  f = #foo (MkS 3)
  g x = #foo x
```


The renamer calculates the free variables of each definition, to produce a list of `DefUses`. The typechecker will discover that `f` uses only `S(foo)` while `g` uses neither. The simplest thing is to make an occurrence of an overloaded field in an expression return as free variables all the selectors it might refer to. This will sometimes fail to report unused local bindings: in the example, it will not spot that `T(foo)` is unused.
