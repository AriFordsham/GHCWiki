# `DuplicateRecordFields`


This page describes the `DuplicateRecordFields` extension, part 1 of the [OverloadedRecordFields proposal](records/overloaded-record-fields/redesign).

## Design


The `DuplicateRecordFields` extension permits existing Haskell records to use duplicate field labels.  Thus the following is legal in a single module:

```wiki
data Person  = MkPerson  { personId :: Int, name :: String }
data Address = MkAddress { personId :: Int, address :: String }
```


Without the extension, this module will not compile because it tries to generate two selector functions called `personId`. When the extension is enabled, both selector functions are generated, and the renamer will determine which is meant at use sites.

### Selector functions


Bare uses of the field refer only to the selector function, and work only if this is unambiguous.  Thus, with the above example definitions we can write

```wiki
x p = name p
```


but bare use of `personId` leads to a name resolution error, e.g. in

```wiki
y p = personId p
```


There is not yet any facility for using types to disambiguate which selector function is meant.


Even though a field label is duplicated in its defining module, it may be possible to use the selector unambiguously elsewhere. For example, another module could import `Person(personId)` but not `Address(personId)`, and then use `personId` unambiguously. Thus it is not enough simply to avoid generating selector functions for duplicated fields.

### Construction and pattern-matching


Uses of fields that are always unambiguous because they mention the constructor, including construction and pattern-matching, may freely use duplicated names.  For example, the following are permitted with both `Person(personId)` and `Address(personId)` in scope:

```wiki
a = MkPerson { personId = 1, name = "Julius" }

f (MkPerson{personId = i}) = i
```


In particular, this makes it possible to extract a field from a record even if the selector function is ambiguous.

### Disambiguating record updates


In a record update such as `e { personId = 1 }`, if there are multiple `personId` fields in scope, the type of the context must fix which record datatype is intended, or a type annotation must be supplied. While we require record updates to determine a single unambiguous record type, we can be slightly more liberal than Haskell 98 in how we determine that record type. Consider the following definitions:

```wiki
data S = MkS { foo :: Int }
data T = MkT { foo :: Int, bar :: Int }
data U = MkU { bar :: Int, baz :: Int }
```


Previously, an update mentioning `foo` would automatically be ambiguous if all these definitions were in scope. With `DuplicateRecordFields`, however, we can try the following:

1. Check for types that have all the fields being updated. For example:

  ```wiki
  f x = x { foo = 3, bar = 2 }
  ```

  Here `f` must be updating `T` because neither `S` nor `U` have
  both fields. This may also discover that no possible type exists.
  For example the following will be rejected:

  ```wiki
  f' x = x { foo = 3, baz = 3 }
  ```

1. Use the type being pushed in, if it is an application of a type constructor. The following are valid updates to `T`:

  ```wiki
  g :: T -> T
  g x = x { foo = 3 }

  g' x = x { foo = 3 } :: T
  ```

1. Use the type signature of the record expression, if it exists and is an application of a type constructor. Thus this is valid update to `T`:

  ```wiki
  h x = (x :: T) { foo = 3 }
  ```


Note that we do not look up the types of variables being updated, and no constraint-solving is performed, so for example the following will be rejected as ambiguous:

```wiki
let x :: T
    x = blah
in x { foo = 3 }

\x -> [x { foo = 3 },  blah :: T ]

\ (x :: T) -> x { foo = 3 }
```


We could add further tests, of a more heuristic nature. For example, rather than looking for an explicit signature, we could try to infer the type of the record expression, in case we are lucky enough to get an application of a type constructor straight away. However, it might be hard for programmers to predict whether a particular update is sufficiently obvious for the signature to be omitted.

### Import and export of record fields


When `DuplicateRecordFields` is enabled, an ambiguous field must be exported as part of its datatype, rather than at the top level. For example, the following is legal:

```wiki
module M ( Person(personId), Address(..) ) where
data Person  = MkPerson  { personId :: Int, name :: String }
data Address = MkAddress { personId :: Int, address :: String }
```


However, this would not be permitted, because `personId` is ambiguous:

```wiki
module M (personId) where ...
```


Similar restrictions apply on import.

## Implementation


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


apply to all the `foo` fields in a module, but there are difficulties in deciding when a deprecated field has been used similar to those for [unused imports](records/overloaded-record-fields/duplicate-record-fields#unused-imports).

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
