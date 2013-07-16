# Overloaded record fields: implementation notes


Here be dragons. This page describes implementation details and progress on the implementation of [the overloaded record fields plan](records/overloaded-record-fields/plan). Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields').

## The basic idea


Typechecking a record datatype still generates record selectors, but their names have a `$sel` prefix and end with the name of their type. Additionally, a dictionary function (for the `Has` instance) is generated. Thus

```wiki
data T = MkT { x :: Int }
```


generates

```wiki
$sel_x_T :: T -> Int       -- record selector (used to be called `x`)
$df_x_T  :: Has T "x" Int  -- dictionary function (coerced selector)
```

## The naming of cats


The `AvailTC Name [Name] [OccName]` constructor of `AvailInfo` represents a type and its pieces that are in scope. Record fields are now stored in a separate list (the third argument). Every field should have a corresponding selector (in the second argument). Since we have the name of the type (the first argument), we can find the selector corresponding to a field. Similar changes are required to the `IEThingWith` constructor of `IE`, which represents a thing that can be imported or exported.


The `Parent` type has an extra constructor `FldParent Name OccName` that stores the parent `Name` and the field `OccName`. The `GlobalRdrElt` (`GRE`) for a field stores the selector name directly, and uses the `FldParent` constructor to store the field. Thus a field `foo` of type `T` gives rise to two entries in the `GlobalRdrEnv`:

```wiki
$sel_foo_T  |->  GRE $sel_foo_T (ParentIs T) LocalDef
foo         |->  GRE $sel_foo_T (FldParent T foo) LocalDef
```


Note that the `OccName` used when adding a GRE to the environment now depends on the parent field: for `FldParent` it is the field rather than the selector name.


The `dcFields` field of `DataCon` stores a list of

```wiki
type FieldLabel = (OccName, Name)
```


where the first component is the field and the second is the selector function.


Where an AST representation type (e.g. `HsRecField` or `ConDeclField`) contained an argument of type `Located id` for a field, it now stores a `Located RdrName` for the label and `Maybe id` for the selector. The parser supplies `Nothing` for the selector; it is filled in by the renamer  (by `rnHsRecFields1` in `RnPat`, and `rnField` in `RnTypes`). Partial functions are provided to extract the `Located id`, but they will panic if called on not-yet-renamed syntax.

## Next steps


The `HsExpr.HsExpr` type has an extra constructor `HsOverloadedRecFld OccName`. When `-XOverloadedRecordFields` is enabled, and the renamer encounters `HsVar "x"` where `x` refers to multiple `GRE`s that are all record fields, it replaces it with `HsOverloadedRecFld "x"`. There needs to be a similar constructor for non-overloaded projections, so that we can pretty-print the field name but store the selector name.


When the typechecker sees `HsOverloadedRecFld x` it emits a wanted constraint `Has alpha x beta` and returns type `alpha -> beta` where `alpha` and `beta` are fresh unification variables.


Automatic `Has` instances are generated, provided the extension is enabled, around the same time as derived instances (from **deriving** clauses) are generated. Every record field `GRE` in scope gives rise to an instance, and the dictionary function is already available. Such instances are available when typechecking the current module (in `tcg_inst_env`) but not exported to other modules (via `tcg_insts`).
