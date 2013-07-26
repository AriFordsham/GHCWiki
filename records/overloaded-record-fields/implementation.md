# Overloaded record fields: implementation notes


Here be dragons. This page describes implementation details and progress on the implementation of [the overloaded record fields plan](records/overloaded-record-fields/plan). Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields').

## The basic idea


Typechecking a record datatype still generates record selectors, but their names have a `$sel` prefix and end with the name of their type. Thus

```wiki
data T = MkT { x :: Int }
```


generates

```wiki
$sel_x_T :: T -> Int       -- record selector (used to be called `x`)
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

## Source expressions


The `HsExpr` type has extra constructors `HsOverloadedRecFld OccName` and `HsSingleRecFld OccName id`. When `-XOverloadedRecordFields` is enabled, and `rnExpr` encounters `HsVar "x"` where `x` refers to multiple `GRE`s that are all record fields, it replaces it with `HsOverloadedRecFld "x"`. When the typechecker sees `HsOverloadedRecFld x` it emits a wanted constraint `Has alpha x beta` and returns type `alpha -> beta` where `alpha` and `beta` are fresh unification variables.


When the flag is not enabled, `rnExpr` turns an unambiguous record field `foo` into `HsSingleRecFld foo $sel_foo_T`. The point of this constructor is so we can pretty-print the field name but store the selector name for typechecking.

## Automatic instance generation

`Has` instances are generated, provided the extension is enabled, in `tcInstDecls1` (the same time as derived instances (from **deriving** clauses) are generated). Every record field `GRE` in scope gives rise to an instance. Such instances are available when typechecking the current module (in `tcg_inst_env`) but not exported to other modules (via `tcg_insts`). At the moment, fresh `DFunId`s are generated for all instances in scope for each module, even though they are exported in interface files. Perhaps this should change.

## Known bugs


Unused imports and generation of the minimal import list (`warnUnusedImportDecls` in `RnNames`) show selector names rather than labels. This is a pain to fix, as there is no easy way to get from a selector `Name` to the `OccName` of the label. We could add the label to the `RecSelId` constructor of `IdDetails`. Or perhaps there is a way to keep track of label/selector relationships when renaming imports?


Some of the ghci tests fail with the following messages:

```wiki
Not in scope: ‛System.IO.hSetBuffering’
Not in scope: ‛System.IO.stdout’
Not in scope: data constructor ‛System.IO.LineBuffering’
Not in scope: ‛GHC.TopHandler.runIOFastExit’
```


I'm not sure if this is my fault.


Tests in need of attention:

- rename/should_compile/T5334, should_fail/T5372, T5533, T5892a, T7943
- gadt/records-fail1 (bad error message)

## To do


Typechecking of record updates still uses the first field to determine the type, so the renamer has to produce unambiguous selector names. It should be possible to specify the record type via a type signature, so `rnHsRecFields1` needs to be a bit more liberal.


Only projection is implemented, not update, so there is no lens integration. We need to decide on a story here.


Rather than generating fresh `DFunId`s for all the fields in scope, only generate them for locally-defined fields, and pass them around so that other modules can use them.


Implement the syntactic sugar `r { x :: t }`.


Test the interaction between fields and qualified names. In particular, a qualified name can be used for unambiguous identification of fields (e.g. in updates) but should probably not be used as an overloaded variable.


Universally quantified fields should result in a warning being emitted and no Has instance generated. What about existentially quantified fields?


How should deprecation work for fields? Not at all?
