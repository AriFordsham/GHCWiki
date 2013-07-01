# Overloaded record fields: implementation notes


Here be dragons. This page describes implementation details and progress on the implementation of [the overloaded record fields plan](records/overloaded-record-fields/plan). Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields').

## Upcoming changes


Typechecking a record datatype will still generates record selectors, but their names contain the name of their type. Additionally, a dictionary function (for the `Has` instance) is generated. Thus

```wiki
data T = MkT { x :: Int }
```


generates

```wiki
$sel_x_T :: T -> Int      -- record selector (used to be called `x`)
$df_x_T  :: Has T "x" Int  -- dictionary function (coerced selector)
```


The `dcFields` field of `DataCon.DataCon` (constructor `MkData`) stores a list of `FastString` field names, rather than `Name`s.


The `AvailTC` constructor of `Avail.AvailInfo` represents a type and its pieces that are in scope. Record fields are stored in a separate list (rather than bundled in with data constructors and class methods), which contains the `FastString` field name, and perhaps the names of the record selector and dictionary function.


Similarly, the `RdrName.Parent` type has an extra constructor `FldParent` which stores the field name (and possibly the dfunid). Thus it is easy to tell whether a `RdrName.GlobalRdrElt` (`GRE`) is a field. In this case, the name of the `GRE` is the selector function.


The `HsExpr.HsExpr` type has an extra constructor `HsOverloadedRecFld FastString`. When `-XOverloadedRecordFields` is enabled, and the renamer encounters `HsVar "x"` where `x` refers to multiple `GRE`s that are all record fields, it replaces it with `HsOverloadedRecFld "x"`. (Is there any reason to treat single record fields differently to other ids?)


When the typechecker sees `HsOverloadedRecFld x` it emits a wanted constraint `Has alpha x beta` and returns type `alpha -> beta` where `alpha` and `beta` are fresh unification variables.


Automatic `Has` instances are generated, provided the extension is enabled, around the same time as derived instances (from **deriving** clauses) are generated. Every record field `GRE` in scope gives rise to an instance, and the dictionary function is already available. Such instances are available when typechecking the current module (in `tcg_inst_env`) but not exported to other modules (via `tcg_insts`).
