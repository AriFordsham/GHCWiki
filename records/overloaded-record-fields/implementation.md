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
$sel_x_T (T x) = x

$dfHasTx :: Has T "x"      -- corresponds to the Has instance decl
$dfHasTx = Has $sel_x_T
```

## The naming of cats


The `AvailTC Name [Name] [(OccName, Name)]` constructor of `AvailInfo` represents a type and its pieces that are in scope. Record fields are now stored in a separate list (the third argument), along with their selectors. The `IEThingWith name [name] [OccName]` constructor of `IE`, which represents a thing that can be imported or exported, only stores the field labels. **SLPJ** Whoa!  Why should we duplicate this info.  My gut feel is that the selector should not appear in the second argument. **AMG** Does this sound better now? It's helpful if `gresFromAvail` need not do lookups (it is called by the desugarer).


The `Parent` type has an extra constructor `FldParent Name OccName` that stores the parent `Name` and the field `OccName`. The `GlobalRdrElt` (`GRE`) for a field stores the selector name directly, and uses the `FldParent` constructor to store the field. Thus a field `foo` of type `T` gives rise this entry in the `GlobalRdrEnv`:

```wiki
foo |->  GRE $sel_foo_T (FldParent T foo) LocalDef
```

**SLPJ** moreover I think we should store the *dictionary*`$dfHasTfoo` in the GRE for `foo`, not the selector.  That way we get both getter and setter (via the dictionary) in one go.  **AMG** Now I'm not sure about this. We can't build the dictionary for higher-rank fields, but they have a perfectly good selector. Moreover, if we want type-changing update there will be two dictionaries (one for the getter and one for the setter).


Note that the `OccName` used when adding a GRE to the environment (`greOccName`) now depends on the parent field: for `FldParent` it is the field label rather than the selector name.


The `dcFields` field of `DataCon` stores a list of `FieldLabel`, defined thus:

```wiki
type FieldLbl a = FieldLabel {
                      flOccName  :: OccName, -- ^ Label of the field
                      flSelector :: a        -- ^ Record selector function
                  }
type FieldLabel = FieldLbl Name
```


whereas the `ifConFields` field of `IfaceConDecl` stores a list of `FieldLbl OccName`.

## Source expressions


The `HsExpr` type has extra constructors `HsOverloadedRecFld OccName` and `HsSingleRecFld OccName id`. When `-XOverloadedRecordFields` is enabled, and `rnExpr` encounters `HsVar "x"` where `x` refers to multiple `GRE`s that are all record fields, it replaces it with `HsOverloadedRecFld "x"`. When the typechecker sees `HsOverloadedRecFld x` it emits a wanted constraint `Has alpha x beta` and returns type `alpha -> beta` where `alpha` and `beta` are fresh unification variables.


When the flag is not enabled, `rnExpr` turns an unambiguous record field `foo` into `HsSingleRecFld foo $sel_foo_T`. The point of this constructor is so we can pretty-print the field name but store the selector name for typechecking.


Where an AST representation type (e.g. `HsRecField` or `ConDeclField`) contained an argument of type `Located id` for a field, it now stores a `Located RdrName` for the label, and some representation of the selector. The parser uses an error thunk for the selector; it is filled in by the renamer  (by `rnHsRecFields1` in `RnPat`, and `rnField` in `RnTypes`). The new definition of `ConDeclField` (used in types) is:

```wiki
data ConDeclField name
  = ConDeclField { cd_fld_lbl  :: Located RdrName,
                   cd_fld_sel  :: name,
                   cd_fld_type :: LBangType name, 
                   cd_fld_doc  :: Maybe LHsDocString }
```


The new definition of `HsRecField` is:

```wiki
data HsRecField id arg = HsRecField {
        hsRecFieldLbl :: Located RdrName,
        hsRecFieldSel :: Either id [(id, id)],
        hsRecFieldArg :: arg,
        hsRecPun      :: Bool }
```


The renamer (`rnHsRecFields1`) supplies `Left sel_name` for the selector if it is unambiguous, or `Right xs` if it is ambiguous (because it is for a record update, and there are multiple fields with the correct label in scope). In the latter case, the possibilities `xs` are represented as a list of (parent name, selector name) pairs. The typechecker (`tcExpr`) tries three ways to disambiguate the update:

1. Perhaps only one type has all the fields that are being updated.

1. Use the type being pushed in, if it is already a `TyConApp`. 

1. Use the type signature of the record expression, if it exists and is a `TyConApp`.

## Automatic instance generation

`Has` instances are generated, provided the extension is enabled, in `tcInstDecls1` (the same time as derived instances (from **deriving** clauses) are generated). Every record field `GRE` in scope gives rise to an instance. Such instances are available when typechecking the current module (in `tcg_inst_env`) but not exported to other modules (via `tcg_insts`). At the moment, fresh `DFunId`s are generated for all instances in scope for each module, even though they are exported in interface files. Perhaps this should change.

**AMG** I wanted to generate each `DFunId` for `Has` once, at the field's definition site, but this causes problems for the fields defined in `base`, as the `Has` class may not be available. I've reverted to generating fresh `DFunId`s locally to each module for which `-XOverloadedRecordFields` is used. Is there a better way to do this?

## Unused imports


Unused imports and generation of the minimal import list (`RnNames.warnUnusedImportDecls`) currently show selector names rather than labels. We may need to create a mapping from dfun names to field labels (cf. `kids_env` in `RnNames.reportUnusedNames`) to know how to print them. Moreover, things are a bit trickier with `-XOverloadedRecordFields` enabled. Quoting SLPJ:


On unused imports, it's not just an implementation question.  

```wiki
module A where
  data T = MkT { x,y:Int }

module B where
  data S = MkS { x,y::Bool }

module C where
  import A( T(x) )
  import B( S(x) )

  foo :: T -> Int
  foo r = r.x + 2
```


Now, do we expect to report the 'x' in S(x) import as unused?  Actually the entire 'import B' is unused.  Only the typechecker will eventually know that.  But I think the type checker does actually record which instances are used, so perhaps we can make use of that info to give accurate unused-import info.

## Outstanding bugs


Some of the tests fail for the ghci way because the `System.IO` and `GHC.TopHandler` modules are not loaded automatically. I'm not sure if this is my fault, or if the problem existed in HEAD when I branched. For the moment, I've tweaked the testsuite to load the necessary modules.


Tests in need of attention:

- rename/should_fail/T2901 (mysterious change of location)
- rename/should_fail/T5372 (mysterious change of location)
- rename/should_fail/T5892a (accept changed output)

## To do


Only projection is implemented, not update, so there is no lens integration yet. We need to decide on a story here.


Implement the syntactic sugar `r { x :: t }`.


Test the interaction between fields and qualified names. In particular, a qualified name can be used for unambiguous identification of fields (e.g. in updates) but should probably not be used as an overloaded variable.


Universally quantified fields should result in a warning being emitted and no Has instance generated. What about existentially quantified fields?


How should deprecation work for fields? Not at all?
