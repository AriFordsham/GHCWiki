# Overloaded record fields: implementation notes


Here be dragons. This page describes implementation details and progress on the implementation of [the overloaded record fields plan](records/overloaded-record-fields/plan). Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields'). A [ prototype implementation](https://github.com/adamgundry/records-prototype/blob/master/RecordsPrototype.hs) is also available.

## The basic idea


The `Has` and `Upd` classes, and `GetResult` and `SetResult` type families, are defined in the module [ GHC.Records](https://github.com/adamgundry/packages-base/blob/overloaded-record-fields/GHC/Records.hs) in the `base` package.


Typechecking a record datatype still generates record selectors, but their names have a `$sel` prefix and end with the name of their type. Moreover, instances for the classes and type families are generated. For example,

```wiki
data T = MkT { x :: Int }
```


generates

```wiki
$sel_x_T :: T -> Int -- record selector (used to be called `x`)
$sel_x_T (MkT x) = x

$dfHasTx :: forall a . a ~ Int => Has T "x" a -- corresponds to the Has instance decl
$dfHasTx = Has { getField _ = $sel_x_T }

$dfUpdTx :: forall a . a ~ Int => Upd T "x" a -- corresponds to the Upd instance decl
$dfUpdTx = Upd { setField _ s e = s { x = e } }

axiom TFCo:R:GetResult : GetResult T "x" = Int   -- corresponds to the GetResult type family instance
axiom TFCo:R:SetResult : SetResult T "x" Int = T -- corresponds to the SetResult type family instance
```

## The naming of cats


The `AvailTC Name [Name] [(OccName, Name)]` constructor of `AvailInfo` represents a type and its pieces that are in scope. Record fields are now stored in a separate list (the third argument), along with their selectors. The `IEThingWith name [name] [OccName]` constructor of `IE`, which represents a thing that can be imported or exported, only stores the field labels. **SLPJ** Whoa!  Why should we duplicate this info.  My gut feel is that the selector should not appear in the second argument. **AMG** Does this sound better now? It's helpful if `gresFromAvail` need not do lookups (it is called by the desugarer).


The `Parent` type has an extra constructor `FldParent Name OccName` that stores the parent `Name` and the field `OccName`. The `GlobalRdrElt` (`GRE`) for a field stores the selector name directly, and uses the `FldParent` constructor to store the field. Thus a field `foo` of type `T` gives rise this entry in the `GlobalRdrEnv`:

```wiki
foo |->  GRE $sel_foo_T (FldParent T foo) LocalDef
```

**SLPJ** moreover I think we should store the *dictionary*`$dfHasTfoo` in the GRE for `foo`, not the selector.  That way we get both getter and setter (via the dictionary) in one go.  **AMG** Now I'm not sure about this. We can't build the dictionary for higher-rank fields, but they have a perfectly good selector. Moreover, with type-changing update there are two dictionaries (one for the getter and one for the setter) and two coercion axioms.


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


As well as `Has` instances, instances of the type family `GetResult` are generated, and exactly the same question about dfun names applies to their axiom names.

## Unused imports


Unused imports and generation of the minimal import list (`RnNames.warnUnusedImportDecls`) use a map from selector names to labels, in order to print fields correctly. However, fields may currently be reported as unused even if the corresponding Has instance is used. Consider the following:

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

**AMG** I thought this would be the `tcg_ev_binds` field of the `TcGblEnv`, but this seems to be empty by the end of `tcRnModule`. We could also look at the `inert_solved_dicts` field of `InertSet`, but I'm not sure how to propagate the required information out of the `TcS` monad to the `TcM` monad where unused names are reported.

## GADT record updates


Annoyingly, the generated code for `setField` doesn't typecheck for GADTs, because of [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595). Consider the example

```wiki
data W a where
    MkW :: a ~ b => { x :: a, y :: b } -> W (a, b)
```


At the moment, my code generates

```wiki
-- setField :: proxy "x" -> W (a, b) -> a -> W (a, b)
setField _ s e = s { x = e }
```


but this record update is rejected by the typechecker, even though it is perfectly sensible. The alternative is for me to generate the explicit update

```wiki
setField _ (MkW _ y) x = MkW x y
```


which is fine, but rather long-winded if there are many constructors or fields. Essentially this is doing the job of the desugarer for record updates. I wonder if it would be easier to fix [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595). Perhaps not; the general case makes my brain hurt. I only need a rather special case: updating one field, where either the type does not change, or none of the local constraints mention changing type variables.


Note that `W` does not admit type-changing single update for either field, because of the `a ~ b` constraint. Without it, though, type-changing update should be allowed.

## Type-changing update: phantom arguments


Consider the datatype

```wiki
data T a = MkT { foo :: Int }
```


where `a` is a phantom type argument (it does not occur in the type of `foo`). The traditional update syntax can change the phantom argument, for example if `r :: T Int` then `r { foo = 3 } :: T Bool` typechecks. However, `setField` cannot do so, because this is illegal:

```wiki
type instance SetResult (T a) "foo" Int = T b
```


Note that the result of the type family involves an unbound variable `b`. 


In general, a use of `setField` can only change type variables that occur in the field type being updated, and do not occur in any of the other fields' types.

## Data families


Consider the following:

```wiki
data family F (a :: *) :: *
data instance F Int  = MkF1 { foo :: Int }
data instance F Bool = MkF2 { foo :: Bool }
```


This is perfectly sensible, and should give rise to two \*different\* record selectors `foo`, and corresponding `Has` instances:

```wiki
instance t ~ Int => Has (F Int) "foo" t
instance t ~ Bool => Has (F Bool) "foo" t
```


However, what can we call the record selectors? They can't both be `$sel_foo_F`! Ideally we would use the name of the representation tycon, rather than the family tycon, but that isn't introduced until the typechecker (`tcDataFamInstDecl` in `TcInstDcls`), and we need to create the selector in the renamer (`getLocalNonValBinders` in `RnNames`). We can't just pick an arbitrary unique name, because we need to look up the selector to associate it with its data constructor (`extendRecordFieldEnv` in `RnSource`).

## Qualified names


Consider the following:

```wiki
module M where
  data S = MkS { foo :: Int }

module N where
  data T = MkT { foo :: Int }
  data U = MkU { foo :: Int }

module O where
  import M
  import N

  f x = M.foo x
  g x = N.foo x
  h x = foo x
```


Should there be a difference between `f`, `g` and `h`? It would seem odd if `f` could turn out to use the `foo` from `T` or `U` even though it explicitly says `M.foo`. I can see three sensible options:

- Treat qualified and unqualified fields identically, but issue a warning for qualified fields
- Forbid referring to overloaded fields with qualified names (so `M.foo` and `N.foo` yield errors)
- Treat a qualified name as a non-overloaded field, generating an ambiguity error if necessary (so `M.foo` is okay but `N.foo` is ambiguous)


Of course, it is fine to use a qualified name in a record update.

## Outstanding bugs


Some of the tests fail for the ghci way because the `System.IO` and `GHC.TopHandler` modules are not loaded automatically. I'm not sure if this is my fault, or if the problem existed in HEAD when I branched. For the moment, I've tweaked the testsuite to load the necessary modules.


The following triggers a panic when tracing the constraint solver. Again, I need to check if this bug exists in HEAD.

```wiki
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -ddump-tc-trace #-}

type family F (x :: *) :: *

class (y ~ F x) => C x y

x = () :: C x y => ()
```


Tests in need of attention:

- rename/should_fail/T5892a (accept changed output)
- ghci/scripts/ghci042 (accept changed output)
- ghci/prog002 prog003 scripts/ghci029 ghci036 ghci037 (scope issues in GHCi)
- typechecker/should_fail/tcfail102 (changed error message) 
- driver/T4437 (should `OverloadedRecordFields` be a GHC-only extension, or should Cabal know about it?)

## To do

- Sort out GADT record updates.
- Sort out data families with duplicated fields.
- Sort out qualified names.
- Improve error messages from typechecker:

  - Unsolved `Accessor p f` where `p` is something silly
- Consider defaulting `Accessor p` to `p = (->)`, and defaulting `Has r "f" t` constraints where there is only one datatype with a field `f` in scope.
- Sort out reporting of unused imports.
- How should dfunids/axioms and instances be propagated?
- Where should automatic instances be generated for GHCi?
- How should deprecation work for fields? Not at all?
