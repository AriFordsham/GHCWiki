[TypeFunctions](type-functions)/Status

# Type Functions: Implementation Status

**Current:**

1. Dictionary handling for equational constraints: \[**Which of that has Tom done?**\]

  - Where do we check the details of the formation of equational constraints?  (In `check_pred_ty`?)
  - In the case for ordinary instances in `TcInstDcls.tcInstDecl2`, filter the ids of the super class equalities out of `map instToId sc_dicts`.  (They don't appear explicitly in the \`Hs' representation of the methods binding.)
  - Similarly with `map instToId meth_dicts` in `TcClassDcl.tcMethodBind`  Maybe we just need a special function to replace all occurences of `map instToId`?  Occurs also in `TcPat.tcConPat`.
  - We also have `map instToId` in `TcUnify.tcGen`, but here I am not sure yet whether we cans imply drop the coercion variables or have to do something else.
1. `TcSimplify`: Handle the presence of `EqPred`s in the given set, due to appearing in signature contexts.  (Including that `instToId` doesn't work on `EqPred`s.)  \[**Did Tom do that?**\]
1. Well-formedness checks for equational constraints (i.e., anything beyond the type arguments being boxed, rank 0 types)

## Parsing and Renaming


Todo (low-level): None.


Todo (high-level):

1. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)


Done:

- Parsing and renaming of kind signatures (toplevel and in classes).
- Parsing and renaming of indexed type declarations (toplevel and in classes).
- Using new syntax with `family` and `instance` on top level.
- Added `-findexed-types` switch.
- Allowing `type` tag in export lists to list associated types in the sub-binder list of an import/export item for a class.
- Import/export lists: ATs can be listed as subnames of classes and the data constructors of instances of a data family are subnames of that family.
- Parsing and renaming of equational constraints in contexts.

## Type Checking


Todo (low-level):

- Enforce syntactic constraints on type instances needed to ensure the termination of constraint entailment checking.
- If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)


Todo (high-level): \[**Tom has done much of this.**\]

1. Type checking of type functions (and hence, associated type synonyms); routines in `TcUnify` that need to be extended:

  - `boxySplitTyConApp`: The second argument (`BoxyRhoType`) can be a synonym family application.  Then, we must produce a wanted coercion and return a `HsWrapper` value that applies that coercion.
  - `boxySplitAppTy`: Basically, the same deal as the previous.
  - `boxySubMatchType`: Not sure yet.  Do we need to handle the case where in `go` one type is `FunTy` and the other a synonym family application?  But the function doesn't handle the case where a `FunTy` meets a tyvar either, so its probably ok to ignore.
  - `boxy_match`: Not sure yet.  Do we have to handle the case where `t_ty` is a synonym family application?  On the other hand, this prematching seems to be an approximation.  How much does it hurt if we just  ignore this?
  - `boxy_lub`: Unclear.  Also seems to approximate.
  - `tcSubExp`: Defer to boxy matching if we have a synonym family application.
  - `uTysOuter`, `u_tys`, `uPred`, `uVar`, and their intefaces `boxyUnify`, `boxyUnifyList`, `unifyType`, `unifyPred`, `unifyTheta`, and `unifyTypeList`: Generate wanted equalities and produce coercion(s).  !!!Still need to check usage patterns!!!

  To make things easy, we might want to always return a `HsWrapper` value (unless the unification fails), which is `WpHole` whenever the coercion is empty.  The disadvantage is that this blows the tree between type checking and desugaring up.  An alternative is to return the coercion only when needed, but write some auxilliary functions that take the result of `boxySplitTyConApp` and friends and turn the optional coercion result in an always present (real Haskell) function that we always apply to the type-checked pattern (in `TcPat`) or expression.  It is simply `id` when we don't need a coercion.

> >
> > Invariants:

- Arguments and results of synonym families are always tau types.  Then, boxy subsumption becomes easy to handle.  When we have a synonym family application, we defer to boxy matching (i.e., unification) and if that comes up with a coercion, we return it as the subsumption coercion.

1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type check functional dependencies as type functions.


Done: 

- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types, including the generation of representation tycons.
- Wrapper generation and type checking of pattern matching for indexed data and newtypes.
- Consistency checking for family instances.

## Desugaring


Todo (low-level): None.


Todo (high-level): None.


Done:

- Representation of family kind signatures as `TyCon.TyCon`s.
- Extension of `Class.Class` by associated `TyCon`s.
- Extension of `TyCon.TyCon` with a reference to the parent `TyCon` for data instances.
- Extension of `DataCon.DataCon` with instance types for constructors belonging to data instances.
- Extension of `TyCon.TyCon` such that the parent of a data instance is paired with a coercion identifying family instance and representation type.
- For indexed data types, the datacon wrapper uses data instance coercion and pattern matching casts the scrutinee via an `ExprCoFn` in a `CoPat`.
- Import and exporting.
- Generation and plumbing through of rough matches.
- Equational constraints in contexts.

## Regression tests of type family patches


Current `validate` result:

```wiki
Unexpected passes:
   Class1(normal)

Unexpected failures:
   Refl2(normal)
   tc210(normal)
   tc211(normal)
   tcfail065(normal)
   tcfail068(normal)
   tcfail071(normal)
   tcfail076(normal)
   tcfail102(normal)
   tcfail103(normal)
   tcfail153(normal)
   tcfail179(normal)
```

- Class1: VALID.  (Only marked to fail in head to keep validate happy.)
- Relf2: VALID. Type family BUG.
- ~~Simple5a~~: VALID. Changed error message for data families, BUT the new error message is cryptic.
- ~~break001~~: INVALID.  GHC panic instead of printing error message about ambiguous variable.
- ~~break006~~: INVALID.  Seems to be the same problem as break001.
- ~~print019~~: INVALID.  Seems to be the same problem as break001.
- ~~rw~~: VALID. Changed error message for GADTs.  Seems to be the same behaviour as in Simple5a. 
- tc210: INVALID (matching `forall a.a -> Int` against \`Int -\> Int fails).
- tc211: INVALID (tests impredicative types).
- ~~tcfail046~~: VALID.  Changed error message, BUT the new error message has one more type synonym unfolding, which should be avoided.
- tcfail065: VALID.  Cosmetic difference, as tidy names are assigned in different order.
- tcfail068: ?? Reports two errors less (probably due to different recovery points)
- tcfail071: VALID.  Now reports one instead of two errors as deferred unification is checked only after the contexts of mutually recursive groups have been unified.  (The latter is what this test case is really about, and it still works fine for that.)
- tcfail076: VALID.  Same as tcfail065.
- tcfail102: VALID.
- tcfail103: VALID.  Error message is actually better!
- ~~tcfail128~~: VALID. Same as tcfail046.
- ~~tcfail145~~: VALID. Error message got worse.
- tcfail153: VALID.  Error message is different, but equally correct and accurate.  The type mismatch manifests itself at two different subexpressions.  Due to a different traversal order, we now report the error at the other subexpression.
- tcfail179: VALID.  If anything, the error message improved.
- while: VALID. Works if definition of `succeed` gets a type signature `Monad m => a -> m a`.  The error seems to be due to the new GADT rules about annotations, but the error message is a bit strange; ie, need to be improved.


Summary of *critical* problems:

1. ~~Panic in case of ambiguous type variables (break001, break006, and print019).~~
1. Problem instantiating rank-2/impredicative types (tc210 & tc211).
