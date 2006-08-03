# Type Functions and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type functions and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.

## Aims


New features:

- Open type-indexed data types and type functions
- Associated data types and type synonyms, which are type-indexed data types and type functions associated with a class - i.e., associated types are syntactic sugar for type-indexed types and type functions.


Revised features

- We may want to re-implement functional dependencies using associated type synonyms.


We keep track of the current [implementation status](type-functions-status).

## Specification and Restrictions


Refinement of the specification in the *Beyond Associated Types* paper.  (I'll actually link this paper here once it is a bit more coherent.)  Some [examples are on an extra page](type-functions-examples).

- Kind signatures of indexed data types have the form

  ```wiki
  data T a1 .. an :: <kind>
  ```

  and introduce a data type whose first `n` argument are indexes, with `n` \>= 1.  The `<kind>` can specify additional parametric parameters.   Index variables can have a kind annotation.  Indexed newtypes have the same form, except for the keyword.
- Kind signatures of type functions have the form

  ```wiki
  type [iso] T a1 .. an :: <kind>
  ```

  and introduce an `n`-ary type functions, which may be of higher-kind, with `n` \>= 1.  Again, the type variables can have kind signatures.  The modifier `iso` is optional and requires the type function to be injective.  (In principle, we could make the `<kind>` optional, with `*` being the default, but we don't do that for uniformity with signatures of indexed types - the form `data T a1 .. an` is already used for empty data types.)
- Applications of indexed types need to supply all indexes; i.e., partial application to indexes is not admitted.  (Arguments beyond the indexes can be partially supplied as usual.)
- Instances of indexed data types/newtypes and equations of type functions have the same form as vanilla data types/newtypes and type synonyms, respectively, but can have non-variable type indexes in index positions.  Type indexes can include applications of indexed data types and newtypes, but no type functions.
- Instances of indexed data types and new types as well as type equations are only valid if a matching kind signature is in scope.
- The degenerate case of a data type/newtype declaration or type equation where all type parameters are variables is valid without a kind signature and coincides with the data types and type synonyms of vanilla Haskell.  In fact, for the moment, we do not allow the degenerate case to have a kind signature.  The latter constraint could be dropped if it proves to be inconvenient.  (Rationale: Multiple type equations are useless when one is degenerate - as the whole system needs to be confluent and we don't have sequential matching on type equations.  So, we get backwards compatibility for free.)
- All type indexes of an associated indexed type or type function need to be class parameters.
- Instances of indexed types may not overlap.  Instances of type equations may only overlap if the equations coincide at critical pairs.  (Rational: We cannot be more lazy about checking overlap, as we otherwise cannot guarantee that we generate an F<sub>C</sub> program that fulfils the formal consistency criterion.)


Restrictions:

- We currently don't allow associated GADTs. I cannot see any fundamental problem in supporting them, but I want to keep it simple for the moment. (When allowing this, a constructor signature in an associated GADT can of course only refine the instantiation of the type arguments specific to the instance in which the constructor is defined.)

## Terminology

**Parametric type constructors**: Type constructors in vanilla Haskell.

**Indexed type constructors**: Type constructors that are defined via one or more type declarations that have non-variable parameters.  We often call them sloppily just *indexed types*.

**Kind signature**: Declaration of the name, kind, and arity of an indexed type constructor.  The *arity* is the number of type indexes - *not* the overall number of parameters - of an indexed type constructor.

**Type function**: An indexed type synonym.

**Indexed data type**: An indexed type constructor declared with `data` or `newtype`.

**Associated type**: An indexed type that is declared in a type class.

**Type family**: Indexed types can be regarded as families of types; especially in the case of indexed data types, we call each declaration at a particular type index as *member* or *element* of that family.

**Definitions vs. declarations**: We sometimes call the kind signature of an indexed constructor its *declaration* and the subsequent population of the type family by type equations or indexed data/newtype declarations the constructor's *definition*.

## How It Works

### Syntax of kind signatures and definitions of indexed types


All kind signature consists of a type declaration head followed by a `::` and a kind.  In the case of  a data declaration, we addititonally require that there is no `where` clause.   We require for every definition of an indexed type (i.e., type equations or indexed data/newtype declaration) that a matching kind signature is in scope.  Vanilla type synonym definitions and data/newtype declarations fall out as special cases of type function equations and indexed type declarations that have variable-only patterns, for which we require no kind signatures.  The vanilla forms are also closed (further definitions would be useless, as they are bound to overlap).

### Representation of indexed types

#### Kind signatures

`HsDecls.TyClDecl` has a new variant `TyFunction` to represent signatures of type functions.  These consist of the name, type parameters, an iso flag, and optionally an explicit result kind.  The type parameters can have kind signatures as usual.  


Signatures for indexed data and newtypes are represented as a special case of `TyData`, namely when `TyData` has a kind signature, but no constructors. 


We recognise both forms of kind signatures by the predicate `HsDecls.isKindSigDecl`.

#### Definitions of indexed types


We represent type functions and indexed data and newtypes by generalising type synonym declarations `TySynonym` and data type declarations `TyData` to allow patterns ofr type indexes instead of just type variables as parameters.  In both variants, we do so by way of the field `tcdPats` of type `Maybe [LHsType name]`, used as follows:

- If it is `Nothing`, we have a *vanilla* data type declaration or type synonym declaration and `tcdVars` contains the type parameters of the type constructor.
- If it is `Just pats`, we have the definition of an a indexed type (toplevel or nested in an instance declarations).  Then, 'pats' are type patterns for the type-indexes of the type constructor and `tcdVars` are the variables in those patterns.  Hence, the arity of the type constructor is `length tcdPats` and *not*`length tcdVars`.


In both cases (and as before we had type functions), `tcdVars` collects all variables we need to quantify over.

#### Parsing and AST construction


The LALR parser allows arbitrary types as left-hand sides in **data**, **newtype**, and **type** declarations.  The parsed type is, then, passed to `RdHsSyn.checkTyClHdr` for closer analysis (possibly via `RdHsSyn.checkSynHdr`).  It decomposes the type and, among other things, yields the type arguments in their original form plus all type variables they contain.  Subsequently, `RdrHsSyn.checkTyVars` is used to either enforce that all type arguments are variables (second argument is `False`) or to simply check whether the type arguments are variables (second argument `True`).  If in enforcing mode, `checkTyVars` will raise an error if it encounters a non-variable (e.g., required for class declarations).  If in checking mode, it yields the value placed in the `tcdPats` field described above; i.e., returns `Nothing` instead of the type arguments if these arguments are all only variables.

### Representation of associated types


We add type declarations to class declarations and instance declarations by a new field, of type `[LTyClDecl]`, to both `TyClDecl.ClassDecl` (known by the field name `tcdATs`) and `TyClDecl.InstDecl`.  For classes, this new field contains values constructed from `TyData`, `TyFunction`, and `TySynonym`, whereas for instances, we only have `TyData` and `TySynonym`.  This is due to (a) `TyData` representing both signatures and definitions of associated data types (whereas the two are split into `TyFunction` and `TySynonym` for associated synonyms) and (b) associated synonyms having default definitions, which associated data types do not possess.

### Phasing


GHC is organised such that class and type declarations are processed (during renaming and type checking) before any instance declarations are considered.  In the presence of associated types, instance declarations may contain type definitions.  In particular, the *data constructors* introduced by associated data declarations need to be brought into scope before we can rename any expressions.

### Renaming of indexed types

#### Kind signatures


Kind signatures are renamed by `RnSource.rnTySig`, which is parametrised by a function that handles the binders (i.e., index variables) of the declaration.  This is so that we can use the same code for toplevel signatures and those in classes.  In the former case, the variables are in a defining position, whereas in classes they are in a usage position (as all index variables must be class parameters).

#### Definitions of indexed types


There is little extra that needs to be done for indexed types.  The main difference between vanilla synonyms and data/newtype declarations and the indexed variants is that the `tcdTyPats` field is not `Nothing`.  We simply call `rnTyPats` on these fields, which traverses them in the usual way.

#### Renaming of associated types


Associated **data** definitions are particularly interesting, as they not only introduces, but also value level entities, namely the data constructors.  During renaming, we enter the names of all data constructors that an associated data type defines into the global `RdrName` environment by extending the function `RnNames.getLocalDeclBinders` such that it traverses instance declarations, too.  We are careful not to add the data type constructor multiple times by ignoring them in instance declarations.  The global `RdrName` environment only ever contains the type constructor introduced in the class declaration (i.e, the `RdrName` of an associated data type maps to the `Name` of the AT declaration in the class).

---

**Remaining problem:** The function `getLocalDeclBinders` must still supply the parent `Name` to the name generation for the data constructors. That parent name should be the one produced for the associated data declaration in the corresponding class declaration, which is hard to get hold of at this moment. So, we supply the Name of the data type constructor instead. That should probably be replaced by the class name in a later phase.

---


Otherwise, `RnSource.rnSrcInstDecl` invokes `RnSource.rnTyClDecl` on all associated types of an instance to rename them.

#### Lifting of associated type definitions out of instances


In the current implementation, `RnSource.rnSrcDecl` (which is only called by `RnSource.rnSrcDecls`) duplicates all definitions of associated types **after** renaming them.  It does so by adding them to the type and class declarations (i.e., `hs_tyclds`) of the currently processed binding group, but also keeps a copy in the instance declarations, were they are needed during type checking to perform some well-formedness checks (e.g., that each AT of a class receives a definition).
 
NB: Lifted associated type declarations inherit the context of the instance head. However, the variables of the data declaration are renamed independently of those of the instance head (which implies that the inherited copy of the instance context is renamed again as part of the data declaration).

---

**Open Points:**

- Do we really want to copy associated types in `rnSrcDecl` into the toplevel of the binding group?  On one hand, general GHC design priciples discourages moving any code around before type checking has been completed.  On the other hand, by lifting data declarations out before type checking, we have to worry less about phasing.  (NB: Associated type signatures in class declarations are less of an issue as classes are very much treated like type declarations anyway - being in `TyClDecl` and all - and so are usually around when we need to get at their embedded types.)
- In case, we leave the duplication of ATs after renaming as it is, do we still want to add the context to lifted AT definitions?  Strictly speaking, this is not necessary under the new translation scheme.  However, morally it might still be the right thing, as the constructors are declared under that context.

---

### Type checking indexed data types


Type checking in the presence of only indexed data and newtypes is much simpler than in the presence of type functions as type equality remains purely syntactic (i.e., we do not need to change the unification procedure).  However, we need to check that the alternatives of a case expression inspecting an indexed data type contains only constructors of one member of the family.  (To relax this restriction, we would need a story for compiling open data types.)

#### Desugaring indexed data types


The kind signature of an indexed data type

```wiki
data T (a1::<kind1>) .. (an::<kindn>) :: <kind>
```


turns into an F<sub>C</sub> type function declaration

```wiki
type T_n : <kind1> -> .. -> <kindn> -> <kind>
```


A member of an indexed data type

```wiki
data T t1 .. tn b1 .. bm = <constructors>
```


turns into an equality axiom and a vanilla data declaration

```wiki
axiom cTinst : (forall c1..cr. T_n t1 .. tn) :=: (forall c1..cr. Tinst c1 .. cn)
data Tinst c1 .. cr b1 .. bm = <constructors>
```


where the `ci` are the free variables of the `tj`.  Moreover, we replace all occurences of `T` in the rest of the program by `T_n`.

#### Inserting coercions


To ensure that the F<sub>C</sub> code generated by the above desugaring still type checks, we need to introduce cast expressions using `cTinst` to move between the indexed type `T_n` and the representation types, such as `Tinst`, of its members.  The simplicity of type checking and desugaring indexed data types - as opposed to general type functions - is due to the locations where these casts need to be added being well defined.  More precisely, there are two different kinds of locations corresponding to the introduction and elimination of indexed data types:

1. Wrappers for data constructors introduce indexed types.
1. Case expressions scrutinising indexed types eliminate them.

#### Wrappers for indexed types

### Type checking associated types

#### Class declarations

- As part of the knot tying exercises in `TcTyClsDecls.tcTyAndClassDecls`, we extract all AT declarations from classes and add them to the list of class and data type declarations to be processed. This ensures that AT declarations participate in kind checking and that they are entered into the global type-checker environment.
- We do *not* update the data declarations within class declarations (field `tcdATs` within `ClassDecl`), as the `Class.Class` structure produced by the type checker only contains the `Id`s of a classes associated types.
- We check that we have -fglasgow-exts.

#### Instance declarations


We need to handle ATs in `TcInstDcls.tcInstDecls1`, which is where the type information in instances - i.e., in vanilla Haskell just the instance heads - are processed.

### Representation of type functions after type checking


Type functions have a number of properties in common with class instances; in particular, they require a machinery for matching type patterns against types, as instance heads do during context simplification.  Hence, we probably want some structure similar to `InstEnv.Instance` for type functions - for instances this is maintained in the field `iSpec` of `TcEnv.InstInfo` (for type functions we don't need anything like `iBinds` as they are pure type-level entities).  If possible, it would be ideal if we can reuse (or generalise) some of the matching machinery for instance heads.


The essentials of a module after type checking are in `HscTypes.ModGuts`; in particular, we have two fields `mg_insts :: [Instance]` and `mg_binds :: [CoreRule]` containing all instance heads and all rewrite rules respectively.  Similarly, we now want something like `mg_tyequa :: [TyEqua]` to represent all type euqations.  

## Possible Extensions

- Our type-indexed data types are open.  However, we currently don't allow case expressions mixing constructors from different indexes.  We could do that if we had a story for open function definitions outside of classes.
