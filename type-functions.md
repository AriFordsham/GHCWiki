# Type Functions and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type functions and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.

## Aims


New features:

- Toplevel type function definitions.
- Associated data types and type synonyms in classes, where the latter are eseentially type function definitions spread across the instances of the associated class.  Associated types are essentially syntactic sugar for general type functions.


Revised features

- We may want to re-implement functional dependencies using associated type synonyms.


We keep track of the current [implementation status](type-functions-status).

## Specification and Restrictions


Refinement of the specification in the *Beyond Associated Types* paper.  (I'll actually link this paper here once it is a bit more coherent.)

- The degenerate case of a type equation where all type arguments are variables is valid without a kind signature and coincides with the type synonyms of vanilla Haskell.  In fact, for the moment, we do not allow the degenerate case to have a kind signature.  The latter constraint could be dropped if it proves to be inconvenient.  (Rationale: Multiple type equations are useless when one is degenerate - as the whole system needs to be confluent and we don't have sequential matching on type equations.  So, we get backwards compatibility for free.)
- Kind signatures can only occur on the type variables that are in excess of the class parameters in an associated type declaration (in a type class declaration). Rationale: The binding position for the class parameters is the class head. That's where the signatures should be.
- Associated data type definitions (in instances) can have kind signatures at type variables occuring in the head. These signatures must coincide with those of the instance head (for the class parameters) and those of the associated data type declarations (for the excess parameters). Rationale: In contrast to class declarations, we don't regard the instance head as binding variables in the body.
- The declaration of an associated data type in a class cannot have a context. Rationale: We don't want a context constraining class parameters for the same reason that we don't want that on function signatures. A context on additional arguments to the data declaration would be feasible, but doesn't seem worth the trouble.  **This is a pre-F<sub>C</sub> restriction that needs to be removed from the current code base, before being taken off the wiki.**
- The declaration of an associated data type in a class can have a deriving clause. The meaning is that all instances of that type inherit all these derivings (or do we merely want to force them to state - at least - these derivings). Rationale: If I want equality on an associated type, we need to guarantee that all its variants come with an equality.


Restrictions:

- We currently don't allow associated GADTs. I cannot see any fundamental problem in supporting them, but I want to keep it simple for the moment. (When allowing this, a constructor signature in an associated GADT can of course only refine the instantiation of the type arguments specific to the instance in which the constructor is defined.)
- We currently don't have toplevel data definitions with type patterns.  They would essentially be open GADTs, which we probably can type check with the existing GADT machinery and translate much as we translate associated data types in classes.  Again, I want to avoid doing too much in the first sweep.

## How It Works

### Syntax of type functions


Type function (kind) signatures are represented by the new declaration form `TyFunction` (of `HsDecls.TyClDecl`).  Syntactically, we recognise kind signatures by either not having an RHS at all (in which the result kind implicitly is \*) or having a result kind separated from the head by `::`.  We require that every type equation has a kind signature in scope.  However, the degenerate case of a type equation where all type arguments are variables is valid without a kind signature (in fact, it may not have any) and coincides with the type synonyms of vanilla Haskell.

### Representation of indexed types

#### Type function signatures

`HsDecls.TyClDecl` has a new variant `TyFunction` to represent signatures of type functions.  These consist of the name, type parameters, an iso flag, and optionally an explicit result kind.  The type parameters can have kind signatures as usual.

#### Type function equations and associated data types


To represent type functions and associated data types, we need to generalise data type declarations `TyData` and type synonym declarations `TySynonym` to allow type patterns instead of just type variables as parameters.  We do so by way of the field `tcdPats` of type `Maybe [LHsType name]`, used as follows:

- If it is `Nothing`, we have a *vanilla* data type declaration or type synonym declaration and `tcdVars` contains the type parameters of the type constructor.
- If it is `Just pats`, we have the definition of an associated data type or a type function equations (toplevel or nested in an instance declarations).  Then, 'pats' are type patterns for the type-indexes of the type constructor and `tcdVars` are the variables in those patterns.  Hence, the arity of the type constructor is `length tcdPats` and \*not\* `length tcdVars`.


In both cases (and as before type functions), `tcdVars` collects all variables we need to quantify over.

#### Parsing and AST construction


The LALR parser allows arbitrary types as left-hand sides in **data** and **type** declarations.  The parsed type is, then, passed to `RdHsSyn.checkTyClHdr` for closer analysis (possibly via `RdHsSyn.checkSynHdr`).  It decomposes the type and, among other things, yields the type arguments in their original form plus all type variables they contain.  Subsequently, `RdrHsSyn.checkTyVars` is used to either enforce that all type arguments are variables (second argument is `False`) or to simply check whether the type arguments are variables (second argument `True`).  If in enforcing mode, `checkTyVars` will raise an error if it encounters a non-variable (e.g., required for class declarations).  If in checking mode, it yields the value placed in the `tcdPats` field described above; i.e., returns `Nothing` instead of the type arguments if these arguments are all only variables.


NB: Some well-formedness checks are left for the renamer to do.  For example, we don't enforce at this point that toplevel data declarations use variable-only heads (as this requires context information not available during parsing).

### Representation of associated types


We add type declarations to class declarations and instance declarations by a new field (of type `[LTyClDecl]`) to both `TyClDecl.ClassDecl` (known by the field name `tcdATs`) and `TyClDecl.InstDecl`.  For classes, this new field contains values constructed from `TyData`, `TyFunction`, and `TySynonym`, whereas for instances, we only have `TyData` and `TySynonym`.  This is due to (a) `TyData` representing both signatures and definitions of associated data types (whereas the two are split into `TyFunction` and `TySynonym` for associated synonyms) and (b) associated synonyms having default definitions, which associated data types do not possess.

### Phasing


GHC is organised such that class and type declarations are processed (during renaming and type checking) before any instance declarations are considered.  In the presence of associated types, instance declarations may contain type definitions.  In particular, the *data constructors* introduced by associated data declarations need to be brought into scope before we can rename any expressions.

---

**Open Point:** When exactly do we want to lift associated data declarations out of instances?  On one hand, general GHC design priciples discourages moving any code around before type checking has been completed.  On the other hand, by lifting data declarations out before type checking, we have to worry less about phasing.  (NB: Associated type signatures in class declarations are less of an issue as classes are very much treated like type declarations anyway - being in `TyClDecl` and all - and so are usually around when we need to get at their embedded types.)

---

### Renaming and extraction of associated data types


During renaming, we enter the names of all data constructors that an associated data type defines into the global `RdrName` environment by extending the function `RnNames.getLocalDeclBinders` such that it traverses instance declarations, too.  We are careful not to add the data type constructor multiple times by ignoring them in instance declarations.  The global `RdrName` environment only ever contains the type constructor introduced in the class declaration (i.e, the `RdrName` of an associated data type maps to the `Name` of the AT declaration in the class).

---

`Revise from here!`

---

---

**Remaining problem:** The function `getLocalDeclBinders` must still supply the parent `Name` to the name generation for the data constructors. That parent name should be the one produced for the associated data declaration in the corresponding class declaration, which is hard to get hold of at this moment. So, we supply the Name of the data type constructor instead. That should probably be replaced by the class name in a later phase.

---


Now, we can extract the associated data type declarations out of instances in `RnSource.rnSrcDecl`, which is only called by `RnSource.rnSrcDecls`. As part of the extraction process, we also call `RnSource.rnTyClDecl` on each AT declaration to obtain the renamed form of these declarations. We add these renamed forms to the type and class declarations (i.e., `hs_tyclds`) of the currently processed binding group, but also keep a copy in the instance declarations, were they are needed during type checking to perform some well-formedness checks (e.g., that each AT of a class receives a definition). NB: Lifted associated type declarations inherit the context of the instance head. However, the variables of the data declaration are renamed independently of those of the instance head (which implies that the inherited copy of the instance context is renamed again as part of the data declaration).


During renaming, AT declarations in classes are checked for being empty (i.e., no constructors and no context) and for conformance of the type parameters with those of the class. We also check all parameters are type variables, and we inherit kind signatures from the corresponding class parameters (if any of these already have kind signatures, we raise an error) - in fact, we inherit the `Name`s of the class parameters. Afterwards, `tcdTyPats` is reset to `Nothing`.  **!!!The parser now already puts Nothing in when all parameters are variables.  Hence, we can simplify the check and don't need to reset anything.!!'''
**

### Type checking associated data types


Type checking in the presence of only associated data types is much simpler than in the presence of associated type synonyms (or general type functions) as type equality remains purely syntactic (i.e., we do not need to change the unification procedure).  However, we need to check that the alternatives of a case expression inspecting an associated data type contains only constructors defined within the same instances.  (To relax this restriction, we would need a story for compiling open data types.)


Class declarations:

- As part of the knot typing exercises in `TcTyClsDecls.tcTyAndClassDecls`, we extract all AT declarations from classes and add them to the list of class and data type declarations to be processed. This ensures that AT declarations participate in kind checking and that they are entered into the global type-checker environment.
- We do *not* update the data declarations within class declarations (field `tcdATs` within `ClassDecl`), as the `Class.Class` structure produced by the type checker only contains the `Id`s of a classes associated types.
- We check that we have -fglasgow-exts.

## Possible Extensions

- Toplevel *data* types with type patterns can probably be type checked just like GADTs.  However, they would also be a form of open data types, unless we restrict their values to be scrutinised in class instances with matching type restrictions.
