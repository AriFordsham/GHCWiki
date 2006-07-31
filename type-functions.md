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

`HsDecls.TyClDecl` has a new variant `HsFunction` to represent signatures of type functions.  These consist of the name, type parameters, an iso flag, and optionally an explicit result kind.  The type parameters can have kind signatures as usual.

#### Type function equations and definitions of associated data types


More tricky is the addition of type indexes (i.e., non-type variable arguments) to data type declarations. The grammar is already very general and allows arbitrary arguments, but the parser uses `RdHsSyn.checkTyClHdr` to construct the AST and that function ensures that only type variables are supplied. The new story is that `checkTyClHdr` can operate in two different modes: (1) checking mode and (2) extraction mode. Checking mode corresponds to the original behaviour. In extraction mode, all free type variables of the arguments will be collected, but we don't enforce that the arguments are themselves merely type variables. When processing class headers (and for the moment also type synonyms), we use `checkTyClHdr` in checking mode as before. However, when processing a data type (or newtype) declaration, we use extraction mode and keep both the list of type variables (as `tcdTyVars`) and the original arguments (as `tcdTyPats`) in the representation of data type declarations (i.e., in the variant `TyData` of `TyClDecl`). The check enforcing that all arguments to top-level data type declarations and the non-class parameter arguments of associated data types are variables is delayed until the renamer (as we need context information). In the renamer, we check that non-variable type parameters can only occur in the first few arguments of ATs and we remove these parameters by floating the associated data type declarations to the top-level.


In the parser, we put the original type terms specified as parameters in the field `tcdTyPats`. For top-level declarations, after checking that the parameters are all plain type variables (possibly with a kind signature), we reset `tcdTyPats` to `Nothing` (this already happens during AST construction). `DataDecls` created during parsing Core are already born with `tcdTyPats` being Nothing. (Although, the latter may change.)

### Representation of associated types


Adding types declarations to classes is fairly straight forward. The `ClassDecl` variant of `TyClDecl` gets a new field `tcdATs`, which contains a list of type declarations - currently, the parser will only allow data type declarations. Similarly, `InstDecl` gets a fourth argument, which is a list of type declarations.

### Phasing


GHC is organised such that class and type declarations are processed (during renaming and type checking) before any instance declarations are considered. The problem now is that instance declarations may contain type declarations; hence, anything that may depend on a type declaration can now also depend on an instance declaration. We solve that by lifting associated data types out of instances before renaming (and hence also before type checking of type and class declarations).

### Renaming and extraction of associated data types


Before the associated data type declarations are lifted out of the defining instances, we enter the names of all data constructors that an associated data type defines into the global `RdrName` environment by extending the function `RnNames.getLocalDeclBinders` such that it traverses instance declarations, too. We are careful not to add the data type constructor multiple times. In fact, it is ignored in instance declarations. The global `RdrName` environment only ever contains the type constructor introduced in the class declaration (i.e, the `RdrName` of an associated data type maps to the `Name` of the AT declaration in the class).

---

**Remaining problem:** The function `getLocalDeclBinders` must still supply the parent `Name` to the name generation for the data constructors. That parent name should be the one produced for the associated data declaration in the corresponding class declaration, which is hard to get hold of at this moment. So, we supply the Name of the data type constructor instead. That should probably be replaced by the class name in a later phase.

---


Now, we can extract the associated data type declarations out of instances in `RnSource.rnSrcDecl`, which is only called by `RnSource.rnSrcDecls`. As part of the extraction process, we also call `RnSource.rnTyClDecl` on each AT declaration to obtain the renamed form of these declarations. We add these renamed forms to the type and class declarations (i.e., `hs_tyclds`) of the currently processed binding group, but also keep a copy in the instance declarations, were they are needed during type checking to perform some well-formedness checks (e.g., that each AT of a class receives a definition). NB: Lifted associated type declarations inherit the context of the instance head. However, the variables of the data declaration are renamed independently of those of the instance head (which implies that the inherited copy of the instance context is renamed again as part of the data declaration).


During renaming, AT declarations in classes are checked for being empty (i.e., no constructors and no context) and for conformance of the type parameters with those of the class. We also check all parameters are type variables, and we inherit kind signatures from the corresponding class parameters (if any of these already have kind signatures, we raise an error) - in fact, we inherit the `Name`s of the class parameters. Afterwards, `tcdTyPats` is reset to `Nothing`.

### Type checking associated data types


Type checking in the presence of only associated data types is much simpler than in the presence of associated type synonyms (or general type functions) as type equality remains purely syntactic (i.e., we do not need to change the unification procedure).  However, we need to check that the alternatives of a case expression inspecting an associated data type contains only constructors defined within the same instances.  (To relax this restriction, we would need a story for compiling open data types.)


Class declarations:

- As part of the knot typing exercises in `TcTyClsDecls.tcTyAndClassDecls`, we extract all AT declarations from classes and add them to the list of class and data type declarations to be processed. This ensures that AT declarations participate in kind checking and that they are entered into the global type-checker environment.
- We do *not* update the data declarations within class declarations (field `tcdATs` within `ClassDecl`), as the `Class.Class` structure produced by the type checker only contains the `Id`s of a classes associated types.
- We check that we have -fglasgow-exts.

## Possible Extensions

- Toplevel *data* types with type patterns can probably be type checked just like GADTs.  However, they would also be a form of open data types, unless we restrict their values to be scrutinised in class instances with matching type restrictions.
