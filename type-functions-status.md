# Type Functions: Implementation Status


Back to [TypeFunctions](type-functions).

**Current:**

- Next: (1) implicit import and export of data/newtype instances; (2) check for overlapping instances.
- Also add a `-findexed-types`.

## Parsing and Renaming


Todo (low-level):

- Should family declarations be optional with ATs, too?  (See comment at patch making kinds optional at toplevel declarations.)


Todo (high-level):

1. Parse and rename equality constraints in signatures.
1. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)
1. Import/export lists:

  - We need to be able to write something like `GMapKey(GMap,empty)`.
  - Export and import of data constructors declarated in `data instance`s.  We should be able to use the same syntax for the entity specs as for closed data types, but the meaning is somewhat different.


Done:

- Parsing and renaming of kind signatures (toplevel and in classes).
- Parsing and renaming of indexed type declarations (toplevel and in classes).
- Using new syntax with `family` and `instance` on top level.

## Type Checking


Todo (low-level):

- data/newtype instances may not overlap.  (Such definitions would always be non-confluent.)  We might be able to get away with a lazy check at every place where a value of family type is constructed (i.e., occurences of the datacon wrapper).  Such a value may never be an inhabitant of more than one instance declaration.  No, we won't get away with this...
- RHS of a `type instance` must be a tau type.
- Check that patterns of type indexes don't contain type functions.
- Construct `InstInfo` for type equation in `tcIdxTyInstDecl1`.
- If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)


Todo (high-level):

1. Type checking of type functions (and hence, associated type synonyms); forget about `iso` for the moment.
1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type check functional dependencies as type functions.


Done: 

- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types.
- Wrapper generation and type checking of pattern matching for indexed data and newtypes.

## Desugaring


Todo (low-level):

- `data instances` and `newtype instances` need to be implicitly exported as are class instances.
- Derivings on an associated data type *declaration* need to be inherited by all definitions of that data type in instances.


Todo (high-level):

1. Extend interface files to include equality axioms:

  - How do we exactly want to represent type equations in interface files?

    - SPJ pointed out that instances are maintained in `InstEnv.InstEnv` with different values for the home packages and others. Type instances may have to be maintained in a similar way, as they are also incrementally collected during compiling a program.  (We probably include them in the same structure, as they will also be of type `InstInfo`.)
    - `IfaceInst` contains the instance declaration information for interfaces.
  - Export and import lists: The name lists that may appear at class imports and exports can now also contain type names, which is tricky as data type names can carry a list of data constructors.

1. Desugar type functions and equality constraints.


Done:

- Representation of family kind signatures as `TyCon.TyCon`s.
- Extension of `Class.Class` by associated `TyCon`s.
- Extension of `TyCon.TyCon` with a reference to the parent `TyCon` for data instances.
- Extension of `DataCon.DataCon` with instance types for constructors belonging to data instances.
- Extension of `TyCon.TyCon` such that the parent of a data instance is paired with a coercion identifying family instance and representation type.
- For indexed data types, the datacon wrapper uses data instance coercion and pattern matching casts the scrutinee via an `ExprCoFn` in a `CoPat`.

## Testsuite


Todo:

- Compile libraries with CoreLint.
- Convert `TyFuns.hs` to tests in the testsuite.
