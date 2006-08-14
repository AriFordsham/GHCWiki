# Type Functions: Implementation Status


Back to [TypeFunctions](type-functions).

**Current:**

- Compute `TyCon`s resulting from type instance declarations; then, test type checking of declarations of indexed types.

## Parsing and Renaming


Todo (high-level):

1. New syntax for indexed types: `type/data/newtype family` and `type/data/newtype instance`.  Then, the kind can be optional again (with `*` being the default).
1. Parse and rename equality constraints in signatures.
1. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)


Done:

- Parsing and renaming of kind signatures (toplevel and in classes).
- Parsing and renaming of indexed types declarations (toplevel and in classes).

## Type Checking


Todo (low-level):

- Check that the arguments of AT instances coincide with the respective instance arguments of their class. This might be a bit more tricky if we want to allow that they can vary syntactically before expansion of type synonyms.
- Check that each class instance has a definition for every AT and conversely that that all defined associated types are, in fact, part of the class - with the exception of associated synonyms with a default definition. (Do this in the type checker - GHC does the corresponding checks for methods in the type checker, too.)
- Check that patterns of type indexes don't contain type functions.
- For each case scrutinising an associated data type, check that all constructors have been defined in a single instance.  (Maybe we can just extend the existing check that ensures that case expressions don't mix constructors of different data types.)
- Construct `InstInfo` for type equation in `tcIdxTyInstDecl1`.


 
Todo (high-level):

1. Type checking in the presence of associated data types.
1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type checking of type functions (and hence, associated type synonyms); forget about `iso` for the moment.
1. Type check functional dependencies as type functions.


Done: 

- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types.

## Desugaring


Todo (low-level):

- Derivings on an associated data type *declaration* need to be inherited by all definitions of that data type in instances.


Todo (high-level):

1. Desugar indexed data types.
1. Extend interface files.

  - How do we exactly want to represent ATs in interface files?

    - SPJ pointed out that instances are maintained in `InstEnv.InstEnv` with different values for the home packages and others. The definitions of ATs may have to be maintained in a similar way, as they are also incrementally collected during compiling a program.
    - `IfaceInst` contains the instance declaration information for interfaces.
  - Export and import lists: The name lists that may appear at class imports and exports can now also contain type names, which is tricky as data type names can carry a list of data constructors.
  - At the moment, we add as the parent name of the data constructors of associated data types defined in instances, the new name for the data type constructor, which is *different* from that of the data type constructor in the class (also their source representation is the same). We may need to fix that during renaming. (We can't easily fix it in `getLocalDeclBinders`, where the names of the data constructors are made, as we don't have the means to get at the right class at that point.)
1. Desugar type functions and equality constraints.


Done: Nothing.

## Testsuite


Todo:

- Convert AT.hs to tests in the testsuite.
