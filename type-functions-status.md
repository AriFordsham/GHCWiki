# Type Functions: Implementation Status


Back to [TypeFunctions](type-functions).

**Current:**

- Type checking of kind signatures & testing.

## Parsing and Renaming


Todo (high-level):

1. Parse and rename equality constraints in signatures.
1. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)


Done:

- Parsing and renaming of kind signatures (toplevel and in classes).
- Parsing and renaming of indexed types declarations (toplevel and in classes).

## Type Checking


Todo (low-level):

- In `TcTyClsDecls.tcKindSigDecl`: compute resulting `TyCon`
- Applications of indexed types need to be applied to all type indexes.
- Default AT synonyms are only allowed for ATs defined in the same class.
- For each case scrutinising an associated data type, check that all constructors have been defined in a single instance.  (Maybe we can just extend the existing check that ensures that case expressions don't mix constructors of different data types.)
- Check that each AT definition mirrors the class arguments of the instance in its type indexes. This might be a bit more tricky if we want to allow that they can vary syntactically before expansion of type synonyms. (Do this in the type checker unless we find it is very hard to do there; then, revert to trying it during renaming.)
- Check that each instance has a definition for every AT and also that all defined associated types are, in fact, declared by the class. (Do this in the type checker - GHC does the corresponding checks for methods in the type checker, too.) Also check that kind signatures that correspond to type variables in the AT declaration or class declaration match the kinds inferred for the AT declaration. (This certainly needs to be done in the type checker.)
- Check that patterns of type indexes don't contain type functions.
- Constructs `InstInfo` for type equation in `tcIdxTyInstDecl1`.
- Construct representations for kind signatures in `tcTyClDecl1`.


Todo (high-level):

1. Type checking of associated data types.
1. Type checking of type functions (and hence, associated type synonyms); forget about `iso` for the moment.
1. Type check functional dependencies as type functions.


Done: 

- Kind checking of kind signatures.
- Kind and type checking of instance declarations of indexed types.

## Desugaring


Todo (low-level):

- Derivings on an associated data type *declaration* need to be inherited by all definitions of that data type in instances.


Todo (high-level):

1. Desugar associated data types.
1. Desugar type functions and equality constraints.
1. Extend interface files.

  - How do we exactly want to represent ATs in interface files?

    - SPJ pointed out that instances are maintained in `InstEnv.InstEnv` with different values for the home packages and others. The definitions of ATs may have to be maintained in a similar way, as they are also incrementally collected during compiling a program.
    - `IfaceInst` contains the instance declaration information for interfaces.
  - Export and import lists: The name lists that may appear at class imports and exports can now also contain type names, which is tricky as data type names can carry a list of data constructors.
  - At the moment, we add as the parent name of the data constructors of associated data types defined in instances, the new name for the data type constructor, which is *different* from that of the data type constructor in the class (also their source representation is the same). We may need to fix that during renaming. (We can't easily fix it in `getLocalDeclBinders`, where the names of the data constructors are made, as we don't have the means to get at the right class at that point.)


Done: Nothing.

## Testsuite


Todo:

- Convert AT.hs to tests in the testsuite.
