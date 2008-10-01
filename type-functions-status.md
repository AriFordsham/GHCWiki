[TypeFunctions](type-functions)/Status

# Type Functions: Implementation Status

**Open bugs related to type families**

- Well-formedness of declarations involving families:

  - [\#1968](https://gitlab.haskell.org//ghc/ghc/issues/1968) (GADT syntax in family instances; at least provide a proper error message and don't panic!)

    - Need to check the result types of the data constructors, probably in `checkValidDataCon`.
    - `tcFamInstDecl1` needs to allow family GADT instances.
  - Allow repeated variable occurrences in lhses of type instances (see paper).
  - Check that the restrictions on equality constraints in instance and class contexts are enforced.  We should have tests for that in the testsuite.  Document the exact restrictions on the Haskell wiki tutorial page.
  - Test`Simple8`:

    - Fix tcLookupFamInst to gracefully handle this case.  (This requires some care to not violate assumptions made by other  clients of this function, as it is also used for data families,  but I see no fundamental problem.)
    - Issue a warning if there are two identical instances (as per  Roman's suggestion).
  - Addition to user manual, see [ http://www.haskell.org/pipermail/haskell-cafe/2008-March/040989.html](http://www.haskell.org/pipermail/haskell-cafe/2008-March/040989.html) and [http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html\#type-synonyms](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#type-synonyms)
  - [\#2203](https://gitlab.haskell.org//ghc/ghc/issues/2203) (TFs in class instance heads)
  - [\#2435](https://gitlab.haskell.org//ghc/ghc/issues/2435) (Bug with qualified names in declarations)
  - [\#2436](https://gitlab.haskell.org//ghc/ghc/issues/2436) (Bad warning on export)

- Solving of equalities (`TcTyFuns`):

  - [\#2448](https://gitlab.haskell.org//ghc/ghc/issues/2448) (givens not properly used in superclass entailment check)
  - [\#2102](https://gitlab.haskell.org//ghc/ghc/issues/2102) (superclass equalities)

    - To fix superclass equalities (specifically getting the coercion evidence), we could introduce a kind of typelet just for evidence.  In fact, re-use `HsBind.VarBind` and make its right-hand side a specially data structure describing evidence construction, instead of being a general `HsExpr`.  That evidence construction generation can have a case for extracting superclass constraints.  The desugarer than has to generate the case expression bringing the equality in scope from that.

- GADT:
  None.

- Misc:

  - Test `Simple17` & `GADT12` (corelint errors)
  - [\#2291](https://gitlab.haskell.org//ghc/ghc/issues/2291) (panic mixing RULES and type families; rule simplification stumbles over a coercion)
  - [\#1897](https://gitlab.haskell.org//ghc/ghc/issues/1897): If you infer a type for a function, then should check the function against that sigature, to check that if the user gave that signature, then typechecking would again succeed.  See this thread [ http://www.haskell.org/pipermail/haskell-cafe/2008-April/041385.html](http://www.haskell.org/pipermail/haskell-cafe/2008-April/041385.html).
  - [\#1769](https://gitlab.haskell.org//ghc/ghc/issues/1769) (deriving typeable for data families)
  - When a `type instance` changes (in an orphan modules), currently clients are not properly recompiled at least by `--make`.
  - [\#2296](https://gitlab.haskell.org//ghc/ghc/issues/2296): error message involving fundep gives unhelpful location.  I want to remember to come back to this one when we have the new type-family simplification stuff in place.

**Additional feature:**

- [\#2101](https://gitlab.haskell.org//ghc/ghc/issues/2101)
- Total families
- Test `DerivingNewType`
- Implementing FDs by TFs:

  - Step 1: Replace the existing improvement machinery for FDs by code that generates explicit equalities from the two FD rules.  Then, all improvement is by normalisation of equalities, which hopefully allows us to simplify `TcSimplify.reduceContext`.
  - Step 2: Desugar FDs into TFs and superclass equalities.
  - ghci command to print normalised type and add [ http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799](http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799) as a test to the testsuite.

**Debugging of type families:**

1. Open points in the new `TcTyFuns` code:

  - extract from `TODO`s in `TcTyFuns`
1. Issues in `TcSimplify`:

  - Why does the call to `reduceList` (in `reduceContext`) extend the LIE?  What are the produced `extra_eqs`?  (Put an assert there and run the testsuite to see whether `extra_eqs` is non-empty in any of the tests.)
1. Replacing GADT refinements by explicit equality constraints:

  - Regressions that remain to be fixed: 

    - `gadt/lazypatok` needs to be fixed (are irrefutable patterns really ok, see [ http://okmij.org/ftp/Haskell/GADT-problem.hs](http://okmij.org/ftp/Haskell/GADT-problem.hs)\]?)
    - Error message of `tcfail167` should include "Inaccessible case alternative: Can't match types `Char' and `Float'" again
  - Handling of cases expression scrutinising GADTs: 

    - See also test `GADT7`
    - Remove the dodgy rigidity test that is in `tcConPat` right now.
    - implement proposal where we infer a rigidity flag for case scutinees and pass that down when type checking the patterns,
    - We infer the rigidity flag for the case scrutinee by generalising its type and checking whether that has an foralls at the top.  It's rigid if it has no foralls.
    - if a pattern has a GADT constructor (ie, any constraints in the data constructor signature), the scutinee must be rigid,
    - we  need to know of types whether they are rigid (not only whether they contain unification variables, but by a flag in the environment that indicates whether the computation of that type involved non-rigid type variables)
  - Re `tcfail167`, SPJ proposes that could generate a better error message, at least most of the time.  If the "expected type" of a pattern is 's', and we meet a constructor with result type (T t1 ..tn), then one could imagine a 2-step process:

    1. check that 's' is (or can be made to be) of form (T ....)
    1. check that the ... can be unified with t1..tn
      If (1) succeeds but (2) fails, the alternative is in accessible.  Of course, (2) might fail "later" by generating a constraint that later can't be satisfied, and we won't report that well, but we'd get a good message in the common fails-fast case.  We could even improve the message from (1) to say: "Constructor C is from data type T, but a pattern of type s is expected.
1. Comments:

  - When we raise a mismatch error in `TcSimplify` for unresolvable equalities, we effectively tidy the two non-matching types twice.  Add a comment to highlight this and say way it is ok (i.e., they are never grouped together with `groupErrs` or similar).
1. RankN: When can foralls appear in equalities?  What constraints does that place on GADTs?  Also, the code in `TcTyFuns` doesn't really deal with rank-n types properly, esp `decompRule`.  Also test `Simple14` & `GADT10`.
1. Fix export list problem (ie, export of data constructors introduced by orphan data instances):

  - Change `HscTypes.IfaceExport` to use `Name` instead of `OccName`.
  - Then, there is also no need for the grouping of the identifiers by module anymore (but sort it to avoid spurious iface changes dur to re-ordering when re-compiling).
  - We still need to have the name parent map, though.
  - See email for example.
1. Eliminate code duplication between `tcTyClDecl1` and `tcFamInstDecl1`.  The code for vanilla data/newtype declarations and the code for data/newtype instances has many commonalities.
1. Fix everything in the testsuite.
1. What about filtering the `EqInst`s in `TcSimplify.addSCs`.  We need them, don't we?  But they give rise to `Var`s, not `Id`s, and we haven't got selectors.
1. Consider

  ```wiki
  type family F a
  data T a b = MkT1 { fa :: F a, fb :: b }
  upd t x = t { fb = x }
  ```

  What is the most general type of `upd`?  It's

  ```wiki
  upd :: (F a ~ F d) => T a b -> c -> T d c
  ```

  However, we currently insist on the less general

  ```wiki
  upd :: T a b -> c -> T a c
  ```

  It seems a bit complicated to come up with the most general type.  The relevant code is in `TcExpr.tcExpr` in STEP 4 of the `RecordUpd` case.

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

- Allow data family GADT instances.
- Deriving `Typeable` for data families ([\#1769](https://gitlab.haskell.org//ghc/ghc/issues/1769))
- If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)


Todo (high-level): 

1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type check functional dependencies as type functions.


Done: 

- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types, including the generation of representation tycons.
- Wrapper generation and type checking of pattern matching for indexed data and newtypes.
- Consistency checking for family instances.
- Enforce syntactic constraints on type instances needed to ensure the termination of constraint entailment checking.
- Equality constraint normalisation and coercion term generation.
- GADT type checking implemented with equality and implication constraints.

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
