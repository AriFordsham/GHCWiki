[TypeFunctions](type-functions)/Status

# Type Functions: Implementation Status

**Open Trac bugs related to type families**

- [\#1737](https://gitlab.haskell.org//ghc/ghc/issues/1737) (is this related to the other optimisation-related problems with cast?)
- [\#1715](https://gitlab.haskell.org//ghc/ghc/issues/1715) (iface problem, which is tricky to reproduce)
- [\#1738](https://gitlab.haskell.org//ghc/ghc/issues/1738) (GADTs with equalities, only fails **without** profiling)
- [\#1722](https://gitlab.haskell.org//ghc/ghc/issues/1722) (type families & GADTs) \[look at when GADTs are implemented by equalities\]
- [\#1723](https://gitlab.haskell.org//ghc/ghc/issues/1723) (type families & GADTs) \[will be fixed when GADTs are implemented by equalities; we'll want to add the test case to the testsuite\]

**Failing testsuite tests**


All these tests are in `testsuite/tests/ghc-regress/indexed-types`:

- `should_run/GMapAssoc(optc,hpc,profc,profasm)` (data type families)
- `should_run/GMapTop(optc,hpc,profc,profasm)` (data type families)
- `should_run/ind2(profc,profasm)` (data type families)
- `should_compile/GADT3(profc,profasm)` (type syonym families) -- unexpected pass
- `should_run/Simple12(normal,optc,profc,profasm)` (type synonym families)

**Debugging of type families:**

1. `substEqInDict` needs to be symmetric (i.e., also apply right-to-left rules); try to re-use existing infrastructure.  It would be neater, easier to understand, and more efficient to have one loop that goes for a fixed point of simultaneously rewriting with given_eqs, wanted_eqs, and type instances.
1. skolemOccurs for wanteds?  At least `F a ~ [G (F a)]` and similar currently result in an occurs check error.  Without skolemOccurs in wanted, the occurs check for wanted would need to be smarter (and just prevent cyclic substitutions of the outlined form silently).  However, when inferring a type, having the rewrites enabled by skolemOccurs available will leads to potentially simpler contexts.
1. `:t` in ghci doesn't print equalities in contexts properly.
1. ghci command to print normalised type and add [ http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799](http://article.gmane.org/gmane.comp.lang.haskell.cafe/28799) as a test to the testsuite.
1. To move GADT type checking from refinements to equalities, proceed as follows (as suggested by SPJ):

  - Implemented this as follows in `TcPat.tcConPat:579:`

    ```wiki
    - 	      eq_spec' = substEqSpec tenv eq_spec
    +	      eq_spec' = []
    +              eq_preds = [mkEqPred (mkTyVarTy tv, ty) | (tv, ty) <- eq_spec]
    +	      theta'   = substTheta  tenv (eq_theta ++ dict_theta ++ eq_preds)
    ```
  - Results:

    - Works in principle.
    - Immediately fixes the tests GADT3, GADT4 & GADT5.
    - Unfortunately, it breaks a whole lot of tests in `gadt/`.
    - The remaining problems are partially due to (1) the splitBoxyXXX function issue mentioned above, (2) the occurs check issue mentioned below, (3) the same problem exhibited by GADT9 (with or without this change), (4) some problems getting hold of the right given class constraints, and (5) some random stuff that I haven't looked at more closely.
  - Handling of cases expression scrutinising GADTs: 

    - implement proposal where we infer a rigidity flag for case scutinees and pass that down when type checking the patterns,
    - if a pattern has a GADT constructor (ie, any constraints in the data constructor signature), the scutinee must be rigid,
    - we  need to know of types whether they are rigid (not only whether they contain unification variables, but by a flag in the environment that indicates whether the computation of that type involved non-rigid type variables)
  - In `TcUnify`, make all occurs checks more elaborate.  They should only **defer** if the checked variable occurs as part of an argument to a type family application; in other cases, still fail right away.  DONE?
  - `TcGadt.tcUnifyTys` can now probably be replaced again by the non-side-effecting unifier that was in `types/Unify.hs` (recover from previous repo states).
1. Check that the restrictions on equality constraints in instance and class contexts are enforced.  We should have tests for that in the testsuite.  Document the exact restrictions on the Haskell wiki tutorial page.
1. When can foralls appear in equalities?  What constraints does that place on GADTs?  Also, the code in `TcTyFuns` doesn't really deal with rank-n types properly, esp `decompRule`.
1. To fix `Simple8`:

  - Fix tcLookupFamInst to gracefully handle this case.  (This requires some care to not violate assumptions made by other  clients of this function, as it is also used for data families,  but I see no fundamental problem.)
  - Issue a warning if there are two identical instances (as per  Roman's suggestion).
1. CONCEPTUAL issue: At least with `skolemOccurs`, the policy of not zonking the types embedded in the kinds of coercion type variables does no longer work.  This becomes, for example in the test `Simple13`, apparent.  The skolem introduced in `skolemOccurs` finds its way into variable kinds (which is visible when inspecting them during `TcMType.zonk_tc_tyvar`).
1. When `Simple13` is compiled with a compiler that was built with `-DDEBUG`, it prints a warning about not matching types being used during constructing a trans coercion.
1. To fix superclass equalities (specifically getting the coercion evidence), we could introduce a kind of typelet just for evidence.  In fact, re-use HsBind.VarBind and make its right-hand side a specially data structure describing evidence construction, instead of being a general HsExpr.  That evidence construction generation can have a case for extracting superclass constraints.  The desugarer than has to generate the case expression bringing the equality in scope from that.
1. In `TcTyFuns.genericNormaliseInst`, we need to figure out what to do with `ImplicInst`, `Method`, and `LitInst` dictionaries.
1. ghc falls over if a bang pattern is put at an argument of type `F a`.
1. Fix export list problem (ie, export of data constructors introduced by orphan data instances):

  - Change `HscTypes.IfaceExport` to use `Name` instead of `OccName`.
  - Then, there is also no need for the grouping of the identifiers by module anymore (but sort it to avoid spurious iface changes dur to re-ordering when re-compiling).
  - We still need to have the name parent map, though.
  - See email for example.
1. Allow data family GADT instances.
1. Fix everything in the testsuite.
1. Can't we now allow non-left-linear declarations; e.g., `instance type F a a = ..`?
1. Fix core-lint breakage in cholewo-eval.
1. The tests `tcfail068` and `rw` used to raise more type errors right away.  Now, we see less recovery.
1. What about filtering the `EqInst`s in `TcSimplify.addSCs`.  We need them, don't we?  But they give rise to `Var`s, not `Id`s, and we haven't got selectors.

**Current:**

- Add some trac wiki documentation of how inference with type families works.

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
- Deriving `Typeable` for data families.
- If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)


Todo (high-level): 

1. Type checking of type families; routines in `TcUnify` that still need to be extended:

  - `boxySplitTyConApp`: The second argument (`BoxyRhoType`) can be a synonym family application.  Then, we must produce a wanted coercion and return a `HsWrapper` value that applies that coercion.
  - `boxySplitAppTy`: Basically, the same deal as the previous.
1. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
1. Type check functional dependencies as type functions.


Done: 

- Kind and type checking of kind signatures.
- Kind and type checking of instance declarations of indexed types, including the generation of representation tycons.
- Wrapper generation and type checking of pattern matching for indexed data and newtypes.
- Consistency checking for family instances.
- Enforce syntactic constraints on type instances needed to ensure the termination of constraint entailment checking.

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
