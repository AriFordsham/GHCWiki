# Exhaustiveness/Redundancy check implementation \[THE PAGE IS CURRENTLY UNDER CONSTRUCTION\]


This page describes how the exhaustiveness and redundancy cheker is implemented
in GHC. If you are looking for a general description of the problem and an
overview of our approach see [PatternMatchCheck](pattern-match-check).

## The `PmPat` datatype and friends


The `PmPat` data type is defined in `deSugar/Check.hs` as:

```wiki
data PmPat p = PmCon { pm_con_con     :: DataCon
                     , pm_con_arg_tys :: [Type]  -- The univeral arg types, 1-1 with the universal
                                                 --   tyvars of the constructor
                     , pm_con_tvs     :: [TyVar] -- Existentially bound type variables (tyvars only)
                     , pm_con_dicts   :: [EvVar] -- Ditto *coercion variables* and *dictionaries*
                     , pm_con_args    :: [p] }
             | PmVar { pm_var_id      :: Id }
             | PmLit { pm_lit_lit     :: PmLit } -- See NOTE [Literals in PmPat]
```


where type `PmLit` (defined in `deSugar/PmExpr.hs`) is:

```wiki
data PmLit = PmSLit HsLit                -- simple
           | PmOLit Bool  (HsOverLit Id) -- overloaded
```


Literal patterns (constructed using datacon `PmLit`) are not essential for the
algorithm to work, but, as we explain below in section `Translation`, it is
much more efficient to treat them more like constructors by matching against
them eagerly than generating equality constraints to feed the term oracle with
(as we do in the paper). `PmCon` contains several fields, effectively copying
constructor `ConPatOut` of type `Pat` (defined in `hsSyn/HsPat.hs`). Since the
algorithm runs post-typechecking, this information is crucial for the treatment
of GADTS.

### Value Abstractions


A value abstraction looks exactly like in the paper (with the exception of
literal patterns). It is essentially a `PmPat`, we simply tie the recursive
knot with a newtype declaration:

```wiki
newtype ValAbs = VA (PmPat ValAbs) -- Value Abstractions
```

### Patterns


A pattern is either a simple pattern, or a guard pattern. Hence, it lifts the `PmPat` type:

```wiki
data Pattern = PmGuard PatVec PmExpr      -- Guard Patterns
             | NonGuard (PmPat Pattern)   -- Other Patterns
```


Note that the `PmGuard` constructor takes a pattern vector and **NOT** a single
pattern. As we explain in the extended version of the paper (Appendix A, B,
give a link here), during source pattern translation, a source pattern may give
rise to a list of core patterns, by the introduction of more constraints (in
the form of guard patterns).

### Pattern and Value Vectors


A pattern vector `PatVec` and a value vector abstraction `ValVecAbs` is just a
type synonym for a list of the respective element type:

```wiki
type PatVec    = [Pattern] -- Pattern Vectors
type ValVecAbs = [ValAbs]  -- Value Vector Abstractions
```

## Value Set Abstractions


After `PmPat`, the most important data type of the checker is `ValSetAbs`, used to represent value set
abstractions. In the formalisation we treat a value set abstraction as a list of value vector abstractions
but since the vectors we generate usually have a common prefix, we save space and time by representing
them as a prefix tree. This means that we have a choice to either add the constraint set Delta only at the
leaves or have constraints in internal nodes as well. To make the most out of our representation we chose
the latter. Notionally, every path from the root to a leaf represents a value vector abstraction, where the
set Delta is the union of all constraints in the path. This means that we have the following invariant not
represented in our type:

**INVARIANT VsaArity**: The number of `Cons`s in any path to a leaf is the same. We'll refer to this as the
arity of the value set abstraction.

```wiki
data ValSetAbs
  = Empty                               -- {}
  | Union ValSetAbs ValSetAbs           -- S1 u S2
  | Singleton                           -- { |- empty |> empty }
  | Constraint [PmConstraint] ValSetAbs -- Extend Delta
  | Cons ValAbs ValSetAbs               -- map (ucon u) vs
```


where `PmConstraint`s and `PmExpr`s (defined in `deSugar/PmExpr.hs`) take the following form:

```wiki
data PmConstraint = TmConstraint Id PmExpr -- TermEq    : x ~ e
                  | TyConstraint [EvVar]   -- TypeEq    : ...
                  | BtConstraint Id        -- StrictVar : x ~ _|_

data PmExpr = PmExprVar   Id
            | PmExprCon   DataCon [PmExpr]
            | PmExprLit   PmLit
            | PmExprEq    PmExpr PmExpr  -- Syntactic equality
            | PmExprOther (HsExpr Id)    -- NOTE [PmExprOther in PmExpr]
```


Additionally, instead of using constructors `Union`, `Constraint` and `Cons` to
construct value set abstractions, we use the smart constructors instead
`mkUnion`, `mkConstraint` and `mkCons` that ensure that an empty value set
abstraction is represented only by `Empty`. We refer to this as the
**INVARIANT VsaInvariant**.


Why do we need type `PmExpr`? The term oracle (`tmOracle`, defined in
`deSugar/TmOracle.hs`) cannot handle all kinds of expressions (e.g. it doesn't
handle function applications). Hence, there is no need to work on the extremely
big `HsExpr` type (over 40 data constructors). The forms that are not handled
by the term oracle are wrapped in a `PmExprOther` and ignored by the oracle
(and the pretty-printer). Why do we keep them around then? An expression we do
not inspect can always diverge so keeping them around (or at least and
indication of failure) is essential for the laziness check.


Note that data constructor `PmExprEq` does not represent function `(==)` but is
generated by our algorithm, when we KNOW that two things are equal. E.g. when a
simple literal (say `5 :: Int`) is matched, we KNOW that `x` where
`PmExprEq False (PmExprEq x 5)` remains uncovered. So, `PmExprEq` represents
structural equality and NOT an `Eq` instance.

## Translation from Pat to Pattern


The main functions that are responsible for the translation of the type
`(Pat Id)` to our core pattern type `Pattern` are the following:

```wiki
translatePat    :: Pat Id -> UniqSM PatVec
translatePatVec :: [Pat Id] -> UniqSM [PatVec]
translateGuards :: [GuardStmt Id] -> UniqSM PatVec
translateMatch  :: LMatch Id (LHsExpr Id) -> UniqSM (PatVec,[PatVec])
```


The worker functions are `translatePat` which translates a single pattern and
`translateGuards` which translate a list of guards. The implementations are rather
straightforward with ... exceptions:

>
> 1.

>
> 2.

>
> 3.

**THE REST IS JUST DUMPED TEXT, I AM WORKING ON IT**


For the different constructors of `PmExpr` see below (sections Translation and The Term Oracle).

## Translation

```wiki
translatePat    :: Pat Id -> UniqSM PatVec
translatePatVec :: [Pat Id] -> UniqSM [PatVec]
translateGuards :: [GuardStmt Id] -> UniqSM PatVec -- replace unhandled??
```


and the function that combines them all translate a whole match-clause (including guards):

```wiki
translateMatch :: LMatch Id (LHsExpr Id) -> UniqSM (PatVec,[PatVec])
```


Instead of using the (enormous) `HsExpr` type, we drop most syntactic sugar and have only 5 constructors:

- `PmExprVar`: Variables or wildcards
- `PmExprCon`: Constructor patterns, Records
- `PmExprLit`: Overloaded and non-overloaded literals
- `PmExprEq` : Syntactic equality (NOTE: Pattern matching, NOT a call to the equality function `(==)`!!)

## Term Oracle