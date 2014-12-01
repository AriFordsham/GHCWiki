# Exhaustiveness/Redundancy check implementation


This page describes how the exhaustiveness and redundancy cheker is implemented
in GHC. If you are looking for a general description of the problem and an overview
of our approach see [PatternMatchCheck](pattern-match-check).

## The `PmPat` datatype


The `PmPat` data type is defined in `deSugar/Check.lhs` as:

```wiki
data PmPat id = PmGuardPat PmGuard
              | PmVarPat { pm_ty :: Type, pm_var :: id }
              | PmConPat { pm_ty :: Type, pm_pat_con :: DataCon, pm_pat_args :: [PmPat id] }
              | PmLitPat { pm_ty :: Type, pm_lit :: PmLit id }
              | PmLitCon { pm_ty :: Type, pm_var :: id, pm_not_lit :: [PmLit id] }
```


Instead of using the more verbose type `Pat`, type `PmPat` represents only the
following:

- Guards
- Term variables
- Constructor patterns
- Literal patterns
- Negative literal patterns


Note that `PmPat` is more liberal than `Pat`, in the sense that we allow guards
to be interleaved with normal patterns. Hence, we lift the restriction that all
clauses must have the same length.


Also, negative literal patterns `PmLitCon` are used in cases where we we want to
represent what a literal 'cannot be'.

## Transforming `Pat` to `PmPat`


The main function for viewing `Pat Id` into our domain (`PmPat Id`) is

```wiki
mViewPat :: Pat Id -> PmM [PmPat Id]
```


It never generates `PmLitCon`s but may introduce guards in the middle of the
clause in the following cases:

- view patterns:  `(f -> p) ==> [x , p <- f x]` (a variable and a guard)
- `n+k` patterns: `(n+k)    ==> [n', n'>=k, let n = n'-k]` (a variable and two guards)


Overloaded lists and pattern synonyms must be handled too but for now are just
introducing a variable and an unresolved guard.

## Main algorithm


Instead of keeping the eliminated vectors (as we do in the paper), we use the
type `Forces` to keep track if a clause forces the evaluation of an argument
or not:

```wiki
data Forces = Forces | DoesntForce
```


The rest of the types that are heavily used in the main algorithm are the following:

```wiki
data Delta = Delta { delta_evvars :: Bag EvVar, delta_guards :: PmGuard }
 
type InVec = [PmPat Id]
type CoveredVec   = (Delta, [PmPat Id])
type UncoveredVec = (Delta, [PmPat Id])

data PmTriple = PmTriple { pmt_covered   :: [CoveredVec]
                         , pmt_uncovered :: [UncoveredVec]
                         , pmt_forces    :: Forces }
```

`Delta` keeps track of the constraints that correspond to each clause, and includes
both type constraints `EvVar` and term constraints (guards) `PmGuard`.
`InVec` is simply a list of `PmPat`s where guards may appear anywhere in the
vector. Both `Covered` and `Uncovered` represent intermediate vectors generated
by the algorithm where the vectors are guard-free and all constraints are accumulated
in `Delta`.
Finally, `PmTriple`, as shown in the paper, is the information we get for every clause.
It contains information about the cases it covers, the cases that leaves uncovered and
about forcing.


The heavy worker is function `one_step` which is function `alg` from the paper:

```wiki
one_step      :: UncoveredVec   -> InVec -> PmM PmTriple
one_full_step :: [UncoveredVec] -> InVec -> PmM PmTriple
```

`one_full_step` simply calls `one_step` on every uncovered vector and combines
the results.

## The `PmM` monad


All the algorithm lives inside `PmM` monad which is nothing else than `DsM` with
a possibility of failure (`Maybe`).

```wiki
newtype Pm gbl lcl a = PmM { unPmM :: TcRnIf gbl lcl (Maybe a) }
type PmM a = Pm DsGblEnv DsLclEnv a
```


The only reason we had to lift the algorithm in the `TcRnIf` is because we need
fresh term and type variables but, apart from that, the main part of the algorithm
is still purely syntactic.

## Changes to `DsLclEnv`


In order to handle correctly cases like [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139), we have to collect constraints not
only from the match we are currently checking but also from the context. Hence, we
extend `DsLclEnv` with the additional field `ds_dicts :: Bag EvVar` where we accumulate
constraints as we desugar the AST. Also, functions

```wiki
getDictsDs :: DsM (Bag EvVar)
addDictsDs :: Bag EvVar -> DsM a -> DsM a
```


are used for retrieving and adding more constraints in the context.

## External interface


The main function that is exported is `checkpm`:

```wiki
checkpm :: [Type] -> [EquationInfo] -> DsM (Maybe PmResult)
```


Takes as arguments the types of the arguments and all matches and returns a
`Maybe PmResult`. (Note that we expand all type synonyms). The `PmResult` type
synonym is just:

```wiki
type PmResult = ([EquationInfo], [EquationInfo], [UncoveredVec])
```


That is

- Redundant clauses
- Clauses with inaccessible right hand side
- Uncovered cases
