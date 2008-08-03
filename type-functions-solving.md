# Normalising and Solving Type Equalities


The following is based on ideas for the new, post-ICFP'08 solving algorithm described in CVS `papers/type-synonym/new-single.tex`.  Most of the code is in the module `TcTyFuns`.

## Terminology

<table><tr><th>*Wanted equality*</th>
<td>
An equality constraint that we need to derive during type checking.  Failure to derive it leads to rejection of the checked program.
</td></tr>
<tr><th>*Local equality*, *given equality*</th>
<td>
An equality constraint that -in a certain scope- may be used to derive wanted equalities.
</td></tr>
<tr><th>*Flexible type variable*,  *unification variable*, *HM variable*</th>
<td>
Type variables that may be **globally** instantiated by unification.
</td></tr>
<tr><th>*Rigid type variable*, *skolem type variable*</th>
<td>
Type variable that cannot be globally instantiated, but it may be **locally** refined by a local equality constraint.
</td></tr></table>

## Overall algorithm


The overall algorithm is as in `new-single.tex`, namely

1. normalise all constraints (both locals and wanteds),
1. solve the wanteds, and
1. finalise.

## Normal equalities


Central to the algorithm are **normal equalities**, which can be regarded as a set of rewrite rules.  Normal equalities are carefully oriented and contain synonym families only as the head symbols of left-hand sides.  They assume one of the following three forms:

1. `co :: F t1..tn ~ t`,
1. `co :: x ~ t`, where `x` is a flexible type variable, or
1. `co :: a ~ t`, where `a` is a rigid type variable (skolem) and `t` is *not* a flexible type variable.


where

- the types `t`, `t1`, ..., `tn` may not contain any occurrences of synonym families, and
- we call the Forms (2) & (3) **variable equalities**, and require that the left- and right-hand side need to be different, and that the left-hand side does not occur in the right-hand side.

### Observations


The following is interesting to note:

- We explicitly permit equalities of the form `x ~ y` and `a ~ b`, where both sides are either flexible or rigid type variables.
- Normal equalities are similar to equalities meeting the Orientation Invariant and Flattening Invariant of new-single, but they are not the same.
- Normal equalities are **never** recursive.  They can be mutually recursive.  A mutually recursive group will exclusively contain variable equalities.

### Coercions


Coercions `co` are either wanteds (represented by a flexible type variable) or givens *aka* locals (represented by a type term of kind CO).  In GHC, they are represented by `TcRnTypes.EqInstCo`, which is defined as

```wiki
type EqInstCo = Either 
                  TcTyVar    -- case for wanteds (variable to be filled with a witness)
		  Coercion   -- case for locals
```


Moreover, `TcTyFuns.RewriteInst` represents normal equalities, emphasising their role as rewrite rules. 

## Normalisation


The following function `norm` turns an arbitrary equality into a set of normal equalities.  (It ignores the coercions for the moment.)

```wiki
data EqInst         -- arbitrary equalities
data FlatEqInst     -- synonym families may only occur outermost on the lhs
data RewriteInst    -- normal equality

norm :: EqInst -> [RewriteInst]
norm [[F s1..sn ~ t]] = [[F s1'..sn' ~ t']] : eqs1++..++eqsn++eqt
  where
    (s1', eqs1) = flatten s1
    ..
    (sn', eqsn) = flatten sn
    (t', eqt)   = flatten t
norm [[t ~ F s1..sn]] = norm [[F s1..sn ~ t]]
norm [[s ~ t]] = check [[s' ~ t']] : eqs++eqt
  where
    (s', eqs) = flatten s
    (t', eqt) = flatten t

check :: FlattenedEqInst -> [FlattenedEqInst]
-- Does OccursCheck + Decomp + Swap (of new-single)
check [[t ~ t]] = []
check [[x ~ t]] | x `occursIn` t = fail
                          | otherwise = [[x ~ t]]
check [[t ~ x]] | x `occursIn` t = fail
                          | otherwise = [[x ~ t]]
check [[a ~ t]] | a `occursIn` t = fail
                          | otherwise = [[a ~ t]]
check [[t ~ a]] | a `occursIn` t = fail
                          | otherwise = [[a ~ t]]
check [[s1 s2 ~ t1 t2]] = check [[s1 ~ t1]] ++ check [[s2 ~ t2]]
check [[T ~ S]] = fail

flatten :: Type -> (Type, [FlattenedEqInst])
-- Result type has no synonym families whatsoever
flatten [[F t1..tn]] = (x, [[F t1'..tn' ~ x]] : eqt1++..++eqtn)
  where
    (t1', eqt1) = flatten t1
    ..
    (tn', eqtn) = flatten tn
    NEW x
    -- also need to add x := F t1'..tn'
flatten [[t1 t2]] = (t1' t2', eqs++eqt)
  where
    (t1', eqs) = flatten t1
    (t2', eqt) = flatten t2
flatten t = (t, [])
```


Notes:

- Perform Rule Triv as part of normalisation.
- Whenever an equality of Form (2) or (3) would be recursive, the program can be rejected on the basis of a failed occurs check.  (Immediate rejection is always justified, as right-hand sides do not contain synonym familles; hence, any recursive occurrences of a left-hand side imply that the equality is unsatisfiable.)
- Use flexible tyvars for flattening of locals, too.  (We have flexibles in locals anyway and don't use (Unify) on locals, so the flexibles shouldn't cause any harm, but the elimination of skolems is much easier for flexibles - we just instantiate them.)
- **TODO** Do we need to record a skolem elimination substitution for skolems introduced during flattening for wanteds, too?

## Solving


Notes:

- (Unify) is an asymmetric rule, and hence, only fires for equalities of the form `x ~ c`, where `c` is free of synonym families.  Moreover, it only applies to wanted equalities.  (Rationale: Local equality constraints don't justify global instantiation of flexible type variables - just as in new-single.)

- (Local) only substitutes normal variable equalities - and currently also only local equalities, not wanteds.  (We should call this Rule (Subst) again.) 

>
> In principle, a rewrite rule could be discarded after an exhaustive application of (Local).  However, while the set of class constraints is kept separate, we may always have some occurrences of the supposedly eliminated variable in a class constraint, and hence, need to keep all local equalities around.

- (IdenticalLHS) I don't think it is useful to apply that rule when both equalities are wanted, which makes it a variant of (Local).   BUT as SLPJ points out what about `F a ~ x1, F a ~ Int`?  Then we could unify the HM variable `x` with `Int`.

### Observations

-  Rule (Local) -substituting variable equalities- is the most expensive rule as it needs to traverse all type terms.
- Only (Local) when replacing a variable in the *left-hand side* of an equality of Form (1) can  lead to recursion with (Top).

## Termination

### SkolemOccurs


The Note \[skolemOccurs loop\] in the old code explains that equalities of the form `x ~ t` (where `x` is a flexible type variable) may not be used as rewrite rules, but only be solved by applying Rule Unify.  As Unify carefully avoids cycles, this prevents the use of equalities introduced by the Rule SkolemOccurs as rewrite rules.  For this to work, SkolemOccurs also had to apply to equalities of the form `a ~ t[[a]]`.  This was a somewhat intricate set up that we seek to simplify here.  Whether equalities of the form `x ~ t` are used as rewrite rules or solved by Unify doesn't matter anymore.  Instead, we disallow recursive equalities after normalisation completely (both locals and wanteds).  This is possible as right-hand sides are free of synonym families.


To look at this in more detail, let's consider the following notorious example:

```wiki
E_t: forall x. F [x] ~ [F x]
[F v] ~ v  ||-  [F v] ~ v
```

**New-single**: The following derivation shows how the algorithm in new-single fails to terminate for this example.

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (Local) with v
F [a] ~ a  ||-  [a] ~ [x], F [a] ~ x
==> normalise
F [a] ~ a  ||-  x ~ a, F[a] ~ x
==> 2x (Top) & Unify
[F a] ~ a  ||-  [F a] ~ a
..and so on..
```

**New-single using flexible tyvars to flatten locals, but w/o Rule (Local) for flexible type variables**: With (SkolemOccurs) it is crucial to avoid using Rule (Local) with flexible type variables.  We can achieve a similar effect with new-single if we (a) use flexible type variables to flatten local equalities and (b) at the same time do not use Rule (Local) for variable equalities with flexible type variables.  NB: Point (b) was necessary for the ICFP'08 algorithm, too.

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [x2], F v ~ x2  ||-  v ~ [x1], F v ~ x1
** x2 := F v
==> (Local) with v
F [x2] ~ x2  ||-  [x2] ~ [x1], F [x2] ~ x1
** x2 := F v
==> normalise
F [x2] ~ x2  ||-  x2 ~ x1, F [x2] ~ x1
** x2 := F v
==> 2x (Top) & Unify
[F x1] ~ x1  ||-  [F x1] ~ x1
** x1 := F v
==> normalise
x1 ~ [y2], F x1 ~ y2  ||-  x1 ~ [y1], F x1 ~ y1
** x1 := F v, y2 := F x1
..we stop here if (Local) doesn't apply to flexible tyvars
```


A serious disadvantage of this approach is that we **do** want to use Rule (Local) with flexible type variables as soon as we have rank-n signatures.  In fact, the lack of doing so is responsible for a few failing tests in the testsuite in the GHC implementation of (SkolemOccurs).

**De-prioritise Rule (Local)**: Instead of outright forbidding the use of Rule (Local) with flexible type variables, we can simply require that Local is only used if no other rule is applicable.  (That has the same effect on satisfiable queries, and in particular, the present example.)

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (IdenticalLHS) with v & F v
v ~ [a], F v ~ a  ||- [a] ~ [x], x ~ a
==> normalise
v ~ [a], F v ~ a  ||-  x ~ a, x ~ a
==> (Unify)
v ~ [a], F v ~ a  ||-  a ~ a
==> normalise
v ~ [a], F v ~ a ||-
QED
```


In fact, it is sufficient to de-prioritise Rule (Local) for variable equalities (if it is used for other equalities at all):

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (Local) with F v
v ~ [a], F v ~ a  ||-  v ~ [x], x ~ a
==> (Unify)
v ~ [a], F v ~ a  ||-  v ~ [a]
==> (Local) with v
v ~ [a], F [a] ~ a ||- [a] ~ [a]
==> normalise
v ~ [a], F [a] ~ a ||-
QED
```

**One problems remains**: The algorithm still fails to terminate for unsatisfiable queries.

```wiki
[F v] ~ v  ||-  [G v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], G v ~ x
a := F v
==> (Local) with v
F [a] ~ a  ||-  [a] ~ [x], G [a] ~ x
==> normalise
F [a] ~ a  ||-  x ~ a, G [a] ~ x
==> (Unify)
F [a] ~ a  ||-  G [a] ~ a
==> (Top)
[F a] ~ a  ||-  G [a] ~ a
==> normalise
a ~ [b], F a ~ b  ||-  G [a] ~ a
b := F a
..and so on..
```


My guess is that the algorithm terminates for all satisfiable queries.  If that is correct, the entailment problem that the algorithm solves would be  semi-decidable.
