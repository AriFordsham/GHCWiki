# Normalising and Solving Type Equalities


The following is based on ideas for the new, post-ICFP'08 solving algorithm.  Most of the code is in the module `TcTyFuns`.

## Normal equalities


Central to the algorithm are **normal equalities**, which can be regarded as a set of rewrite rules.  Normal equalities are carefully oriented and contain synonym families only as the head symbols of left-hand sides.  They assume one of the following three forms:

1. `co :: F t1..tn ~ t`,
1. `co :: x ~ t`, where `x` is a flexible type variable, or
1. `co :: a ~ t`, where `a` is a rigid type variable (skolem) and `t` is *not* a flexible type variable.


The types `t`, `t1`, ..., `tn` may not contain any occurrences of synonym families.  Moreover, in Forms (2) & (3), the left- and right-hand side need to be different, and the left-hand side may not occur in the right-hand side.


NB: We explicitly permit equalities of the form `x ~ y` and `a ~ b`, where both sides are either flexible or rigid type variables.


Coercions `co` are either wanteds (represented by a flexible type variable) or givens *aka* locals (represented by a type term of kind CO).  In GHC, they are represented by `TcRnTypes.EqInstCo`.   Moreover, `TcTyFuns.RewriteInst` represents normal equalities, emphasising their role as rewrite rules.

## Normalisation


(Ignoring the coercions for the moment.)

```wiki
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
- Do we need to record a skolem elimination substitution for skolems introduced during flattening for wanteds, too?

## Solving

- (Unify) is an asymmetric rule, and hence, only fires for equalities of the form `x ~ c`, where `c` is free of synonym families.  Moreover, it only applies to wanted equalities.  (Rationale: Local equality constraints don't justify global instantiation of flexible type variables.)
- (Local) only applies to normalised equalities in Form (2) & (3) - and currently also only to local equalities, not to wanteds.  In principle, a rewrite rule could be discarded after an exhaustive application of (Local).  However, while the set of class constraints is kept separate, we may always have some occurrences of the supposedly eliminated variable in a class constraint, and hence, need to keep all local equalities around.  NB: (Local) -for Forms (2) & (3)- is the most expensive rule as it needs to traverse all type terms.
- (IdenticalLHS) I don't think it is useful to apply that rule when both equalities are wanted, which makes it a variant of (Local).


Observation:

- Only (Local) when replacing a variable in the *left-hand side* of an equality of Form (1) can  lead to recursion with (Top).

## Termination

### SkolemOccurs


The Note \[skolemOccurs loop\] in the old code explains that equalities of the form `x ~ t` (where `x` is a flexible type variable) may not be used as rewrite rules, but only be solved by applying Rule Unify.  As Unify carefully avoids cycles, this prevents the use of equalities introduced by the Rule SkolemOccurs as rewrite rules.  For this to work, SkolemOccurs also had to apply to equalities of the form `a ~ t[[a]]`.  This was a somewhat intricate set up that's being simplified in the new algorithm.  Whether equalities of the form `x ~ t` are used as rewrite rules or solved by Unify doesn't matter anymore.  Instead, we disallow recursive equalities after normalisation completely (both locals and wanteds).  This is possible as right-hand sides are free of synonym families.


To see how the new algorithm handles the type of equalities that required SkolemOccurs in the ICFP'08 algorithm, consider the following notorious example:

```wiki
E_t: forall x. F [x] ~ [F x]
[F v] ~ v  ||-  [F v] ~ v
```


Derivation with rules in the new-single report:

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


Same, but de-prioritise (Local) - i.e., (Local) applies only if nothing else does:

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


Same, but de-prioritise (Local) rewriting with equalities of Forms (2) & (3):

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


Problem is that we still fail to terminate for unsatisfiable queries:

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


Is the algorithm semi-decidable?


Derivation when normalisation of locals uses flexible tyvars, too - simulates the effect of (SkolemOccurs):

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


Problem is that with rank-n signatures, we do want to use (Local) with flexible tyvars.
