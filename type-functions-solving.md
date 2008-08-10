# Normalising and Solving Type Equalities


The following is based on ideas for the new, post-ICFP'08 solving algorithm described in CVS `papers/type-synonym/new-single.tex`.  A revised version of `new-single.tex` that integrates the core ideas from this wiki page is in `papers/type-synonym/normalised_equations_algorithm.tex`.  Most of the code is in the module `TcTyFuns`.

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


The overall structure is as in `new-single.tex`, namely

1. normalise all constraints (both locals and wanteds),
1. solve the wanteds, and
1. finalise.


However, the three phases differ in important ways.  In particular, normalisation includes decompositions & the occurs check, and we don't instantiate any flexible type variables before we finalise (i.e., solving is purely local).

## Normal equalities


Central to the algorithm are **normal equalities**, which can be regarded as a set of rewrite rules.  Normal equalities are carefully oriented and contain synonym families only as the head symbols of left-hand sides.  They assume one of the following three forms:

1. **Family equality:**`co :: F t1..tn ~ t`,
1. **Flexible variable equality:**`co :: x ~ t`, where `x` is a flexible type variable, or
1. **Rigid variable equality:**`co :: a ~ t`, where `a` is a rigid type variable (skolem) and `t` is *not* a flexible type variable.


where

- the types `t`, `t1`, ..., `tn` may not contain any occurrences of synonym families and
- left-hand side of an equality may not occur in the right-hand side.


The second bullet of the where clause is trivially true for equalities of Form (1) and it implies that the left- and right-hand sides are different.

### Observations


The following is interesting to note:

- We explicitly permit equalities of the form `x ~ y` and `a ~ b`, where both sides are either flexible or rigid type variables.
- Normal equalities are similar to equalities meeting the Orientation Invariant and Flattening Invariant of new-single, but they are not the same.
- Normal equalities are **never** self-recursive.  They can be mutually recursive.  A mutually recursive group will exclusively contain variable equalities. 

### Coercions


Coercions `co` are either wanteds (represented by a flexible type variable) or givens *aka* locals (represented by a type term of kind CO).  In GHC, they are represented by `TcRnTypes.EqInstCo`, which is defined as

```wiki
type EqInstCo = Either 
                  TcTyVar    -- case for wanteds (variable to be filled with a witness)
		  Coercion   -- case for locals
```


Moreover, `TcTyFuns.RewriteInst` represents normal equalities, emphasising their role as rewrite rules. 

**SLPJ**: I propose that we use a proper data type, not `Either` for this.

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
-- Does OccursCheck + Decomp + Triv + Swap (of new-single)
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


A significant difference to new-single is that solving is a purely local operation.  We never instantiate any flexible variables.

**TODO**

- Rules applying to family equalities:

  - SubstFam (formerly, IdenticalLHS) only applies to family equalities (both local and wanteds)
  - Top only applies to family equalities (both locals and wanteds)

>
> We should apply SubstFam first as it cheaper and potentially reduces the number of applications of Top.  On the other hand, for each family equality, we may want to try to reduce it with Top, and if that fails, use it with SubstFam.  (That strategy should lend itself well to an implementation.)  But be careful, we need to apply Top exhaustively, to avoid non-termination.  More precisely, if we interleave Top and SubstFam, we can easily diverge.

- Rules applying to variable equalities:

  - SubstVar (formerly, Local) applies to variable equalities (both locals and wanteds)
- With SubstFam and SubstVar, we always substitute locals into wanteds and never the other way around.  We perform substitutions exhaustively.  For SubstVar, this is crucial to avoid non-termination.
- We should probably use SubstVar on all variable equalities before using SubstFam, as the former may refine the left-hand sides of family equalities, and hence, lead to Top being applicable where it wasn't before.
- We use SubstFam and SubstVar to substitute wanted equalities **only** if their right-hand side contains a flexible type variables (which for variable equalities means that we apply SubstVar only to flexible variable equalities).  **TODO** This is not sufficient while we are inferring a type signature as SPJ's example shows: `|- a ~ [x], a ~ [Int]`.  Here we want to infer `x := Int` before yielding `a ~ [Int]` as an irred.  So, we need to use SubstVar and SubstFam also if the rhs of a wanted contains a flexible variable.  This unfortunately makes termination more complicated.


Notes:

- In principle, a variable equality could be discarded after an exhaustive application of SubstVar.  However, while the set of class constraints is kept separate, we may always have some occurrences of the supposedly eliminated variable in a class constraint, and hence, need to keep all local equalities around.  That reasoning definitely applies to local equalities, but I think it also applies to wanteds (and I think that GHC so far never applies wanteds to class dictionaries, which might explain some of the failing tests.)  Flexible variable equalities cannot be discarded in any case as we need them for finalisation.

### Observations

- SubstVar is the most expensive rule as it needs to traverse all type terms.
- Only SubstVar when replacing a variable in a family equality can  lead to recursion with (Top).

## Finalisation


If only flexible type equalities remain as wanted equalities, the locals entail the wanteds.  We can now instantiate type variables in flexible type equalities where possible to propagate constraints into the environment.  In GHC, we may wrap any remaining equalities (of any form) into an implication constraint to be propagated outwards (where it may be solved under an extended set of local equalities.)


Notes:

- (Unify) is an asymmetric rule, and hence, only fires for equalities of the form `x ~ c`, where `c` is free of synonym families.  Moreover, it only applies to wanted equalities.  (Rationale: Local equality constraints don't justify global instantiation of flexible type variables - just as in new-single.)

- **TODO** Now that we delay instantiation until after solving, do we still need to prioritise flexible variables equalities over rigid ones?  (Probably not.)

## Examples

### Substituting wanted family equalities with SubstFam is crucial if the right-hand side contains a flexible type variable

```wiki
Top: F Int ~ [Int]

  |- F delta ~ [delta], F delta ~ [Int]
(SubstFam)
  |- F delta ~ [delta], norm [[ [delta] ~ [Int] ]]
==>
  |- F delta ~ [delta], delta ~ Int
(SubstVar)
  |- norm [[ F Int ~ [Int] ]], delta ~ Int
==>
  |- F Int ~ [Int], delta ~ Int
(Top)
  |- norm [[ [Int] ~ [Int] ]], delta ~ Int
==>
  |- delta ~ Int
QED
```

### Interaction between local and wanted family equalities


Example 4 of Page 9 of the ICFP'09 paper.

```wiki
  F [Int] ~ F (G Int)  |-  G Int ~ [Int], H (F [Int]) ~ Bool
=(norm)=>
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  G Int ~ [Int], H x ~ Bool, F [Int] ~ x
(SubstFam w/ F [Int])
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  G Int ~ [Int], H x ~ Bool, x ~ a
(SubstFam w/ G Int)
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  b ~ [Int], H x ~ Bool, x ~ a
(SubstVar w/ x)
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  b ~ [Int], H a ~ Bool, x ~ a
```

**TODO** If we use flexible variables for the flattening of the wanteds, too, the equality corresponding to `x ~ a` above will be oriented the other way around.  That can be a problem because of the asymmetry of the SubstVar and SubstFun rules (i.e., wanted equalities are not substituted into locals).

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
