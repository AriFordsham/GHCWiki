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

- Whenever an equality of Form (2) or (3) would be recursive, the program can be rejected on the basis of a failed occurs check.  (Immediate rejection is always justified, as right-hand sides do not contain synonym familles; hence, any recursive occurrences of a left-hand side imply that the equality is unsatisfiable.)

## Termination


The Note \[skolemOccurs loop\] in the old code explains that equalities of the form `x ~ t` (where `x` is a flexible type variable) may not be used as rewrite rules, but only be solved by applying Rule Unify.  As Unify carefully avoids cycles, this prevents the use of equalities introduced by the Rule SkolemOccurs as rewrite rules.  For this to work, SkolemOccurs also had to apply to equalities of the form `a ~ t[[a]]`.  This was a somewhat intricate set up that's being simplified in the new algorithm.  Whether equalities of the form `x ~ t` are used as rewrite rules or solved by Unify doesn't matter anymore.  Instead, we disallow recursive equalities completely.  This is possible as right-hand sides are free of synonym families.
