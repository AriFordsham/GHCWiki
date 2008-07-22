# Normalising and Solving Type Equalities


The following is based on ideas for the new, post-ICFP'08 solving algorithm.  Most of the code is in the module `TcTyFuns`.

## Normal equalities


Central to the algorithm are **normal equalities**, which can be regarded as a set of rewrite rules.  Normal equalities are carefully oriented and contain applications of synonym family applications only in outermost positions of left-hand sides.  They assume one of the following three forms:

1. `co :: F t1..tn ~ t`,
1. `co :: x ~ t`, where `x` is a flexible type variable, or
1. `co :: a ~ t`, where `a` is a rigid type variable (skolem) and `t` is *not* a flexible type variable.


and `t`, `t1`, ..., `tn` contain no applications of synonym families.  Coercions `co` are either wanteds (represented by a flexible type variable) or givens (represented by type term of kind CO).


NB: We explicitly permit equalities of the form `x ~ y` and `a ~ b`, where both sides are either flexible or rigid type variables.
