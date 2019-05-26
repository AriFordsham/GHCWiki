As of 52fc2719b93ab39be3e52eba531ee173b9134183, a type checker plugin can extend GHC in exactly one way:
the GHC constraint solver occasionally gives each plugin a chance to modify the constraints it is working on.

This page summarizes some main ideas of the constraint solver.
I wrote this specifically when I was investigating what Derived constraints are,
so the summary currently focuses on those and related concepts.

\[The `Note` references below are accurate as of writing
(GHC commit 52fc2719b93ab39be3e52eba531ee173b9134183).\]

# The Constraint Solver

The constraint solver manages two groups of constraints
  * a set of *inert* constraints that satisfies several invariants,
    which in particular ensure that each is "independent" of the others and/or marked as "invalid"
	(for some definition of "independent" and "invalid")
  * a work list of constraints not yet in the inert set

Each iteration of the loop uses the inerts to simplify the next work item
until it is also inert (maybe even solved)
and then *kicks-out* any formerly-inert constraint back onto the work list
if the new inert can simplify it.
This repeats until the work list is empty.
If no contradictory or unsatisfiable constraints were found,
the unique solution was found.
Otherwise, GHC reports to the user those it judges to be helpful.

## Evidence

The constraint solver maintains evidence,
A constraint *flavor* is one of
  * `W` "Wanted": the user program wants the constraint's evidence from the solver
  * `G` "Given": the user program gives the constraint's evidence to the solver
  * `D` "Derived": the user program neither wants nor gives the constraint's evidence

(See `Note [Constraint flavours]`.)

Intuitions:
  * a constraint is essentially a type that inhabits the kind `Constraint`
    (eg `Ord Int :: Constraint`, `(a ~ b) :: Constraint`, etc)
  * a constraint's evidence inhabits that type
      * a type class is a record type that its dictionaries inhabit
	  * the dictionary fields are the methods and the superclass dictionaries
      * an instance is a function from its context's dictionary to its head's dictionary
  * Given evidence originates from invisible arguments in patterns
  * Wanted evidence ends up as invisible arguments in expressions
  * Wanted evidence is a tree of built-in axia and top-level instance functions
    applied to Given evidence

## Derived Constraints

Derived constraints have one purpose: *improvement* of genuine constraints
by discovering equalities that must be satisfied in every possible solution
of these Wanteds in the scope of these Givens.
Deriveds are thus used to find and apply most general unifiers during constraint solving.
(See Note `[The improvement story and derived shadows]`.)

(Aside: Derived `CDictCan`s and Derived `CFunEqCan`s are therefore only worth the trouble
because they may interact with other Deriveds to yield Derived `CTyEqCan`s.)

The flavors `W` `G` `D` would suffice for the solver.
However, `W` and `D` share enough semantics
and implementation efficiency matters enough
to justify an additional hybrid flavor:
  * `WD` A `W` that can split (once) into a `W` and an initially-identical `D`

A `D` arises either directly (eg from a fundep) or from the on-demand splitting of a `WD`.
(See `Note [Constraint flavours]`.)

When does a `WD` split? That's determined by rewriting.

## Rewriting

The equality constraints in the inert set define *the inert substitution*,
which *rewrites* work items by replacing each type variable by the type to which it is equal.
As in unification, this substitution propagates so-called "positive information":
what the type variable *is*,
as opposed to what it *is not*.

### The `can-rewrite` relation

Whether an equality constraint is allowed to rewrite another constraint
depends on the flavors and roles.
(See `Definition [Can-rewrite relation]` in `Note [inert_eqs: the inert equalities]`.)

In terms of just flavors
  * `G` `can-rewrite` `*`
  * `D` `can-rewrite` `D`

Wanteds are not allowed to rewrite Wanteds because it results in very unhelpful error messages.
(See `Note [Wanteds do not rewrite Wanteds]`.)

In additon to its flavor, an equality constraint also has a *role*,
which is the equality relation it uses.
It's one of
  * `N` "Nominal": judgemental type equality (as in a `type` synonym declaration)
  * `R` "Representational": representational equality (as in a `newtype` declaration)

In terms of just roles
  * `N` `can-rewrite` `*`
  * `R` `can-rewrite` `R`

The `D/R` constraints are odd (see `Note [Deriveds do rewrite Deriveds]`),
so the actual rules are
  * `G/N` `can-rewrite` `*/*`
  * `G/R` `can-rewrite` `*/R`
  * `D/N` `can-rewrite` `D/N`
  * `WD/N` `can-rewrite` `D/N`

### Splitting `WD`s

Once a `WD` work item has been made inert, one of two things happens:
  * it does not split and remains an inert `WD`
  * it is split into an inert `W` and a new `D` work item

It is split only if the inerts will usefully rewrite the new `D` "shadow".
That is, if
  * an inert equality `can-rewrite` the shadow
  * that equality maps one of the shadow's "interesting" free variables
    (for some definition of "interesting")

Splitting `WD`s
  * makes the solver more complete (cf *improvement*)
  * preserves inert set invariants related to `can-rewrite` that ensure the solver terminates

# TODO

I plan to migrate more of my notes onto this wiki page.

  * Canonical constraints
  * Levels (untouchables)
  * Flattening
  * Zonking
  * Coercions

Please contact me with suggestions: nicolas.frisby@gmail.com.
