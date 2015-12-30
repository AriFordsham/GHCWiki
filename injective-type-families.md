# Injective type families


This page summarizes the design of injective type families ([\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018)).
Implementation discussion and progress was recorded in
[ Phab D202](https://phabricator.haskell.org/D202).


As of September 2015 injective type families are merged into HEAD and will be
available in the next stable GHC release (8.0).


Person responsible for this page is Jan Stolarek (just so you now who is meant
by "I").  I am also responsible for most of the implementation.  Simon Peyton
Jones and Richard Eisenberg participated in the development of theory behind
injective type families, so whenever I say "we" I mean the three of us.  For
full discussion of injective type families see Haskell Symposium 2015 [ paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf)
"Injective type families for Haskell" (henceforth referred to as the
"injectivity paper").

## Forms of injectivity


The idea behind [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018) is to allow users to declare that a type family is
injective.  We have identified three forms of injectivity:

1. Injectivity in all the arguments, where knowing the result (right-hand
  side) of a type family determines all the arguments on the left-hand
  side. Examples:

```
typefamilyId a whereId a = a
```

```
typefamilyF a b c
typeinstanceFIntCharBool=BooltypeinstanceFCharBoolInt=InttypeinstanceFBoolIntChar=Char
```

1. Injectivity in some of the arguments, where knowing the RHS of a type
  family determines only some of the arguments on the LHS. Example:

```
typefamilyG a b c
typeinstanceGIntCharBool=BooltypeinstanceGIntCharInt=BooltypeinstanceGBoolIntInt=Int
```

>
> Here knowing the RHS allows us to determine first two arguments, but not the
> third one.

1. Injectivity in some of the arguments, where knowing the RHS of a type
  family and some of the LHS arguments determines other (possibly not all)
  LHS arguments. Examples:

```
typefamilyPlus a b wherePlusZ     n = n
     Plus(S m) n =S(Plus m n)
```

>
> Here knowing the RHS and the first argument uniquely determines the remaining
> argument.

```
typefamilyH a b c
typeinstanceHIntCharDouble=InttypeinstanceHBoolDoubleDouble=Int
```

>
> Knowing the RHS and either `a` or `b` allows to uniquely determine the
> remaining two arguments, but knowing the RHS and `c` gives us no information
> about `a` or `b`.


In the following text I will refer to these three forms of injectivity as A, B
and C.


Currently GHC implements injectivity of type B (and therefore of type A as well,
since A is a subset of B).

## Proposed syntax


When deciding on a concrete syntax our two main design goals were:

- **Backwards compatibility**: injective type families are automatically
  enabled with the `TypeFamilies` language extension. This means that the
  proposed syntax had to be backward compatible, ie. not break any existing
  code that uses type families.

- **Future extensibility**: currently we we only implemented injectivity A
  and B but we still want to be able to implement injectivity C in the future
  without breaking A and B.


We decided to use syntax borrowed from functional dependencies.  First of all
the user must be able to introduce a variable that names the result of a type
family.  To achieve that we extend syntax of type family head by allowing to
write `= tyvar` or `= (tyvar :: kind)` annotations in addition to already
allowed `:: kind` annotation. In other words all these declaration are
well-formed:

```
typefamilyPlus(a ::Nat)(b ::Nat)where...typefamilyPlus(a ::Nat)(b ::Nat)::Natwhere...typefamilyPlus(a ::Nat)(b ::Nat)= c where...typefamilyPlus(a ::Nat)(b ::Nat)=(c ::Nat)where...
```


but the third or fourth form is required if the user wants to introduce
injectivity annotation. The injectivity annotation itself begins with `|`
following the result type variable.  `|` is followed by an injectivity
condition. Injectivity condition has the form:

```
A->B
```


where `A` is the result type variable and `B` is a non-empty lists of type
variables declared in type family head. Things on the left and right of `->` are
called LHS and RHS of an injectivity condition, respectively. If a type family
is injective in a given argument respective type variable must be mentioned in
the RHS of an injectivity condition. Variables may be skipped if a type family
is not injective in a given argument.


Here are examples of injectivity declarations using proposed syntax:

```
typefamilyId a = result | result -> a where{...}typefamilyF a b c = d | d -> c a b
typefamilyG a b c = foo | foo -> a b where{...}
```


This syntax can be easily extended in the future if we want to implement
injectivity of type C. First required change is that there may be many
comma-separated injectivity conditions.  Second change is that LHS of
injectivity condition can mention type variables that name the arguments, not
just the result.  With this extended syntax we could write:

```
typefamilyPlus a b =(sum ::Nat)| sum a -> b, sum b -> a wheretypefamilyH a b c = xyz | xyz a -> b c, xyz b -> a c
```


Note that for open and closed type families it is correct to declare a type
variable that names the result but skip the injectivity annotation.  That is
not the case for associated types.  If you name the result but ignore the
injectivity annotation GHC will interpret this as an associated type default.

## Implementation outline

### Verifying correctness of injectivity annotation


Before checking that a type family is indeed injective, as declared by the user,
GHC needs to check the correctness of injectivity annotation itself.  This
includes checking that:

- only in-scope type variables are used. For example
  `type family F a = r | r -> b` should result with "not in scope: b" error.


See test T6018rnfail in the testsuite for more examples of invalid declarations.

### Verifying that type family equations agree with injectivity annotation


Once the user declares type family to be injective GHC must verify that this
declaration is correct, ie. type family really is injective.  Below are the
rules we follow when checking for injectivity of a type family. For full details
and rationale behind them see the injectivity paper, Section 4.2.

**Important**: these rules are only for the currently implemented injectivity
of types A and B.  RHS refers to the right-hand side of the type family equation
being checked for injectivity.  LHS refers to the arguments of that type family
equation.  Term "type family equation" can refer to equations of both open and
closed type families, unless stated otherwise.

**Rules for checking injectivity**


A general idea is that if we find at least one equation (bullets (1), (2) and
(3)) or a pair of equations (bullets (4) and (5)) that violate injectivity
annotation then we conclude that a type family is not injective in a way user
claims and we report an error.

1. If a RHS of a type family equation is a type family application we conclude
  that the type family is not injective.

1. If a RHS of a type family equation is a bare type variable we require that
  all LHS variables (including implicit kind variables) are also bare.  In
  other words, this has to be a sole equation of that type family and it has to
  cover all possible patterns.  If the patterns are not covering we conclude
  that the type family is not injective.

1. If a LHS type variable that is declared as injective is not mentioned on
  injective position in the RHS we conclude that the type family is not
  injective.  By "injective position" we mean argument to a type constructor or
  argument to a type family on injective position.

Open type families (including associated types)


Open type families are typechecked incrementally.  This means that when a module
is imported type family instances contained in that module are checked against
instances present in already imported modules.  In practice this is done by
checking equations pair-wise (a new equation against all already checked
equations -- modulo optimisations).

1. When checking a pair of an open type family equations we attempt to unify
  their RHSs. If they don't unify this pair does not violate injectivity
  annotation.  If unification succeeds with a substitution (possibly empty)
  then LHSs of unified equations must be identical under that substitution. If
  they are not identical then we conclude that a type family is not injective.


Note that we use a special variant of the unification algorithm that treats type
family applications as possibly unifying with anything.

Closed type families


In a closed type family all equations are ordered and in one place. Equations
are also checked pair-wise but this time an equation has to be paired with all
the preceeding equations.  Of course a single-equation closed type family is
trivially injective (unless (1), (2) or (3) above holds).

1. When checking a pair of closed type family equations we try to unify their
  RHSs.  If they don't unify this pair does not violate injectivity annotation.
  If the RHSs can be unified under some substitution (possibly empty) then
  either the LHSs unify under the same substitution or the LHS of the latter
  equation is subsumed by earlier equations.  If neither condition is met we
  conclude that a type family is not injective.


Again, we use a special variant of the unification algorithm.

### Source code tour


Below is a list of primary source code locations that implement injectivity:

- [compiler/rename/RnSource.hs](/trac/ghc/browser/ghc/compiler/rename/RnSource.hs).`rnInjectivityAnn`: checks
  correctness of injectivity annotation (mostly variable scoping).

- [compiler/typecheck/FamInst.hs](/trac/ghc/browser/ghc/compiler/typecheck/FamInst.hs).`checkForInjectivityConflicts` is
  an entry point for injectivity check of open type families.

- [compiler/typecheck/TcTyClsDecls.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcTyClsDecls.hs).`checkValidClosedCoAxiom` is
  an entry point for injectivity check of closed type families.

- [compiler/types/FamInstEnv.hs](/trac/ghc/browser/ghc/compiler/types/FamInstEnv.hs).`injectiveBranches` checks that a
  pair of type family axioms does not violate injectivity annotation.

- [compiler/types/FamInstEnv.hs](/trac/ghc/browser/ghc/compiler/types/FamInstEnv.hs).`lookupFamInstEnvInjectivityConflicts`
  implements condition (4) of injectivity check.

- [compiler/typecheck/TcTyClsDecls.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcTyClsDecls.hs).`checkValidClosedCoAxiom.check_injectivity.gather_conflicts`
  implements condition (5) of injectivity check.

- [compiler/types/Unify.hs](/trac/ghc/browser/ghc/compiler/types/Unify.hs).`tcUnifyTyWithTFs` is our special
  variant of a unification algorithm.

- [compiler/typecheck/FamInst.hs](/trac/ghc/browser/ghc/compiler/typecheck/FamInst.hs).`makeInjectivityErrors` checks
  conditions (1), (2) and (3) of the injectivity check.  It also takes as an
  argument results of check (4) or (5) and constructs error messages, if
  necessary.

- [compiler/typecheck/TcInteract](/trac/ghc/browser/ghc/compiler/typecheck/TcInteract), functions
  `improveLocalFunEqs.do_one_injective` and `improve_top_fun_eqs` implement
  typechecking improvements based on injectivity information.


Relevant source code notes are:

- `Note [FamilyResultSig]` in [compiler/hsSyn/HsDecls.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsDecls.hs)
- `Note [Injectivity annotation]` in [compiler/hsSyn/HsDecls.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsDecls.hs)
- `Note [Injective type families]` in [compiler/types/TyCon.hs](/trac/ghc/browser/ghc/compiler/types/TyCon.hs)
- `Note [Renaming injectivity annotation]` in [compiler/rename/RnSource.hs](/trac/ghc/browser/ghc/compiler/rename/RnSource.hs)
- `Note [Verifying injectivity annotation]` in [compiler/types/FamInstEnv.hs](/trac/ghc/browser/ghc/compiler/types/FamInstEnv.hs)
- `Note [Type inference for type families with injectivity]` in [compiler/typecheck/TcInteract.hs](/trac/ghc/browser/ghc/compiler/typecheck/TcInteract.hs)

## Injectivity for poly-kinded type families


With *PolyKinds* extension enabled it is possible to declare kind variables as
injective.  Moreover, if a type variable is declared as injective its associated
kind variable is also considered injective.  See section 6 of the injectivity
paper for full details.

## Connection with functional dependencies


In the course of our work it turned out that there is a close correspondence
between injective type families and functional dependencies.  Section 7 of
the injectivity paper discusses this connection.

## Future plans and ideas

### Type C injectivity


Our plan for the nearest future is to implement injectivity of type C.  This
will give injective type families expressive power identical to this of
functional dependencies.


If we decied to implement injectivity of type C checking injectivity annotation
would become more complicated as we would have to check for things like:

- `type family F a b = r | r -> a, r -> b`. This is technically correct but we
  could just say `result -> a b`.

- there are no identical conditions (this wouldn't hurt, but the user deserves
  a warning about this)

- type variables are not repeated on either LHS or RHS of the injectivity
  condition. For example `r a a -> ...` or `... -> a b a` should generate
  a warning. Note that it probably is OK to have the same variable both on the
  LHS and RHS of an injectivity condition: in the above examples it is true
  that `type family G a b c | result c -> a b c`. The question is whether this
  has any practical relevance.

- injectivity conditions don't overlap (eg. `result -> a b` overlaps
  `result -> a`). This probably deserves a warning.

### Inferring injectivity

[Here](https://gitlab.haskell.org//ghc/ghc/issues/6018) it was suggested by Simon that we could infer
injectivity for closed type families. I initially argued that, while inferring
injectivity of type A should be simple, inferring injectivity of type B would be
exponential in the numer of arguments to a type family. I take back that claim
as I now believe the algorithm can be made linear in the number of
arguments. Say we have a closed type family:

```
typefamilyF a b c ... n = r where
```


Since there are n arguments there are 2<sup>n possible injectivity annotations.
That's why I believed that we have to check every possible annotationton.  I now
believe that instead checking every possible annotation we should only check
whether knowing the RHS allows to infer each argument independently.  In other
words when inferring injectivity we would check whether annotations `r -> a`, \`r
-\> b`, `r -\> c` ... `r -\> n` hold true. If we learn for example that knowing `r\`
allows to infer `a`, `c` and `n` but not the other argumenst we infer the
annotation `r -> a c n`.
</sup>


There are several concerns to consider before implementing this:

- Before GHC 8.0 comes out there will already be some code in the wild that uses
  closed type families introduced in GHC 7.8. None of these type families
  require injectivity to work because GHC 7.8 does not support injectivity. If
  we attempt to infer injectivity for all these already existing closed type
  families we will only increase compilation time of existing code with
  absolutely no gain in functionality of the code. There were some complaints
  about GHC's performance decreasing with each release and I don't want to add
  to that.

- I believe that requiring explicit injectivity annotations is a valuable
  source code documentation for the programmer.  This is very subjective, but
  the truth is injectivity is very subtle and can easily be broken by slight
  modifications of type family definition.  It is much better to be explicit
  about relying on injectivity.

- Annotations also make it explicit which code compiles with GHC 8.0 and
  which does not. If we infer injectivity the code that works with 8.0 might
  result in typing errors for earlier GHC versions. Of course requiring
  annotations will also prevent that code from compiling but I believe that it
  will be easier for the programmer to track the source of the problem when
  she gets syntax errors rather than typing errors.

- I don't like the idea of mismatch between open type families and closed type
  families, meaning that injectivity of open type families would be openly
  documented whereas for closed type families it would be hidden.
  Counterargument: there is already a mismatch between open and closed type
  families, since the latter have kind inference.

- If we ever implement injectivity of type C it might not be possible to infer
  injectivity annotation of type C.  I think that this time the algorithm will
  really be exponential.

## Example use cases


In the injectivity paper we presented two practical use cases for injectivity.
If you have more uses cases to demonstrate please add them here.
