[core-spec]: uploads/ceaedb9ec409555c80ae5a97cc47470e/minicore.pdf

On this page we describe the principles behind the implementation of the linear types extension as described at [LinearTypes](linear-types).

The current implementation can be reviewed on [Gitlab (!852)](https://gitlab.haskell.org/ghc/ghc/merge_requests/852). There is also a [ GitHub repository](https://github.com/tweag/ghc/tree/linear-types) which contains the history of the project and a list of known issues.


Authors or the implementation are:

- Krzysztof Gogolewski
- Matthew Pickering
- Arnaud Spiwack

*Core specification.*  The implementation extends GHC's Core language to support linearity.  [This document][core-spec] formalises the extensions to Core ([Latex source](https://github.com/tweag/linear-types/blob/master/minicore.lhs)).  It should be treated as our reference document, specifying precisely what Core is intended to be.

*Table of contents*

--------------
[[_TOC_]]

## Very high-level summary


The linear types branch adds a new extension `-XLinearTypes`. This allows programs to use the linear arrow `a #-> b` and the multiplicity-polymorphic arrow `FUN m a b` (`FUN` is to come with a mix-fix syntax: `a # m -> b`), where `m` is any type which belongs to the new `Multiplicity` kind, with two types `One` and `Omega`.  The linear arrow `⊸` is available when compiling with `-XUnicodeSyntax`.


Example:

```wiki
id :: a # One -> a   -- or: id :: a #-> a
id x = x
```


is the linear identity function.

### funTyCon and unrestrictedFunTyCon

In the GHC, prior to linear types, `funTyCon` is the `(->)` type, of kind `forall {a b :: RuntimeRep}. TYPE a -> TYPE b -> Type`.

In the linear types branch, `funTyCon` refers to a new primitive type called `FUN`, of kind `forall (m :: Multiplicity) -> forall {a b :: RuntimeRep}. TYPE a -> TYPE b -> Type`.

The prefix `(->)` arrow is now a type synonym for `FUN 'Omega` and is available as `unrestrictedFunTyCon`. The partial application `FUN 'Omega` is pretty-printed as `(->)`, just like `TYPE 'LiftedRep` is pretty-printed as `Type` (or `*`). Currently, the ghci command `:info` does not show instances for type synonyms (issue #16475). As a workaround, we detect this fact in `orphNamesOfType`. Note that the kind of `(->)` stays the same. 

There is no `(#->)` (linear arrow) type constructor yet. It will eventually be defined as a type synonym in `TysWiredIn` like `(->)`.

The additional multiplicity parameter to `funTyCon` is the main principle behind the implementation. The rest of the implementation is essentially correctly propagating and calculating linearity information whenever a `FunTy` is created.

In binders, where we stored a type, we now store a pair of type and multiplicity. This is achieved with data type `Scaled` which stores a type together with a multiplicity (it's a convention similar to `Located` for storing source locations).

## Frontend

### HsType

We need to distinguish `a -> b`, `a #-> b` and `a # m -> b` in the surface syntax. The `HsFunTy` constructor has an extra field containing `HsArrow`, which stores this information:


```
data HsArrow pass
  = HsUnrestrictedArrow
    -- ^ a -> b
  | HsLinearArrow
    -- ^ a #-> b
  | HsExplicitMult (LHsType pass)
    -- ^ a # m -> b (very much including `a # Omega -> b`! This is how the
    -- programmer wrote it). It is stored as an `HsType` so as to preserve the
    -- syntax as written in the program.
```
### Typechecking

Multiplicities are stored as types (`type Mult = Type`), of kind `Multiplicity`. There are pattern synonyms `One` and `Omega` for convenient access.

The checking algorithm is as follows:

- The typechecking monad is enriched with an extra writer-like state called the *usage environment*, which is a mapping from variables to multiplicities. Variables not present in the usage environment are considered to have usage Zero (which is not a multiplicity).
- The usage environment output by `u v` is `U1 + p * U2` where

  - `U1` is the usage environment output by `u`
  - `U2` is the usage environment output by `v`
  - `u :: _ # p -> _`
- Adding a new variable `x` with multiplicity `p` to the context to typecheck `u` is performed as

  - `x` is added to the context
  - `u` is typechecked in that context
  - The usage `q` of `x` is retrieved from the output usage environment
  - It is checked that `q <= p`
  - `x` is removed from the output usage environment


Concretely, there are precisely two places where we check how often variables are used.

1. In `TcEnv.tc_extend_local_env`, which is the function which brings local variables into scope. Upon exiting a binder, we call `check_binder` to ensure that the variable usage is compatible with the declared multiplicity (if no multiplicity was declared, a fresh existential multiplicity variable is created instead). In `check_binder` there is a call to `submult` which checks for obvious cases such as `1 <= p` before we delegate to `tcSubMult`. The function `tcSubMult` should generate the constraint `π ⩽ ρ`. For now, we do not have submultiplicity constraints and generate `π ~ ρ` (`tcEqMult`) as an approximation.

2. In `tc_sub_type_ds`, In the `FunTy` case, we unify the arrow multiplicity which can lead to the unification of multiplicity variables.

  - `tc_sub_type_ds` emits constraints of the form `π = ρ`, this is achieved by a call to `tcEqMult` which just calls`tc_sub_type_ds` recursively.

A better implementation would probably emit a real constraint `pi <= rho` and then add logic for solving it to the constraint solver. The current ad-hoc approach reduces the task of checking the relation to checking certain equality constraints.

#### Solving constraints

Constraint solving is not completely designed yet. The current implementation follows very simple rules, to get the implementation off the ground. Basically both equality and subsumption constraints are treated as syntactic equality unification (as opposed, in particular, to unification up to laws of multiplicities as in the proposal). There are a few other rules (described below) which are necessary to type even simple linear programs:

##### The 1 \<= p rule


Given the current domain, it is true that `1` is the smallest element. As such, we assume `1` is smaller than everything which allows more functions to type check.


This is implemented by making sure to call `submult` on the multiplicities before passing them to the normal unifier which knows nothing special about multiplicities. This can be seen at both
`tc_extend_local_env` and `tc_sub_type_ds`. At the moment we also get much better error messages by doing this short circuiting.

##### Complex constraints


Multiplication and addition are approximated.  `p1 + p2` is always simplified to `Omega`. `p1 * p2 ≤ p` is simplified to `p1 ≤ p ∧ p2 ≤ p`.

### Defaulting

Unsolved multiplicity variables are defaulted to `Omega`. We detect them by calling `isMultiplicityVar`; this happens in :

1. `TcSimplify.defaultTyVarTcS`
2. `TcMType.defaultTyVar`

We never infer multiplicity polymorphic arrows (like levity polymorphism). Any type variables which get to the top level are default to `Omega`. Thus, in most cases the multiplicity argument is defaulted to `Omega` or forced to be `Omega` by unification.

Incidentally, this lead for a proposal for an improved defaulting strategy. See [Gitlab issue #17201](https://gitlab.haskell.org/ghc/ghc/issues/17201).

### Data Constructors are polymorphic

In linear types, constructors of data types are multiplicity-polymorphic. For example,

```
(,) :: forall {m :: Multiplicity} {n :: Multiplicity} a b. a # m -> b # n -> (a,b)
```

This is important for backwards compatibility; if we used a linear type `a #-> b #-> (a,b)`, code such as `f (,)` [where `f :: (a -> b -> (a,b)) -> c`] would break.

This variable is inferred, so that there's no conflict with visible type application.

When constructors are used as patterns, the fields are treated as linear (i.e. `a #-> b #-> (a,b)`).

##### Implementation

The way this is implemented is that data constructors, when desugared, are eta-expanded with multiplicity-polymorphic binders. Which is done by adding `WpFun` wrappers in `tc_infer_id` (look for `Note [Linear fields generalization]`).

Because no variable can have a levity-polymorphic argument, this instantiates the runtime-representation variables of unboxed tuples and sum. Which prevents visible type application on the runtime-representation variables of unboxed tuples and sum. We haven't found any example of use of this feature yet. So it shouldn't break any code.

### Do-notation/rebindable syntax


Type-checking of the do notation relies on typing templates (`SyntaxOpType`). The type of `(>>=)` (in particular), in the context, is matched against its template, returning the type.


In order to support `(>>=)` operators with varying multiplicities, function templates (`SynFun`) now return their multiplicity. Specifically `SyntaxOpType` now returns a second list of all the multiplicities (from left to right) which it computed.


I (aspiwack) introduced a second list at a time where there wasn't a correspondence between types and multiplicities. It could be changed to return multiplicities as types in the main list. It's not much of a simplification, though.

## Core

### Core specification

The implementation extends GHC's Core language to support linearity.  This document:
* [Linear Mini-Core][core-spec]

formalises the extensions to Core.  It should be treated as our reference document, specifying precisely what Core is intended to be.

It is more complete than the paper. It is work in progress, and needs a lot more English text to explain what is going on.

### The big difference: usages on lets

In the paper (and in the frontend type system), all binders have a
multiplicity, and the body of the binder must abide by it. So you'd
have an error in

```haskell
let_1 x = u in
case v of
  C {} -> … x …
  D {} -> 0
```

Because a linear `let` requires `x` to appear linearly in both `C` and
`D`.

But, in Core, this form can naturally occur. In many ways.

#### Problem 1: Join-points

The most obvious, perhaps, is because of join-point creation: when
duplicating a case (in the case-of-case transformation), to avoid code
explosion, the branches of the case are first made into join points:

```haskell
case e of
  Pat1 -> u
  Pat2 -> v
~~>
let j1 = u in
let j2 = v in
case e of
  Pat1 -> j1
  Pat2 -> j2
```

If there is any linear variable in `u` and `v`, then the standard
`let` rule above will fail (since `j1` occurs only in one branch, and
so does `j2`).

#### Problem 2: Float out

In float out, let bindings are moved upwards in the expression
tree. In particular

```haskell
case e of
  Pat1 -> let x = u in v
  Pat2 -> w
~~>
let x = u in
case e of
  Pat1 -> v
  Pat2 -> w
```

Now `x` only appear in `v`. If `let x` is a linear let, this would
fail to lint.

Besides the actual float out part, there are other transformation
(most notably the simplifier) which do float some lets out

#### Problem 3: inlining

Inlining finds individual use site of a `let`-bound variable, and
will replace the variable by the right-hand side. This transformation
occurs for a variety of reasons, the simplifier uses it in the
case-of-known-constructor transformation (to see through definition
for opportunities). Crucially, not all use site update all the time.

```haskell
let x = u in
case e of
  Pat1 -> … x …
  Pat2 -> … x …
~~>
let x = u in
case e of
  Pat1 -> … u …
  Pat2 -> … x …
```

Again, `x` appears in one branch and not the other, which would break
for a linear let.

#### This also occurs for recursive lets

Less obviously, we can have a similar pattern occurring in a recursive let.

The main culprit for this is the case-of-case rule for join points:

```haskell
case join j = u in v of {…}
~~>
join j = case u of {…} in case v of {…}
```

```haskell
case jump j … of {…}
~~>
jump j …
```

Now consider the following example, where `<branches>` capture some
linear variables `y`

```haskell
case join rec j n = case e of
       Pat1 -> jump j (n-1)
       Pat2 -> x
     in …
of
{ <branches> }
~~>
join rec j n = case e of
  Pat1 -> jump j (n-1)
  Pat2 -> case x of { <branches> }
in …
```

Now `y` appears in the second branch, and not the first!

A similar phenomenon can happen on regular (non-join point) recursive
lets via the Static Argument Transformation. Whereby

```haskell
let rec f x n =
  if n = 0 then x
  else f x (n-1)
~~>
let f x =
  let rec loop n =
    if n = 0 then x
    else loop (n-1)
```

#### The solution

As we've seen above, `let`-binders, in Core, are much more mobile than
linear lets can afford. Deactivating these rules for linear lets is
out of the question. For each of the individual problem we could try
and partially fix the rewrite rules. But better, especially with the
accumulation of problematic rules, is to change the typing rule of
`let` to accommodate this transformation.

The goal, then, is to understand patterns such as

```haskell
let x = (y, z) in
case e of
  Pat1 -> … x …
  Pat2 -> … y … z …
```

The idea is to notice that using `x` and using both `y` and `z` is the
same thing. After all, The only thing that we need to guarantee is
that if the expression is consumed exactly once, then `y` and `z` are
consumed exactly once. In a sense, we don't want to count `let x = (y,
z)` as consuming `y` and `z`, but the call of `x` itself.

##### A model

We can model this without changing the language: it suffices to
lambda-lift `x` (at least with respect to its non-unrestricted
variables).

```haskell
let x y' z' = (y', z') in
case e of
  Pat1 -> … x y z …
  Pat2 -> … y … z …
```

Since `y` and `z` only appear at the use site they are consumed by the
use site. And `x` has now become an unrestricted `let`-binder, it can
move wherever.

##### Internalising the rules

The problem with lambda-lifting is that it is, generally speaking,
slower. There are exceptions, but GHC prefers avoiding lambda-lifting
(static argument transformation is really the opposite of
lambda-lifting, for instance).

But we can type lets _as if_ they were lambda-lifted.

We annotate let-binders with a so-called usage annotation: it records
all the variables which we would need to lambda-lift over. And, at
use-site, we count the usage of these variables. Our example becomes

```haskell
let_𝛥 x = (y, z) in
case e of
  Pat1 -> … x …
  Pat2 -> … y … z …
```

With `𝛥=[y :-> 1, z :-> 1]`. And the `Pat1 -> … x …` branch would
count as consuming `y` and `z` once (but not `x`, which would be
unrestricted under the lambda-lifted interpretation).

### Core Lint


Core variables are changed to carry either a multiplicity if the are bound by a `let` or a `case`, or a usage annotation (`𝛥` above) if they are bound by a let. The multiplicity annotation on the case, from the paper, is the represented as the multiplicity of the case-binder.

(As of writing this section, the usage annotation is only partially implemented)

The linter is modified in two ways to account for linearity. First the main loop (`lintCoreExpr`) returns a usage environment in addition to a type. This is like the surface-language type checker. In addition, the environment in the `LintM` monad is modified to carry a mapping from alias-like variables to the usage environment of their right-hand side.

When the call-site of a variable `x` is linted, if `x` has a multiplicity, then it emits the usage environment `[x :-> 1]`. If it has a usage environment, it emits, instead, the usage environment computed at the call site (essentially the same as the usage annotation on `x`, up to some substitutions).

### FunTyCon

`FunTy` is a special case of a `Type`. It is a fully applied function type constructor, so now a function type constructor with five arguments.
This special case is constructed in `mkTyConApp`. The problems come when a `FunTy` is deconstructed, for example `repSplitTyConApp_maybe`, if this
list is not the right length then you get some very confusing errors. The place which was hardest to track down was in `Coercion` where `decomposeFunCo`
had some magic numbers corresponding to the the position of the types of the function arguments.


Look for `Note [Function coercions]` and grep for lists of exactly length 5 if you modify this for whatever reason.

### FunCo


FunCo is modified to take an extra coercion argument which corresponds to coercions between multiplicities. This was added because there was a point to where `mkTyConAppCo` took a coercion as an argument and needed
to produce a `Mult`. It was defaulted to `Omega` for a long time but eventually I changed it to `Coercion`. This seemed to work but there were some problems in CoreLint which mean the check for the role of the coercion
has been commented out for now.


A noteworthy consequence of having an extra argument to `FunTyCon` and `FunCo`, is that some hand-written numbers in the code must change. Indeed, the injectivity of type constructors (that `C a ~ C b` implies `a ~ b`) is implemented by projecting an argument referred by number. This works for `FunCo` too. And it is made use of directly in the code, where the field number is manually written. These field numbers had to be changed. A more robust solution would be to name the projections which are used in the code, and define them close to the definition of `FunCo`.

#### `splitFunTy` and `mkFunTy`.


The core of the implementation is a change to `splitFunTy`. As the arrow now has an associated multiplicity, `splitFunTy` must also return the multiplicity of the arrow. Many changes to the compiler arise from situations where
either `splitFunTy` is called and then we must keep track of the additional information it returns. Further, when there is a call to `mkFunTy`, we must also supply a multiplicity.

### Core to core passes

#### Case-of-case

##### The case-of-case rule

We have to be careful with the case multiplicity during case-of-case:

```haskell
case_𝜔 (case_1 u of { Foo x -> x }) of
  Qux _ -> "Bar"
```

A naive case-of-case might change this into

```haskell
case_1 u of 
  Foo x -> case_𝜔 x of
    Qux _ -> "Bar"
```

Oops! `x` is bound linearly by the outer-case, but consumed by a `case_𝜔`.

We need to scale the newly outer `case` by to account for the commutation.

```haskell
case_𝜔 u of 
  Foo x -> case_𝜔 x of
    Qux _ -> "Bar"
```

More generally, the transformation is

```haskell
case_𝜋 (case_𝜇 u of { Pat_i -> v_i } ) of { … }
⤳
case_(𝜋𝜇) u of { Pat_i -> case_𝜋 v_i of { … } } 
```

For the original discussion see: [https://github.com/tweag/ghc/issues/78](https://github.com/tweag/ghc/issues/78) and [ https://github.com/tweag/ghc/pull/87](https://github.com/tweag/ghc/pull/87)

##### Implementation overview

Case-of-case is implemented in the simplifier. There we have a term to simplify, and an evaluation context (aka continuation in the simplifier's code), from which the term comes from. When simplifying a `case` expression, case-of-case is implemented by simplifying the branches under said evaluation context.

Therefore we need to know how much scaling factor must be applied by commuting thus with the evaluation context. Which we compute recursively on the structure of the evaluation context using the `contHoleScaling` function.

##### Interaction with case-of-known-constructor

In the simplifier, case-of-case is combined with case-of-known constructor, so an expression such as

```haskell
case_𝜔 (case_1 u of Foo x -> Qux x) of
  Qux _ -> "Bar"
```

will be simplified to

```haskell
case_𝜔 u of
  Foo x -> "Bar"
```

It needs to be a `case_𝜔`, since `x` is not used. Despite the fact that the outside `case` has disapeared. It's readily handled by the implementation above, it is worth noting however, since it may make debuggint the multiplicities in the simplifier harder.

#### Pushing function-type coercions



Coercions of kind `a -> b ~ c -> d` are routinely pushed through lambdas or application as follows


```
(f |> co) u  ~~>  (f (u |> co_arg)) |> co_res

(\x -> u) |> co  ~~>  \x' -> u[x\(x |> co_arg)] |> co_res
```


However, this can't always be done when multiplicities are involved: the multiplicity could be coerced (in particular, by `unsafeCoerce`). So, it's possible that the left hand side of these rules is well-typed, while the right hand side isn't. Here is an example of this phenomenon.


```
-- Assuming co :: (Int -> ()) ~ (Int #-> ())

fun x ::(1) Int -> (fun _ -> () |> co) x  ~~>  fun x ::(1) Int -> (fun _ ::(ω) Int -> ()) x
```


To prevent this, we guard this reduction with the condition that the multiplicity component of the coercion is a reflexivity coercion.


I (aspiwack) believe we are only checking whether the coercion is syntactically a reflexivity coercion. This is probably over conservative as coercions are not necessarily fully simplified. We probably need a finer grained test, otherwise this will cause performance regressions.

#### CPR worker/wrapper split

##### Case multiplicity



The CPR split transforms a function


```
f :: A -> B
```


Into a pair


```
$wf :: A -> (# C, D #) -- supposing f's B is built with a constructor with two arguments

f x = case $wf x of
  (# y, z #) -> B y z
```


With linear types, we still need to choose a multiplicity for the case. The correct multiplicity is `1`. It works whether `f` has linear arguments or not. So, the linear transformation is:


```
$wf :: A -> (# C, D #) -- supposing f's B is built with a constructor with two arguments

f x = case_1 $wf x of
  (# y, z #) -> B y z
```


The worker is defined similarly, and also uses a `case_1`.

##### Unrestricted fields



Consider a function


```
f :: Int -> Unrestricted A
```


The argument type doesn't matter, the result type does.



The CPR split yields:


```
$wf :: Int -> (# A #)

f x = case_1 $wf x of
  (# y #) -> Unrestricted y
```


This is ill-typed unless `(# #)` has an unrestricted field (currently, all fields of an unboxed tuple are linear).



The principled solution is to have unboxed tuple be parametrised by the multiplicity of their field, that is


```
type (#,#) :: forall r s. Multiplicity -> Multiplicity -> TYPE r -> TYPE s -> TYPE …

data (#,#) p q a b where
  (#,#) :: a :p-> b :q-> (#,#) p q a b
```


At least the unboxed tuples used by core should have such a type. It can also be a user-facing feature.


At the moment, however, CPR is simply restricted to the case where the constructor only has linear field, precluding some optimisation, but being less intrusive.

#### Common subexpression elimination

When CSE encounters a binder, it will check whether it is unrestricted (or alias). If it isn't, no CSE-rule is added.

Indeed, it doesn't make sense to do CSE for a binding which can't be freely
shared or dropped. In particular linear bindings, but this is true for
any binding whose multiplicity contains a variable.

This shows up, in particular, when performing a substitution

```
CSE[let x # 'One = y in x]
==> let x # 'One = y in CSE[x[x\y]]
==> let x # 'One = y in y
```

Here `x` doesn't appear in the body, but it is required by linearity!
Also `y` appears shared, while we expect it to be a linear variable.

This is usually not a problem with let-binders because their multiplicity
is set to `Alias` in prior phases. But we don't have such luxury for case
binders. Still, substitution of the case binder by the scrutinee happens
routinely in CSE to discover more CSE opportunities (this particular point is explained in `Note [CSE for case expressions]`).

It's alright, though! Because there is never a need to share linear
definitions.


## Debugging


If you are debugging then it is very useful to turn on the explicit printing of weights in two places.

1. The Outputable instance for `Scaled` in `Multiplicity`.
2. The multiplicity of variables in `Var`, line 309.


There are disabled by default as they affect test output.

## Misc

- Patterns are type checked in a *context multiplicity* which scales the constructor fields, extending the `case_p` from the paper.