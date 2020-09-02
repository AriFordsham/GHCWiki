This page is dedicated to a thorny subject on the way of fully featured linear types: namely what should we do with evidence from multiplicity constraints (and, for that matters, what should these constraints be).

Both these questions are contingent both on the typing algorithms (surface-language type checking and linting) and on how we want to use evidences in core passes. A question that comes up a lot is: why can't multiplicity evidence been treated exactly as type evidence. This page tries to answer this question (the answer, after careful examination, may very well be: it can absolutely be treated the same, but it's not clear at the time of writing this sentence).

## Typing algorithms

### Current state

#### Overview

The general shape of both the type-checking and linting algorithms, as they are currently implemented is the following:

- A notion of _usage_ is collected and returned, much like a type but for each variable. The general idea is that the usage of a variable is `1`, then usages are summed, and scaled like in the paper (there is a bit of uncertainty about how to deal with multiple branches: currently both algorithms use a join operation, but it does have its issues). All of this happens in the `1`/`𝜔` semiring from the paper.
- At the location where a variable is bound, it is bound with a multiplicity. We look at the usage of said variable, and check whether it's compatible with the multiplicity.

There is not currently a native notion of compatibility constraint, the implementation currently uses workaround, based on some eager solving when possible, and reducing to an equality constraint when it can't.

#### Challenges

##### Attaching a cast to a variable

Now, one minor issue that we have, is that, while usages are outputs, like types, there is one per variable, so we need to apply the cast to that variable. It's a slight syntactic complication, but nothing too serious.

##### Implication coercions

Because the check happens at the binding site, the binding site would be the right place to include this evidence. This does ask the question of whether the evidence should be attached to the binder itself (which would solve the point about attaching the cast to the appropriate variable), or if they should be generated close to it, but syntactically distinct (and, possibly, move about).

However, it doesn't make much sense with implication constraints.

Suppose we have

```haskell
data SMult (m :: Multiplicity)
  = SOne :: SMult One
  | SMany :: SMult Many
  
f :: Int # m -> SingMult m -> Int
f x SOne  = x
f x SMany = 0
```

Then we have to solve two different constraints `m ~ One ⟹ compatible One m`, and `m ~ Many ⟹ compatible 0 m`. The constraint solver will make short work of these ones.

But if we place the evidence at the binding site, we run into troubles. Let's look at the desugared core:

```haskell
f = \(x # m) (s :: SingMult m) -> cast x m <evidence from the type checker>
  case s of
    SOne ev1 -> x
    SMany evm -> 0
```

We have several big problems here:
- There is room for just one evidence, whereas we really need two different proofs depending on the branch
- The evidence generated, say, for `m ~ One ⟹ compatible One m` depends on `ev1`, which is free at the site for our multiplicity evidence.

This is why, currently, the type checker only accept reflexivity evidences for multiplicity.

### Changing the type checker

This may suggest that the gather-usage-up-then-check-at-binding-site approach is not the right one. It is, at least, not compatible with implication constraints.

A popular idea is the following: instead of considering usages as output of the algorithm, we take multiplicities as inputs, then when we see something of the form `f u`, with `f :: a #𝜇-> b`, then for every `x` in the environment, whose input multiplicity is `𝜋` we emit a constraint `p+𝜇q~𝜋`. That is, we let the constraint solver decide how `𝜋` is split between the two branches. Then we typecheck `f` with `x # p` and `u` with `x # q`. We can't quite get away with just equality constraints though: when type-checking a variable, we need to emit a constraint saying `𝜋 is compatible with 1`.

In such an algorithm, we need a way to represent the splitting constraints, these really don't look like casts for type constraints. They can appear at every application (and every `let` possibly, though `let`-s are a bit special in Core, and every `case`). And there would, probably, be a constraint more like casts near variables, to say things like: this one variable call counts as being uses `𝜔` times by the way.

There is an essential optimisation which can be done here. If `𝜋` is `𝜔`, then we don't need to generate a split: we can use `p=q=𝜔` since it's a correct split, and unconstraining.

Nevertheless, it's not clear that this won't bog terms with a ton of additional casts. For instance, if we actually need the cast around variables when we use them `𝜔` times, then that's one additional cast on each variable in normal Haskell. Which would probably have an undesirably large effect on compile time with `-O1`. So it's not clear whether this avenue is worth exploring. But we shouldn't discount it either.


## Usage in Core

As far as I [aspiwack] can remember, we are manipulating multiplicity evidence in exactly one place. In this one place, by the way, this is really about multiplicity equalities.

In order to avoid prevent coercions from having an effect on the equational theory of Core, there are two rules surounding casts and functions (omitting symmetries):

```haskell
(f |> co) u  ~~>  (f (u |> co_arg)) |> co_res

(\x -> u) |> co  ~~>  \x' -> u[x\(x |> co_arg)] |> co_res
```

What's happening here is that a coercion on function types is decomposed to expose more 𝛽-redexes.

With multiplicity, however, the coercion is from `a #p -> b` to `c #q -> d`. And if the coercion `p ~ q` is not a reflexivity, then the transformation, as written above, is incorrect. It's easier to see when `p=𝜔` and `q=1`, so we are equating `𝜔~1` using `unsafeCoerce` (this happens frequently when building foundations for linearly typed abstractions: here is a function that doesn't know it is linear, so let me actually claim it to be linear). Then, in,

```haskell
(\x -> u) |> co  ~~>  \x' -> u[x\(x |> co_arg)] |> co_res
```

The left hand side will be a of type `c #1 -> d`, while the right hand will have type `c #𝜔 -> d`.

Currently, the only case where the coercion, on multiplicity, is not reflexivity, is because of `unsafeCoerce`. So we can take the following restriction: only fire the rules when the coercion, on multiplicity, can be proved by reflexivity.

However, a more satisfying solution would restore these two rules to always fire, by utilising the multiplicity coercion.

I [aspiwack] haven't analysed what would make sense here. But I consider that, to be viable, a solution to the multiplicity evidence problem, as witnessed by type-checking would also have to handle these two rules.
