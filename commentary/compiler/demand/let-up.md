# Making LetUp more precise


The [demand analyser](commentary/compiler/demand) uses two different rules for handling let bindings, as explained in the [ cardinality paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf). TLDR:

- **LetDown** is used for top-level bindings, recursive bindings and local, non-recursive functions. It closely mimics semantics of an actual function call, as it unleashes the demand signature of the bound RHS at the call site within the let body.
- **LetUp** is used for local, non-recursive thunks. This works more like a type-system: Analysing the let-body reveals its demand on the identifier, in which the RHS of the bindings is analysed exactly once. Only now it is that the results from the body and the RHS get sequentially combined.


There are reasons why we currently need both rules ([ cardinality paper §3.5 and §3.6](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf)), although the general approach of **LetDown** is more flow-sensitive and thus strictly more precise than **LetUp**.


Consider the following running example:

```wiki
let y = ... 
in let x = 2*y
   in if ... then x else y
```


This evaluates `y` only once, but the **LetUp** rule (in contrast to **LetDown**) fails to recognise this:
The inner let-body has a demand environment of `[x :-> <L,1*U>, y :-> <L, 1*U>]`, which combines with the DmdEnv of the RHS `[y :-> <L, 1*U>` to a total DmdEnv for the let-expression binding `x` of `[y :-> <L,U>]`.
E.g., `y` is possibly evaluated multiple times!

## Co-call graphs


Meanwhile, [Call Arity](call-arity), using a **LetUp** strategy at let-bindings exclusively, has countered the exact same loss of precision by employing *Co-call graphs*. Instead of having cardinality information stored in a plain identifier map, where identifiers are assumed to be evaluated with each other, co-call graphs additionally assert that some ids are never evaluated during the same evaluation of the containing expression. 


For the above example, the co-call graph for the inner let-body would specifically *not* contain an edge between `x` and `y`, because they are evaluated on mutually exclusive code paths. Thus, substituting the co-call graph of `x`s RHS into the co-call graph of the let-body will *not* induce a loop on `y`, so y will be recognised as evaluated at most once, which is what we want here.

## BOTH/OR trees


The idea behind co-call graphs translates to demand analysis by *being as lazy as possible* in computing `DmdEnv`s. 


In particular, `bothDmdType` and `lubDmdType` currently smash together both incoming `DmdEnv`s. This forgets important structure for the above example: Namely that `x` and `y` are evaluated mutually exclusively.


To remedy that, we could model `DmdEnv`s as a tree structure (leaving `Termination`-based nastiness aside for a moment):

```wiki
data DmdEnv'
  = Empty
  | Var Id Demand
  | Or DmdEnv' DmdEnv'
  | Both DmdEnv' DmdEnv'

flattenDmdEnv' :: DmdEnv' -> DmdEnv
flattenDmdEnv' env =
  case env of
    Empty -> emptyVarEnv
    Var id dmd -> unitVarEnv id dmd
    Or env1 env2 -> lubDmdEnv env1 env2
    Both env1 env2 -> bothDmdEnv env1 env2
```


This is essentially the interpreter pattern, with `flattenDmdEnv'` being an interpreter in terms of old `DmdEnv`.


However, as we'll see in a second, this buys us the ability to perform proper substitution when resolving let-bindings with **LetUp**, instead of just deleting the bound id from the body env and `both`ing with the result of the RHS.


For the above example, we get `(Both (...) (Or (Var x <S,1*U>) (Var y <S,1*U>)))` as the `DmdEnv'` of the let-body and `(Var y <L,1*U>)` as the `DmdEnv'` of `x`s RHS under the recorded demand `<L,1*U>` on `x` within the body.
We can now *substitute* the `DmdEnv'` of `x`s RHS into all occurences of `x` within the `DmdEnv'` of the let-body:

```wiki
  (Both (...) (Or env             (Var y <S,1*U>)))[env := (Var y <L,1*U>)]
= (Both (...) (Or (Var y <L,1*U>) (Var y <S,1*U>)))
```


If we look up the demand on `y` in the resulting `DmdEnv'`, we find that `y` is put under demand `<L,1*U>`, so used at most once, which is what we wanted.


Note however that it is still not detected as being used strictly! 
For this, we would need to analyse `x`s RHS under the demand of the use site we substitute the `DmdEnv'` into, much like **LetDown** would.
Let's revisit this later.

## Thunk Sharing


There's an(other) issue with the substitution model.


Substitution appeals because it approximates **LetDown** in terms of precision, but it also suffers from the same imprecision when it comes to modeling thunk sharing:

```wiki
let y = ..
in let x = 2*y
   in x + x
```


Note that although `x` is evaluated twice, the evaluation of its RHS is shared, so that `y` is evaluated at most once.
This is also what the current demand analysis finds out.


Let's look at the `DmdEnv'` for the inner let-body:

```wiki
(Both
  (Var x <S,1*U>) 
  (Var x <S,1*U>))
```


Substituting the `DmdEnv'` of `x`s RHS:

```wiki
(Both
  (Var y <L,1*U>) 
  (Var y <L,1*U>))
```


Ouch! Substitution forgets about sharing and let's us see an imprecise upper bound of `w*U`.
What we are lacking is some kind of model for the runtime heap.


How does **LetUp** solve this? 
Well, it operates under the simple assumption that the binding is evaluated exactly once in the entire body, if it's evaluated at all.
So, it's OK to `both` the `DmdEnv'` of the RHS to that of the let body at the root. This won't destroy sharing!


Now, for the first example (`if ... then x else y`), it is quite obvious that the bound id `x` doesn't even occur in the first `Or` branch of the `DmdEnv'` of the let body (after deleting `x` from it).
Only the second branch evaluates `x` at all, so it should be safe to 'push' the *graft point*, where we graft the `DmdEnv'` of the RHS of `x` onto the `DmdEnv'` of the let body, down to the [ ''most recent common ancestor'' (MCRA)](https://de.wikipedia.org/wiki/Most_recent_common_ancestor) of all occurences of `x` in the body.


For the first example:

```wiki
-- [] marks the MCRA of all previous occurences of `x`, which were already deleted
  graft (\t -> (Both t (Var y <L,1*U>))) onto (Both (...) (Or [Empty] (Var y <S,1*U>))))
= (Both (...) (Or (Both Empty (Var y <L,1*U>)) (Var y <S,1*U>))))
```

## Multiple graft points


Grafting the `DmdEnv'` of the RHS of `x` at the MCRA of all occurences of `x` in the `DmdEnv'` of the body is AFAICT the only solution that doesn't duplicate the `DmdEnv'` of the RHS into the tree, so should be quite predictable from a performance perspective.


However, there are shortcomings, exposed by examples like this:

```wiki
let y = ... in
let x = 2*y in
if ... then
  x
else if ... then
  y 
else if ... then
  x

(Both 
  (...)
  (Or
    (Var x <S,1*U>)
    (Both
      (...)
      (Or
        (Var y <S,1*U>)
        (Var x <S,1*U>)))))
```


The MCRA of all occurences of `x` is the root of the `DmdEnv'`. That's unfortunate, as this means we aren't more precise than **LetUp** here, which is insufficient to realise that `y` is only evaluated at most once.


This tension can only be resolved by having multiple graft points, one at each occurence of `x`.


When is this safe to do? 
It's always safe to push down a graft point when there's only one child node in which `x` occured (that's why it's safe to choose the MCRA).
For the case when `x` occured in both children (can only be `Both` or `Or`):

1. It's safe to push graft points down both branches of an `Or`, as this can't destroy sharing. The only gripe is that the tree grows linear in the number of `Or` nodes with this property compared to the MCRA or plain **LetUp** approach.
1. It's not generally safe push graft points down both branches of a `Both`: `(Both (Var x <S,1*U>) (Var x <S,1*U>))` was an earlier example that proofs that.


So, by additionally pushing down graft points along both branches of an `Or` node, if needed, we can handle cases like the above. 
We buy increased precision by possibly super-linear space complexity.
