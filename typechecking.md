
This page is to track thoughts about the core type-checking (constraint-generation) algorithm.

# New Proposal (August 2019)

https://gitlab.haskell.org/ghc/ghc/merge_requests/1637 adds an open recursive type. With that we can do

```haskell
data ExpType = Check (Type0 ExpType)
             | Infer !InferResult
```

In other words we have a partial type with a number of holes, where pure inference is an outer `Infer ...` as before and pure checking is no `Infer ...` sub-terms. I used this in a toy compiler once and it seemed to work OK.

This enforces 2 as before, but rejects 1. Why stop at function types? Perhaps because of expected "blame" with `case` elimination?

I hope this could be used to make the code for partial type signatures more natural. Even if `case` remains the same, inferring the scrutinee's type rather than checking it against something gleaned from the constructors, partial type signatures generate richer checking cases.

# Current state (August 2019)

Much of this proposal (without `ExpFun`) is now in HEAD and 8.0. We have a simply "hole or type"

```haskell
data ExpType = Check TcType
             | Infer !InferResult
```

# Previous state (Oct 2015)


GHC claims to do bidirectional type-checking, but it doesn't quite do this. In a proper bidirectional type-checking algorithm (as in [the "Practical Type Inference" paper](http://repository.upenn.edu/cis_papers/315/)), the algorithm is either in "infer" mode or "checking" mode, never both. But GHC uses more of a mixed economy.



The relevant function is


```
tcExpr :: HsExpr Name -> TcRhoType -> TcM (HsExpr TcId)
```


where the `TcRhoType` in there is the expected type for the expression. If we are inferring, the expected type will be a `ReturnTv` unification type variable. `ReturnTv`s can unify with *anything* -- polytypes, type families, whatever. In reality, it is more of a hole than a type variable. For example, a `ReturnTv` should never be compared against another type for equality, and type-checking against a `ReturnTv` should always fill in the hole. But none of this is checked.


Because of the lack of clarity around these issues, GHC has some strange behavior around the margins. For example, see #10619.

# Proposed change


For the most part (see below for exceptions), it seems nothing really is gained by the mixed economy -- that is, allowing holes within other types. This design makes the code a little simpler, but we're not taking advantage of type-checking against, say, `(T, <<hole>>)`. So perhaps we can do better.



Concretely, we can change `tcExpr` to look like this:


```
tcExpr :: HsExpr Name -> ExpType -> TcM (HsExpr TcId)
```


where


```
data ExpType = Check TcRhoType                -- checking
             | Infer (TcRef (Maybe TcType))   -- inferring
             | ExpFun ExpType ExpType         -- mixed (but only for arrows)
```


By using `ExpType`, we can obviate the need for `ReturnTv` and protect against certain bad things. Specifically, holes should

1. never be nested (except in the arrow case)
1. only appear to the right of a colon in a typing judgment (never in the context)
1. always be used linearly

`ExpType` gives us 1 and 2. 3 is checked dynamically by making sure we don't fill a hole twice.


The exception to the mixed economy bit is for arrows. For example, consider

```wiki
((\x y -> (y 'a', y True)) 5) :: (forall a. a -> a) -> (Char, Bool)
```


That bizarre expression should be able to type-check. But it can only do so by putting a hole under an arrow.


The other place where it comes up is in dealing with rebindable syntax. For example, we want to type-check `negate` against `hole1 -> hole2`. Perhaps by having the arrow in the structure, type inference could make more progress than it could otherwise.

## A specification


Simon has suggested how to write a declarative specification for the `ExpType` story. He has suggested this key rule:

```wiki
G |- e1 : box(t1)  -> bt2   G |- e2 : t1
-----------------------------------------------------------  APP
G |- e1 e2 : bt2
```


But no one has worked out the details. 
