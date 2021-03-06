# Exhaustiveness/Redundancy Check


As stated in #595, GHC's overlapping/non-exhaustive pattern checking is old and
crufty and misbehaves with several GHC's extensions, notably GADTs. In this page
we describe the problem and the algorithm.  It forms part of GHC 8.0.

## Status


The ticket #11528 tracks our aspiration to return to a more elegant (but currently less performant) implementation.

See the ~"pattern match warnings" label for many other relevant tickets.



## Background

- The paper on which the previous approach was based [Two techniques for compiling lazy pattern matching](http://moscova.inria.fr/~maranget/papers/lazy-pats-derniere.ps.gz)
- Peter Sestoft's paper for negative patterns [ML's pattern matching compilation and partial evaluation](http://lambda.csail.mit.edu/~chet/papers/others/s/sestoft/sestoft96ml.pdf)

## Our solution

- Our paper, describing the algorithm we implemented [GADTs meet their match](http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf).
- [PatternMatchCheckImplementation](pattern-match-check-implementation) talks about the implementation in GHC.

## Related tickets

Ones that are closed are still useful examples in the wild; they were only closed as duplicates:

- #29
- #322
- #366
- #595
- #851
- #1307
- #2006
- #2204
- #3078
- #3927
- #4139
- #5724
- #5728
- #5762
- #6124
- #8016
- #8494
- #8779
- #8853
- #8970
- #9113
- #9951
- #10116
- #10600

# The main problem we wish to solve


Since GHC's exhaustiveness/redundancy checker was outdated, it did not take into account constraints introduced
by GADT matches when reporting warnings. This is illustrated in the following example (#3927):


```haskell
data T a where
  T1 :: T Int
  T2 :: T Bool

f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
```


Even though the above definition for `f` is exhaustive, we get a warning of the form:

```wiki
    Pattern match(es) are non-exhaustive
    In an equation for `f':
        Patterns not matched:
            T1 T2
            T2 T1
```


Obviously, both pattern vectors issued as not matched, are ill-typed, because they both generate the inconsistent
constraint `Int ~ Bool`. This becomes more clear if we rewrite the definition of `T` in the equivalent form:


```haskell
data T a where
  T1 :: forall a. (a ~ Int)  => T a
  T2 :: forall a. (a ~ Bool) => T a
```


Additionally, if we add one more branch to `f`:


```haskell
f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
f _  _  = undefined -- inaccessible
```


we get no warning about the redundancy of the last clause.

### Two more problems


It has not been only GADTs that have not been handled by the coverage/exhaustiveness checker. More exotic pattern
matching features like pattern guards, view patterns, overloaded literals etc. usually made the pattern match
checker give imprecise warnings because there was no systematic way to treat them. Additionally, laziness has been
fully taken into account by the previous checker. The new pattern match checker addresses these problems too.

# General approach


Note that improving the redundancy check is quite more challenging than the exhaustiveness check. For
exhaustiveness it is sufficient to collect all potentially missing vectors using the previous syntax-based approach
and then filter out the ill-typed. Nevertheless, for redundancy we need to compute the cases that are covered by
every clause and then filter out the ill-typed. The difficulty lies in the fact that what the last branch of `f`
covers depends on what remains uncovered by the above two clauses. This indicates that for the redundancy check we
need an incremental way of computing uncovered vectors.

## The solution


Until now, the algorithm used by GHC was based on a technique originally introduced for compilation of pattern
matching in decision trees (see paper above). Hence, it used a column-based approach to traverse the pattern
matrix, which cannot be used incrementally as we described above. (By pattern matrix we mean a list of matches.
Since every clause has the same length, --let's call say `m`-- a match of `n` clauses, each of length `m` defines
a pattern matrix of dimensions `n x m`. For more details see the paper [Two techniques for compiling lazy pattern matching](http://moscova.inria.fr/~maranget/papers/lazy-pats-derniere.ps.gz)).


Additionally, this pattern matrix approach assumes that each clause has the same length, which is rather restrictive
since we may have arbitrary guards accompanying a pattern vector (clauses may have different lengths).


Instead, we traverse the clauses line-by-line (one-by-one, from left to right, top-down), following the semantics
of the source language.
The general approach is the following:
We assume that initially nothing is covered (everything is uncovered) and the, for every clause we compute:

- Which cases it covers
- If it forces the evaluation of arguments (see Laziness below)
- Which cases are left unhandled


There are two major differences with previous approaches:

- We use a core pattern syntax that allows guards to appear **anywhere** in a clause. As we have shown in the
  paper, in this expressive pattern language we can encode all source features (and even more).
- The sets of covered/uncovered and divergent cases we mention above do not contain pattern vectors. Instead,
  they contain vectors accompanied by a set of type and term constraints. We illustrate:


For example, for function `f` above we have:

- initial_missing = `[(x y)]` where `x :: T a` and `y :: T a`

  ```haskell
  f T1 T1 = True -- first clause
  ```
- Covers `[(T1 T1 |> {a ~ Int, a ~ Int})]`
- Forces the evaluation of the 1st argument
- If 1st argument is `T1` forces the evaluation of the 2nd argument
- Remain uncovered:

  ```
  [ (T2 y  |> {a ~ Bool})
  , (T1 T2 |> {a ~ Int, a ~ Bool) ]
  ```


The main idea is that every vector we generate comes with a set of constraints that have to be satisfied. This set
of constraints is the ` |> {...}`-part of every vector. Now it becomes apparent that case
`(T1 T2 |> {a ~ Int, a ~ Bool)` can be removed. The type constraints it contains
(`a ~ Int, a ~ Bool`) is not satisfiable (which in turn means that it represents an inaccessible case).
Removing the second vector gives us:

```
uncovered = [ (T2 y  |> {a ~ Bool})]
```


With this, we now process the second clause:


```haskell
f T2 T2 = False -- second clause
```

- Covers `[(T2 T2 |> {a ~ Bool, a ~ Bool})]`
- If 1st argument is `T2` forces the evaluation of the 2nd argument
- Remain uncovered `[(T2 T1 |> {a ~ Bool, a ~ Int})]`


Same as before, the uncovered set contains non satisfiable constraints, so we end up with

```haskell
uncovered = []
```


Finally, checking the last clause given that nothing is uncovered we get: 


```haskell
f _  _  = undefined -- third clause (inaccessible)
```

- Covers: `[]`
- Doesn't force anything
- Remain uncovered: `[]`


Which means that we rightly now detect the last clause as redundant. Also note that the uncovered set was empty
after the first clauses, which means that the first two clauses make `f` exhaustive and the bogus warnings go
away.


Although not illustrated here, we check for satisfiability both the covered and the uncovered sets of clauses.
Finally, note that in this example we showed only the type constraints, for simplicity. We also store and check
term constraints so that we can handle guards (see section below).

# Pattern Guards



A pattern guard (`p <- e`) behaves exactly like if we had one more argument to pattern match against. For example:


```haskell
f x :: [a] -> Bool
f x | (z:zs) <- x = ...
```


is equivalent to:


```haskell
g x :: [a] -> [a] -> Bool
g x (z:zs) = ...

f :: [a] -> Bool
f x = g x x
```


Using this trick, we can reason about pattern guards in the same way we reason about normal pattern matching.
Consider the following example:


```haskell
-- the above example, written using guards
f :: T a -> T a -> Bool
f c d | T1 <- c, T1 <- d = True
      | T2 <- c, T2 <- d = False
      | otherwise        = undefined -- inaccessible
```


Again, we have:

- initial_missing = `[(x y)]` where `x :: T a` and `y :: T a`

- 1st Clause: Covered (z-variables represent the **fake** arguments we match against):

  ```wiki
  [(x y |> { x ~ c, z1 ~ c, c ~ T1, a ~ Int
           , y ~ d, z2 ~ d, d ~ T1, a ~ Int })]
  ```

  Solving the constraints gives us a solution: { x = c = z1 = T1, y = d = z2 = T2 } which we can use to
  simplify the clause `(x y)` to `(T1 T1)` (exactly like we had in the non-guard example).

- 1st Clause: Uncovered:

  ```wiki
  [(x y |> { x ~ c, z1 ~ c, c ~ T2, a ~ Bool
           , x ~ c, z1 ~ c, c ~ T1, a ~ Int, y ~ d, z2 ~ d, d ~ T2, a ~ Bool })]
  ```

  Again, the second uncovered vector has non-satisfiable constraints and can be removed. Solving the constraints
  for the first uncovered vector gives us a solution: { x = c = z1 = T2 }. Using this, we can again simplify the
  clause `(x y)` to `(T2 y)`, and we end up with exactly the same state we had in the non-guard example.

- etc.


Obviously, pattern guards generate more constraints while the actual patterns of the clause are unaffected
(until we solve and substitute back our findings like we did in the above example). Hence, the expressivity of the
checker concerning guards heavily relies on the expressive power of the term oracle. Similarly, the expressivity
concerning guards totally relies on OutsideIn(X), GHC's inference engine, which we use to check for satisfiability
of type constraints.

# Laziness


The previous algorithm was not exactly laziness-aware. The two examples below illustrate some (rather pathological)
cases:

### Example 1



Consider the following function:


```haskell
g :: Bool -> Bool -> Bool
g _    True = True
g True True = True
g _    _    = False
```


The old checker emits the following warning:

```wiki
    Pattern match(es) are overlapped
    In an equation for `g': g True True = ...
```


Even though this is correct (indeed the second clause is totally overlapped by the first clause), the above warning
until now was used to mean that the respective clause is redundant, which is not true in this case.
If we call `g` we get:

```wiki
ghci> g undefined False
*** Exception: Prelude.undefined
```


because the second clause forces the evaluation of the first argument. Yet, if we remove it we get instead:

```wiki
ghci> g undefined False
False
```


That is, we changed the semantics of `g`. What happens is that all cases that match the second clause match the
first too, which in turn means that the second clause has an inaccessible right-hand-side. Yet, pattern matching in
a lazy language like Haskell is not only used for matching but also for **evaluating**. The second clause is useful,
not for matching, but for evaluating the first argument of `g`. With our new checker these cases can be easily
detected: The covered set for the second clause is empty (it is overlapped from the above equations) but we detect
that it evaluates something. In principle, we follow the following reasoning:

- Covers = Yes, Forces = Yes ==\> Useful clause
- Covers = Yes, Forces = No  ==\> Useful clause
- Covers = No,  Forces = Yes ==\> Clause with inaccessible RHS
- Covers = No,  Forces = No  ==\> Redundant clause


By "useful" we mean that the RHS will be returned if matched.

### Example 2



Interestingly, one can trigger this behaviour indirectly, using GADTs. Consider the function `h`:


```haskell
data F a where
  F1 :: F Int
  F2 :: F Bool

data G a where
  G1 :: G Int
  G2 :: G Char

h :: F a -> G a -> Int
h F1 G1 = 1
h _  G1 = 2 -- what about this clause?
```


Running the check on the first clause leaves us with an uncovered set:

```wiki
uncovered = [ (F2 y  |> {a ~ Bool})
            , (F1 G2 |> {a ~ Int, a ~ Char}) ]
```


and by dropping the second (has non-satisfiable type constraints) we end up with:

```wiki
uncovered = [ (F2 y  |> {a ~ Bool}) ]
```


About the second clause:

- Both the uncovered vector `(F2 y)` and the constraints make clear that the second argument is not evaluated yet.
  By matching with the second clause `(_ G1)` we certainly force the evaluation of the second argument.

- The covered set is:

  ```wiki
  covered = [ (F2 G1  |> {a ~ Bool, a ~Int}) ]
  ```


and because `{a ~ Bool, a ~Int}` is non-satisfiable, the covered set is empty. Hence, even though `h` can never
return a `2`, the second clause is not actually redundant.


Note that this case is a bit different from the one above: If we consider the type ofemits warning that the RHS is
inaccessible `h`, indeed the only well-typed **values** that can match are `F1` and `G1`. But since Haskell is not
call-by-value, `F2 undefined` is a perfectly well-typed combination of arguments! Hence, this example illustrates
the following interesting behaviour:


```haskell
h1 :: F a -> G a -> Int
h1 F1 G1 = 1
h1 _  G1 = 2 -- emits warning that the RHS is inaccessible
             -- but h1 is considered exhaustive

h2 :: F a -> G a -> Int
h2 F1 G1 = 1
  -- emits a warning that h2 is non-exhaustive with (F2 _) being the missing case

h3 :: F a -> G a -> Int
h3 F1 G1 = 1
h3 F2 _  = 2
  -- no warning for this one
```