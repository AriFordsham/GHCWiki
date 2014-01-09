
This is nomeata’s notepad about the nested CPR information:

### Related tickets

- [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600) Main tickets where I mention progress.


Tickets with stuff that would make nested CPR better:

- [\#8598](https://gitlab.haskell.org//ghc/ghc/issues/8598) CPR after IO (partly done)

### Related testcases

- Everything in [source:testsuite/tests/stranal/sigs/](/trac/ghc/browser/testsuite/tests/stranal/sigs)

### TODOs

- Does Nick Frisby’s late λ-lifting alleviate problems when CPR’ing join-points?

  - Need to see if his branch can be merged onto master.
- Paper-Writeup of CPR
- Shouldn’t nested CPR help a lot with Complex-heavy code? Is there something in nofib?
- Try passing CPR information from the scrunitee to the pattern variables. For that: Reverse flow of analysis for complex scrunitees (for simple, we want the demand coming from the body, for complex, this is not so important.)
- Use ticky-profiling to learn more about the effects of nested CPR.
- Look at DmdAnal-related \[SLPJ-Tickets\] and see which ones are affected by nested-cpr.
- Do not destroy join points (see below).
- Can we make sure more stuff gets the `Converging` flag, e.g. after a `case` of an unboxed value? Should case binders get the `Converging` flag? What about pattern match variables in strict data constructors? Unboxed values?
- Why does nested CPR make some stuff so bad?

  - Possibly because of character reboxing. Try avoiding CPR’ing `C#` alltogether!

### Degradation exploration and explanation


At one point, I thought that a major contributor to increased allocations is nested-CPR’ing things returning `String`, causing them to return `(# Char#, String #)`. But removing the `CPR` information from `C#` calls has zero effect on the allocations, both on `master` and on `nested-cpr`. It had very small (positive) effect on code size. Will have to look at Core...


Here are some case studies with extensive commenting of steps and results:

- [wave4main](nested-cpr/wave4main)


And here a summary of the problems identified, and solution attempts

- CPR kill join-points, because the wrapper does not completely cancel with anything else.

  - Detecting join-points at the position of its binding is not enough.
- A recursive function can have a CPR-beneficial recursive call that makes CPR worthwhile, even if it does not help at the initial call. But it is also not unlikely that the recursive call is a tail-call, and CPR-ing has zero effect on that. Then it all depends on the external call.

### Converges detection


Nested CPR is only sound if we know that the nested values are known to converge for sure. The semantics is clear: If `f` has CPR `<...>m(tm(),)`, then in the body of `case f x of (a,b)`, when entering `a`, we are guaranteed termination.


What is the semantics of an outer `t`? Given `f` with CPR `<L>tm()` and `g` with CPR `<S>tm()`? Does the latter even make sense? If so, should `f undefined` have CPR `m()` or `tm()`? Two possibilities:

1. The convergence information a function is what holds if its strictness annotations are fulfilled: So if `g x`  has `tm()` if `x` has `t` (possibly because it has previously been evaluated by the caller), otherwise `m()`. `f x` always has `m ()` (presumably because `x` is _never_ entered when evaluating `f`.
1. The convergence information a function is what holds always. This would in effect prevent `<S>tm()` from happening.


Clearly, 1. holds strictly more information than 2.: Under variant `2`, `<S>tm()` would not occur, while under variant 1 that would be usefully different from `<S>m()`.


Also, under 2, `I#` would not be known to terminate for sure, as it is strict. This would destroy any hope for nested CPR for things like `(Int, Int)`.


For the implementation this implies that we need to be careful when `lub`’ing: If one branch is lazy, but not absent in an argument or free variable), and the other branch is strict, then even if both branches claim to terminate, we need to remove the termination flag (as one had the termination under a stronger hypothesis as the hole result) (Feels inelegant.)


Similar thought needs to be considered whenever one goes up in the lattice (better always go up by using `lub` then...)


In pictures: This is the lattice, which is not a simple product lattice any more:

```wiki
       ------ <L><L>------
     /          | \       \
    /           | <L><L>t  \
<S><L>          |           <S><L>  
 |  \ \         |           |  |  \
 |   \ <S><L>t  |          /   |   <S><L>t 
 |    \         |         /    |
 \     \        |        /     |  
  \     ----- <S><S>-----      | 
   \           |    \          |
    \          |     <S><S>t   /
     \         |              /
      ---------⊥-------------/
```


When evaluating a demand type as a demand transformer, we need to compare the convergence flags of the argument expressions with the strictness demands, and possibly adjust the termination information. I believe can be done using `deferAndUse` as before.


What about nested strictness annotations? For now the hypothesis of the demand transformer only requires the arguments (and free variables) to be terminating; if there is a `<S(S)>`, then it should not be marked as converging. Maybe this can be improved later, using nested CPR.


Some implementation implications:

- There is no unit for `lubDmdType` any more. So for case, use `botDmdType` for no alternatives, and `foldr1` if there are multiple.
- Unlifted variables (e.g. `Int#`) are tricky. Everything is strict in them, so for an \*unlifted\* argument, `<L>t` implies `<S>t` and hence `<S>t ⊔ <L>t = <S>t`, and we really want to make use of that stronger equation. But when lub’ing, we don’t know any more if this is the demand for an unlifted type. So instead, the demand type of `x :: Int#` itself is `{x ↦ <L>} t`, while `x :: Int` continues to have type `{x ↦ <S>} t`.
- It is important that functions (including primitive operations and constructors like `I#`) have a strict demand on their unlifted argument. But it turned out to be easier to enforce this in the demand analyser: So even if `f` claims to have a lazy demand on a argument of unlifted type, we make this demand strict before feeding it into the argument.
- **It is no longer safe to remove variables from the demand environment**, if the demand on them is strict. There must be so many bugs lurking around...


Unfortunate example:

```wiki
g :: Int -> Int
g 1 = 0
g x = x
```


What should its signature be? We want `<S>t`, but we get `<S>`. Why? Because the branches have signatures `t` and `{x ↦ S} t`. So their lub is going to be `{x ↦ L}`. In a later step, the demand on `x` will be strict again, but that is not easily visible here.


Can we avoid this? Not if we want to keep using the strictness demand as the hypothesis for the termination information. The alternative would be to to track the demand required separately from strictness. Then the lub of `t` and `{x ↦ required} t` would be `{x ↦ required} t`, and this case would work fine. That would turn it into a completely separate analysis, it seems.

### join points


CPR can kill join points.

#### Common Context


Idea to fix this, and possibly more general benefits:
[ http://www.haskell.org/pipermail/ghc-devs/2013-December/003481.html](http://www.haskell.org/pipermail/ghc-devs/2013-December/003481.html); prototype in branch `wip/common-context`.

- On its own, improvements are present but very small: [ http://www.haskell.org/pipermail/ghc-devs/2013-December/003500.html](http://www.haskell.org/pipermail/ghc-devs/2013-December/003500.html)
- Enabling CPR for sum types in non-top-level-bindings (which is currently disabled due to worries abut lost join points) yields mixed results (min -3.8%, mean -0.0%, max 3.4%).
- Enabling sum types inside nested CPR: Also yields mixed, not very promising results (-6.9% / +0.0% / +11.3%).

#### Direct detection


Alternative: Detect join points during `dmdAnal` and make sure that their CPR info is not greater than that of the expression they are a join-point for. Would also fix [\#5075](https://gitlab.haskell.org//ghc/ghc/issues/5075), see [5075\#comment:19](https://gitlab.haskell.org//ghc/ghc/issues/5075) for benchmark numbers.

- On its own, no changes.
- Enabling CPR for sumtypes: (min -3.8%, mean -0.0%, max 1.7%) (slightly better than with Common Context)
- Enabling sum types inside nested CPR: TBD

### Side tracks

- Should `runSTRep` be inlined (see [ticket:1600\#comment:34](https://gitlab.haskell.org//ghc/ghc/issues/1600))?
- Can we use `Terminates` CPR information to eagerly evaluate thunks? Yes, and there is a small gain there: [\#8655](https://gitlab.haskell.org//ghc/ghc/issues/8655)

  - But why no allocation change? Understand this better!
  - Can we statically and/or dynamically count the number of thunks, and the number of CBV’ed thunks?
- Why is `cacheprof` not deterministic? (→ [\#8611](https://gitlab.haskell.org//ghc/ghc/issues/8611))
- What became of Simon’s better-ho-cardinality branch? See [better-ho-cardinality](nested-cpr/better-ho-cardinality).
- Try vtunes to get better numbers.
