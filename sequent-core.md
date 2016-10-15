## Done so far

- Original paper: [ Sequent calculus as an intermediate language](https://www.microsoft.com/en-us/research/publication/sequent-calculus-as-a-compiler-intermediate-language/)

- Repo:  `git://github.com/lukemaurer/ghc`, branch `wip/join-points`

- New variant of Core.

  - IdDetails has a constructor for JoinPointId, with its arity.
  - Join points can be recursive
  - Lint checks many invariants about join points
  - All transformations are "join-point aware"; that is, they maintain join-point-hood.
  - Nullary join points do not take a void argument (as they did before).
  - Join-point Ids survive in Iface unfoldings

## Join Point Analysis (JPA)


Join Point Analysis (JPA), implemented in `CoreJoins.findJoinsInPgm`, is a new analysis that identifies join points, and marks them as such.


Currently we run JPA fairly frequently in the pipeline.  In due course this could become part of the occurrence analyser, because (a) we'd discover join points quickly, (b) it's doing much the same kind of thing (analysing occurrences).


Currently JPA has two passes, so that it can propagate the binding sites to occurrences.  Not necessary to do this if the simplifier runs immediately afterwards (it propagates).

## Transformations


These places need to be made join-point aware

- Worker/wrapper for strictness: we do want w/w for arguments, but not for the return side (CPR).

>
> We can't do CPR because (in the recursive case) the worker calls the wrapper, so it needs to be a join point, but a CPR wrapper always invokes the worker from a `case` expression, so it can't be a join point. Fortunately, CPR is rarely necessary for join points because they benefit from the CPR on their context:
>
> ```wiki
>   f z = let join j x y = (x+1, y+1)
>         in case z of A -> j 1 2
>                      B -> j 2 3
> ```
>
>
> Performing CPR on `f` gives us
>
> ```wiki
>     f z = case $wf z of (# a, b #) -> (a, b)
>   $wf z = case (let join j x y = (x+1, y+1)
>                 in case z of A -> j 1 2
>                              B -> j 2 3) of (a, b) -> (# a, b #)
> ```
>
>
> and now the simplifier will push the outer `case` into the join point:
>
> ```wiki
>   f z = case $wf z of (# a, b #) -> (a, b)
>   $wf z = let join j x y = (# x+1, y+1 #)
>           in case z of A -> j 1 2
>                        B -> j 2 3
> ```
>
>
> (But what if the join point has the CPR property but the outer function doesn't? Seems like we're still ahead because original GHC would've ruined the join point.)

- Float In is crucial for finding join points, especially recursive ones. Consider:

  ```wiki
  f1 x = let j y = ... j z ... in
         case j x of A -> ...
                     B -> ...
  ```

  If neither branch mentions `j`, then `j`*could* be a join point if we moved it inward a bit:

  ```wiki
  f2 x = case (let join j y = ... j z ... in
               j x) of A -> ...
                       B -> ...
  ```

  Now the call to `j z` is in tail position with respect to `j`'s definition, so `j` can be a join point. However, the existing Float In pass goes a bit too far:

  ```wiki
  f3 x = case (let j y = ... j z ... in
               j) x of A -> ...
                       B -> ...
  ```

  This is *equivalent* to the second version, but it doesn't follow the join point invariant.

>
> This is a funny habit of the Float In implementation: it often floats a `let`-bound function inward so far that the body of the `let` becomes just the identifier itself. Normally the simplifier fixes this right up, so it hasn't ever mattered, but the simplifier will just move the `let` all the way out again, turning `f3` back into `f1`. We need Float In to get it exactly right, since handling `case`-of-recursive-join is exactly what lets us do fusion with recursion.

- Float-out.

  - First approximation: don't float join points at all.
  - Nullary join points, if floated, cease to be join points but instead become shared thunks.  On balance this is a win.
  - Floating to top level.  Doesn't make much difference either way.  BUT we lose the ability to move case context into the join point. eg

    ```wiki
    f x = let j y = blah in
          case x of
            True  -> j 1
            False -> j 2
    ```

    Now if we inline `f` into a case scrutinee, the case will move into `blah`.  BUT if we float `j` to top level.  So you might think that floating to top level was harmful. But consider (non-recursive case):

    - If `blah` is big, `f` will not inline, so we will never wrap its RHS in a case.
    - If `blah` is small enough for `f` to inline, then a fortiori `j` will inline too.
      Moreover, floating to top level makes f more likely to inline.  Example:

      ```wiki
      f x y = let j v = blah(strict in v) in
              case x of
                A -> j y
                B -> j y
                C -> x
      ```

      Here `f` is strict in `x` but not `y`.  If we float the joint point to top level, `f` can inline, which exposes the strictness in `y`.

> >
> > If `j` is recursive, the above argument doesn't apply; not floating a small join point would be good, so that f can inline with it intact.

- Simplifier, obviously.  Moving the context into the RHS of join points.  Never float a join point at all.

- Rule matcher does some let-floating of its own.

  ```wiki
  RULE   f (g x) = x+1

       ...(f (let v = e in g (v-2)))....
  ==> (rule fires)
       ...(let v = e in
           let x = v-2 in
           x+1)...
  ```

  Be careful not to do this for join points, since you can't float a join point out of an argument.

- NB: Float-in is a transformation that often creates join points:

  ```wiki
       let f x = ...f x'... in
       case f t of alts
  ==>
       case (let f x = ...f x'... in f t) of alts
            -- Now f is a join point!
  ```

  NB: the very next run of the simplifier will float that `let`-binding for `f` out of the `case` scrutinee.  So it's important to look for join points before running the simplifier again.  Thus: (float-in; then find-join-points; then simplify)

- Added Late Lambda Lift.  But still work to do here.

## Cases where we win


Add `testsuite/test/perf/join-points/`

- For each place where you had to work to retain join points, make an example in which GHC currently destroys one, and behaves badly as a result.  Plus some examples like Section 4.3 in the paper.

- The new simplifier opens new possibilities for fusion, especially in the `unfold`/`destroy` tradition (which we can think of as stream fusion without the `Skip` constructor).

  ```wiki
  data Stream a = forall s. Stream (s -> Step a s) s
  data Step a s = Done | Yield a s

  {-# INLINE stream #-}
  stream :: [a] -> Stream a
  stream xs = Stream snext xs where
    snext []     = Done
    snext (x:xs) = Yield x xs

  {-# INLINE unstream #-}
  unstream :: Stream a -> [a]
  unstream (Stream next s) = go s where
    go s = case next s of
             Done       -> []
             Yield x s' -> x : go s'

  {-# INLINE sfilter #-}
  sfilter :: (a -> Bool) -> Stream a -> Stream a
  sfilter p (Stream next s) = Stream fnext s
    where
      fnext s = 
        let fgo s = case next s of
              Done -> Done
              Yield x s' | p x       -> Yield x s'
                         | otherwise -> fgo s'
         in fgo s

  filter :: (a -> Bool) -> [a] -> [a]
  filter = unstream . sfilter . stream
  ```

  Getting good performance out of fusion depends on getting rid of the `Done` and `Yield` constructors, which are never intended to create long-lived data, only to direct control flow. Hence any allocation is a waste.
  So how does `filter` get compiled? Inlining the three constituents `unstream`, `sfilter`, and `stream` gets us here (after a bit of floating):

  ```wiki
  filter xs =
    let snext []     = Done
        snext (x:xs) = Yield x xs

        fnext s = 
          let <join> fgo s = case snext s of
                Done -> Done
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> fgo s'
           in fgo s

        go s = case fnext s of
                 Done       -> []
                 Yield x s' -> x : go s'
    in go xs
  ```

  Note that `fgo` is flagged as a join point; this will be crucial! Now, `snext` is non-recursive, so it inlines happily enough into fnext:

  ```wiki
    ...
    let fnext s = 
          let <join> fgo s = case (case s of []     -> Done
                                             x : xs -> Yield x xs) of
                Done -> Done
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> fgo s'
           in fgo s
    ...
  ```

  And the usual case-of-case transform does its magic:

  ```wiki
    ...
    let fnext s = 
          let <join> fgo s = case s of
                [] -> Done
                x : s' | p x       -> Yield x s'
                       | otherwise -> fgo s'
           in fgo s
    ...
  ```

  But can we do the same when we inline `fnext` into `go`?

  ```wiki
  filter p xs =
    let go s = case (let <join> fgo s = case s of
                           [] -> Done
                           x : s' | p x       -> Yield x s'
                                  | otherwise -> fgo s'
                     in fgo s) of
                 Done       -> []
                 Yield x s' -> x : go s'
    in go xs
  ```

  The original simplifier gets stuck here; `fgo` will get floated out, but the `Done` and `Yield` constructors will remain. Since `fgo` is a join point, however, the new simplifier will instead pull its context *in*:

  ```wiki
  filter p xs =
    let go s = let <join> fgo s =
                     case (case s of
                             [] -> Done
                             x : s' | p x       -> Yield x s'
                                    | otherwise -> fgo s') of -- (*)
                       Done       -> []
                       Yield x s' -> x : go s'
               in fgo s
    in go xs
  ```

  And now case-of-case does the rest:

  ```wiki
  filter p xs =
    let go s = let <join> fgo s =
                     case s of
                       [] -> []
                       x : s' | p x       -> x : go s'
                              | otherwise -> fgo s'
               in fgo s
    in go xs
  ```

  We are left with the `filter` function as it would be hand-written.

>
> The alert reader may have an objection: On the starred line above, there is a jump (a call to a join point) that is not in tail position (with respect to the join point). Indeed, that example would not pass Core Lint! However, that snippet is a bit of a fiction; the Core AST never takes that form. Rather, the Core code shown here represents the state of the simplifier as it carries the `case` continuation inward. Whenever the simplifier comes to a jump, it throws away the continuation, thus maintaining the invariant that the jump is a tail call.

>
> (For the reader familiar with continuation-passing style or the sequent calculus: The case-of-case transform is simply an administrative reduction, substituting a continuation for free occurrences of a continuation variable. A join point (itself actually just a continuation) usually contains free occurrences of a continuation variable, whereas a jump does not. Hence the above behavior, pushing the context into a join point but leaving it off of a jump.)

- NB: that sometimes `go` functions do not start life as join points; we could also write `sfilter` above simply as

  ```wiki
  sfilter p (Stream next s)
    = Stream fnext s
    where
      fnext s = case next s of Done                   -> Done
                               Yield x s' | p x       -> Yield x s'
                                          | otherwise -> fnext s'
  ```

>
> Here `fnext` is not a join point, because it is not called in a saturated way.   But when we inline `sfilter`, it becomes one.  We must spot this pronto before we destroy it.  (A reason for doing JPA in the occurrence analyser.)

- The original Float Out is quite hazardous to join points. Since a join point is never allocated as a closure, floating it out doesn't improve sharing, and in most cases it can't be a join point anymore, so floating only *increases* allocations. (As always, there may be second-order effects, however; for instance, floating outward may leave behind a function that's small enough to inline.)

  ```wiki
  f x =
    let g y =
      let <join> j z = ... x ...
      in case p y of A -> j 1
                     B -> j 2
    
    in ...
    
    =>

  f x =
    let j z = ... x ... -- ruined!
        g y = case p y of A -> j 1
                          B -> j 2
    in ...
  ```

>
> We do still want to float out join points, however, just not too far. A good example occurs during unfold/destroy fusion, where a chain of filters becomes a series of trivially nested "loops":
>
> ```wiki
> filter odd . filter (> 4)
>
>   =>
>
> \xs ->
>   let next xs0 =
>     let <join> go1 xs1 =
>       let <join> go2 xs2 =
>         case xs2 of []    -> []
>                     x:xs' -> case x > 4 of False -> go2 xs'
>                                            True  -> case odd x of False -> go1 xs'
>                                                                   True  -> x : next xs'
>       in go2 xs1
>     in go1 xs0
>   in next xs
> ```
>
>
> Here we consider two filters, but this works to arbitrary depth. Since `go1` does nothing but invoke `go2`, it is just a needless bit of indirection. Float Out de-nests the loops:
>
> ```wiki
> \xs ->
>   let next xs0 =
>     let <join>
>       go1 xs1 = go2 xs2
>       go2 xs2 =
>         case xs2 of []    -> []
>                     x:xs' -> case x > 4 of False -> go2 xs'
>                                            True  -> case odd x of False -> go1 xs'
>                                                                   True  -> x : next xs'
>     in go1 xs0
>   in next xs
> ```
>
>
> And now `go1` can be inlined, completing the flattening process.

## Alternatives

- Rather than enforce a new invariant, we could give a semantics to non-tail calls to join points by seeing them as *abortive continuations*, as seen in Scheme. This would wreak havoc on Core, however! Calling a continuation would constitute a side effect; in fact, one could write `call/cc` quite easily:

  ```wiki
  callcc f = let<join> k x = x in f (\y -> k y)
  ```

  Keeping the join point invariant effectively restricts us to those programs for which jumps and function calls act in precisely the same way, thus making most of the Core-to-Core (and Core-to-STG) machinery blissfully unaware of the new construct.

## Still to do

- Try not propagating join points to occurrences in `findJoinsInPgm`; instead rely on simplifier.

- Desguarer should not add Void args to nullary join points.

- Dump the CoreToStg join point analysis in favour of the known join-points.

  - Check: does the CoreToStg analysis miss any JoinPointIds
  - Question: since STG is untyped, could it find more joint points that JPA does?)

- Currently CorePrep adds a void arg for a nullary join point.  Check: why?  What goes wrong if we don't do this?

- Idea: heap check for join point done at call site, not in join point itself. (Does not work for recursive join oints.)

- `CoreUnfold.sizeExpr`: SPJ claims: we should charge nothing for a join point binding or for its lambdas, or for its call.  (Reason: a join-point binding generates no allocation.)  Luke thinks that this was catastrophic in at least one case.  Investigate.

- Do Late Lambda Lifting (followed by simplify) *after*`CoreTidy`.

  - Then post LLL unfoldings won't affect downstream modules
  - But newly-small functions can still be inlined
  - Absolutely requires Arity/CAF info to be fed back from `CoreToStg`

- Join points are always fully eta-expanded, even when they would be trivial otherwise. This greatly simplifies many traversals, since typically the first step in processing a join point of arity N is to grab exactly N lambdas. The problem is that `exprIsTrivial` then returns `False`. This is particularly bad in `preInlineUnconditionally`, so there we check if a join point is eta-reducible to a trivial expression. But it's an ugly workaround, and there are other issues as well (sometimes trivial join points become loop breakers, for instance). Better would be to relax the invariant to allow trivial join points to elide lambdas, then provide a convenience function to eta-expand when needed (not hard to do for a trivial expression!).