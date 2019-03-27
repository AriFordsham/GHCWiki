# The Static Argument Transformation (SAT)


This page summarises progress on the Static Argument Transformation.


See:

- [Andre Sansos's thesis](https://www.microsoft.com/en-us/research/publication/compilation-transformation-non-strict-functional-languages/) which has a whole chapter.
- [Danvy's lambda-dropping paper](http://ojs.statsbiblioteket.dk/index.php/brics/article/view/18785)


In comment:10 of [\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059), Max notes that SAT provides 20-30% wins in nofib, wow! 

Also see [this comment in #9374](https://gitlab.haskell.org/ghc/ghc/issues/9374#note_187163) which shows some spectacular wins.

## Tickets


Use Keyword = `StaticArgumentTransformation` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#888](https://gitlab.haskell.org//ghc/ghc/issues/888)</th>
<td>Implement the static argument transformation</td></tr>
<tr><th>[\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059)</th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th>[\#9374](https://gitlab.haskell.org//ghc/ghc/issues/9374)</th>
<td>Investigate Static Argument Transformation</td></tr>
<tr><th>[\#13502](https://gitlab.haskell.org//ghc/ghc/issues/13502)</th>
<td>Static argument transformation should also run after specialisation</td></tr>
<tr><th>[\#13966](https://gitlab.haskell.org//ghc/ghc/issues/13966)</th>
<td>Skip-less stream fusion: a missed opportunity</td></tr>
<tr><th>[\#14211](https://gitlab.haskell.org//ghc/ghc/issues/14211)</th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th>[\#14231](https://gitlab.haskell.org//ghc/ghc/issues/14231)</th>
<td>Core lint error "in result of Static argument"</td></tr>
<tr><th>[\#14649](https://gitlab.haskell.org//ghc/ghc/issues/14649)</th>
<td>ghc panic: mergeSATInfo</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#9545](https://gitlab.haskell.org//ghc/ghc/issues/9545)</th>
<td>Evaluate Takano Akio's foldrW/buildW fusion framework as a possible replacement for foldr/build</td></tr></table>

# Matt's Notes


The primary value of performing `SAT` is to enable the function to be inlined and hence simplified. 


The decision about whether to run SAT is currently predicated on the number of static value arguments. If there is more than one static argument then the transformation is run, otherwise it is not. I am unsure why this decision has been made, there are useful situations where running the transformation with one argument (see `test2` below). I don't think it has been scrutinised much as 1. Not many people use this optimisation and 2. Usually there is more than one static argument. (For example, a dictionary and a function which uses that dictionary). 

# Interaction with specialisation


One very useful time to run SAT is after specialisation and [SpecConstr](spec-constr). Specialisation especially can create situations where static dictionaries are passed to functions using `RankNTypes` which are then not eliminated leading
to slow dictionary lookups. 


However, we also have the be careful as sometimes these passes produce recursive groups which are later broken up. SAT only works for recursive groups which contain exactly one definition due to the possibility of blow-up.

`test` and `test2` are only optimised well with an additional late pass of SAT. 

```wiki
{-# LANGUAGE RankNTypes, BangPatterns  #-}
module Standalone where

import qualified Control.Monad.State as S

times :: Monad m => Int -> m a -> m ()
times n ma = go n where
  go 0 = pure ()
  go n = ma >> go (n - 1)
{-# inline times #-}


newtype VSM s a = VSM { runVSM :: forall m. Monad m => m s -> (s -> m ()) -> m a}

instance Functor (VSM s) where
  fmap f (VSM g) = VSM $ \get put ->
    g get put >>= \a -> pure (f a)
  {-# inline fmap #-}

instance Applicative (VSM s) where
  pure a = VSM $ \get put -> pure a
  VSM mf <*> VSM ma = VSM $ \get put ->
    mf get put >>= \f ->
    ma get put >>= \a -> pure (f a)
  {-# inline pure #-}
  {-# inline (<*>) #-}

instance Monad (VSM s) where
  return a = VSM $ \get put -> pure a
  VSM ma >>= f = VSM $ \get put ->
    ma get put >>= \a -> runVSM (f a) get put
  {-# inline return #-}

vmmodify :: (s -> s) -> VSM s ()
vmmodify f = VSM $ \get put ->
  get >>= \s ->
  let !s' = f s in
  put s'
{-# inline vmmodify #-}

vmrunState :: VSM s a -> s -> (a, s)
vmrunState (VSM ma) = S.runState (ma S.get S.put)
{-# inline vmrunState #-}

test :: Int -> ((), Int)
test n = vmrunState (times n $ vmmodify (+1)) n


newtype CS s a = CS {runCS ::
     forall r.
     (a -> r)          -- pure
  -> ((s -> r) -> r)   -- get
  -> (s -> r -> r)     -- put
  -> r
  }

instance Functor (CS s) where
  fmap f (CS g) = CS $ \pure get put -> g (pure . f) get put
  {-# inline fmap #-}

instance Applicative (CS s) where
  pure a = CS $ \pure get put -> pure a
  {-# inline pure #-}
  CS mf <*> CS ma = CS $ \pure get put ->
    mf (\f -> ma (pure . f) get put) get put
  {-# inline (<*>) #-}

instance Monad (CS s) where
  return a = CS $ \pure get put -> pure a
  {-# inline return #-}
  CS ma >>= f = CS $ \pure get put ->
    ma (\a -> runCS (f a) pure get put) get put
  {-# inline (>>=) #-}
  CS ma >> CS mb = CS $ \pure get put -> ma (\_ -> mb pure get put) get put
  {-# inline (>>) #-}

cmodify :: (s -> s) -> CS s ()
cmodify f = CS $ \pure get put ->
  get $ \s -> let !s' = f s in
  put s' $
  pure ()
{-# inline cmodify #-}

crunState :: CS s a -> s -> (a, s)
crunState (CS f) = f
  (\a s -> (a, s))
  (\got s -> got s s)
  (\s' put s -> put s')
{-# inline crunState #-}

test2 :: Int -> ((), Int)
test2 n = crunState (times n (cmodify (+1))) 0
```

# Interaction with Join Points


In section 5 of "compiling without continuations" it is noted that join points enable fusion but in order to do so you have to write your programs in a particular style.

```wiki
module Join where

find :: (a -> Bool) -> [a] -> Maybe a
find p xs = go xs
  where
    go [] = Nothing
    go (x:xs) = if p x then Just x else go xs

any :: (a -> Bool) -> [a] -> Bool
any p xs  = case find p xs of
              Just _ -> True
              Nothing -> False
```


The core for `any` is then nicely fused as `go` is identified as a join point which means that it can be inlined even though `find` is recursive. 


This particular style is exactly the kind of code which the static argument transformation produces. Currently it is not enough to turn on `-fstatic-argument-transformation` as the order the transformations are run seems wrong.

# Nofib notes

- CSD yields a function `$wcstime` which takes 3 static dictionary arguments. Applying SAT to this function allows it to be inlined and a lot of further simplification. 
- anna - `Utils.sort.insert` gets satted as it has two static arguments, a dictionary and a non-function value. This has no positive impact as `insert` can't be inlined productively so on each iteration there is the additional cost of allocating a closure for the worker.
- atom - `runExperiment` is turned into a loop which as identified in the comment improves the performance.
- cichelli - Three functions in `Auxil.hs` are satted, `member`, `assoc`, `assocm`. The gain comes from allocations saved from `assocm` which is called twice in `findHash'`.
- event - `f_foldr` is unproductively satted, a static value which is not applied or consumed by a static argument.
- fibheaps - `ins` is satted because `a` and `Ord a` are static arguments. It is plausible that this could be a big win \*if\* when `ins` is applied to a specific `a` then `readArray` and `writeArray` could be simplified further. It turns out this  never happens so `ins` just ends up allocating more. I don't know how to avoid this. 
- genftt - `f_foldr` is satted but this time allocations go down..? In f_rvar and other functions which call `f_foldr` the arguments are nicely specialised away. If SAT is not run then f_foldr gets sced.
- gg - `group`, `insert``replace`, `remove` and `collect` are satted. Main effect is from `insert` which inserts an ordered value into a list and allows specialisation to an ord dictionary. I wonder whether the same would be achieved by -fspecialise-aggressively without also having to SAT the pointless value. Well no, as it is self-recursive but an `INLINABLE` pragma does the trick.
- last-piece - `fit` is satted and then allocates more. If it is not satted then it is spec-constred (which then fire 17 times). OTOH, fit is never even inlined as it is too big.
- mate - `pieceAtWith` increases allocations as no additional specialisation can occur but `sift` being inlined means `moveDetailsFor` becomes a very big function as alot is inlined.
- parstof - Hard to here here as the code is so strange but 4 plausible functions get satted.
- solid - Code similarly hard to read and understand looks like one of the satted functions gets inlined which makes it specialise better.
- insertT - Has two static value arguments `k` and `e`. Inlining to specialise `e` will not win anything. Inlining `k` has the potential to specialise the comparision but likely not to do a lot of simplification for `Int`. Could be better for a more complex type. None of these are statically known anyway so it will never be a win.

# David's Comments

>
> I think we really want to be able to perform value specialization of recursive functions without SAT. The performance trade-off of SAT when it comes to code generation doesn't look so hot in many cases. BTW, I noticed that the lack of SAT or argument specialization can prevent good optimization of derived code. For example, `data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Foldable` yields a very mediocre-looking `foldl'` that would be fixed by a `foldr` that could inline. It also produces a very boxing `length`, but that smells like a trickier higher-order demand issue. --dfeuer


Indeed, more inlining does happen if you turn on `-fstatic-argument-transformation` when deriving Foldable as it will allow the `foldMap` to be inlined and `f` to be specialised.

# SAT vs. SpecConstr

I (Sebastian) argue that SAT only is beneficial if the SAT'd argument ultimately leads to an increase of analysis information about use sites (like a known call or a redex). This is the same reasoning that drives `SpecConstr`, so it makes sense to compare the two (I'm not the first to realise this, ofc).

Example:

```
f x y z = ... case x of ... case y of ... f x y (z+1) ...

g a = ... f (Just a) True (a+3) ... f Nothing False (a-3) ...
```

## SAT

SAT will turn this into

```
f x y z = sat_worker z
  where
    sat_worker z = ... case x of ... case y of ... sat_worker (z+1) ...

g a = ... f (Just a) True (a+3) ... f Nothing False (a-3) ...
```

Note that operationally, this probably wasn't a beneficial transformation at all. It's basically inverse lambda lifting: We allocate an extra thunk for `sat_worker` that only really breaks even when the recursive call site is inside a thunk (see the concept of *closure growth* in [selective lambda lifting](https://pp.ipd.kit.edu/~sgraf/papers/sel-lam-lift-preprint.pdf)).

But `f` has a huge advantage: Inlining `f` means specialising `sat_worker` for that particular call site:

```
f x y z = sat_worker z
  where
    sat_worker z = ... case x of ... case y of ... sat_worker (z+1) ...

g a = ... sat_worker1 (a+3) ... sat_worker2 (a-3) ...
  where
    sat_worker1 z = ... case Just a of ... case True of ... sat_worker1 (z+1) ...
    sat_worker2 z = ... case Nothing of ... case False of ... sat_worker2 (z+1) ...
```

And the simplifier will perform case-of-known-constructor. But it's problematic when `f` doesn't inline! Seen this way, SAT is just a way to express that we want to specialise for a particular (static) argument. Note that now we have selective lambda lifting, we will probably revert some of the badness, by turning `f` into the following

```
$lsat_worker x y z = ... case x of ... case y of ... $lsat_worker x y (z+1) ...
f x y z = $lsat_worker x y z
```

Eww... Regardless, what counts is that prior Core passes specialised the function and that we exported an unfolding that has the SAT'd form. We hopefully never actually call this function. But SAT can't know, because it's a decision made by the inliner.

## SpecConstr

Call-pattern specialisation would go differently about specialising `f`, by first looking at its particular call sites and recording the call-patterns `[Just _, True, _]` and `[Nothing, False, _]`, leading to the following specialisations:

```
$sf1 a z = ... case Just a of ... case True of ... f (Just a) True (z+1) ...
$sf2 z = ... case Nothing of ... case False of ... f Nothing False (z+1) ...

{-# RULES "SC:$sf1" forall a z. f (Just a) True z = $sf1 a z #-}
{-# RULES "SC:$sf2" forall a z. f Nothing False z = $sf2 z #-}
f x y z = ... case x of ... case y of ... f x y (z+1) ...

g a = ... f (Just a) True (a+3) ... f Nothing False (a-3) ...
```

Note how `SpecConstr` defers resolution of the call sites to the RULES engine and how the non-specialisable parts `a` and `z` are passed as arguments instead of free variables. This allows the simplifier to *share* specialisations, somewhat remedying the code bloat potential (I'm unsure if it matters in practice).

This is the result after optimisations:

```
$sf1 a z = ... $sf1 a (z+1) ...
$sf2 z = ... $sf2 (z+1) ...

{-# RULES "SC:$sf1" forall a z. f (Just a) True z = $sf1 a z #-}
{-# RULES "SC:$sf2" forall a z. f Nothing False z = $sf2 z #-}
f x y z = ... case x of ... case y of ... f x y (z+1) ...

g a = ... $sf1 a (a+3) ... $sf2 (a-3) ...
```

No passing of static arguments in specialisations, either, and the original `f` is still in a lambda lifted form.

## Comparison

|        | SpecConstr | SAT |
| ------ | ---------- | --- |
| Reliability | Reliably specialises, but currently not for call patterns outside the defining module | Hinges on the inliner's decision to inline the wrapper function at call sites. Works flawlessly across module boundaries. |
| Code bloat | Always specialises when call pattern is met with argument occurrences, leading to multiple specialisations (polyvariance). Tries to share specialisations with RULES | The inliner decides if it is worthwhile to inline, thus specialising the SAT worker. The effect is one specialisation per inlined call site, regardless of whether the same argument pattern occurred somewhere else |
| Unspecialised fallback | The original function body is left untouched, only RULES for call sites | The original function remains SAT'd and non-inlined calls will have to allocate an unnecessary closure for the `sat_worker`. Although selective lambda lifting rectifies this in part. |
| Specialises for functions | No, sadly. Possibly in the future. | Yes |
| Specialises for non-static arguments | Yes | No |

## Ramblings

I actually think that the SAT'd form (as opposed to the lambda lifted form) is the better informed one, because the knowledge that the arguments are static is implicitly encoded in the syntax tree, so that more passes implicitly profit from that knowledge. Since we do a late selective lambda lifting pass, the non-beneficial cases (from the perspective of allocations) should be identified pretty reliably.

But SAT's greatest weakness is how it relies on the inliner to become actually beneficial: Knowing that some argument is static is useless when it's just bound as a parameter of an outer function (which is the case when the wrapper isn't inlined).