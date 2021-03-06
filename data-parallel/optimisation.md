## Optimisation problems


>
>
> **This page is outdated.**
>
>


This is a summary of the current optimisation problems we know about.

1. General remarks


This is mostly about SpecConstr which is the central optimisation for
stream-based fusion. The stream stuff is based on seeds:

```wiki
data Step s a = Done | Skip s | Yield a s
data Stream a = forall s. Stream (s -> Step s a) s
```


Ultimately, every stream is consumed by some kind of fold, e.g.,

```wiki
foldlS :: (a -> b -> a) -> a -> Stream b -> a
foldlS f z (Stream step s) = go z s
  where
    go z s = case step s of
               Done       -> z
               Skip    s' -> go z s'
               Yield x s' -> go (f z x) s'
```


In every iteration we obtain a new seed and pass it to the next
iteration. In general, the seed is a combination of tuples, Eithers and
some primitive values (Ints, mostly). If we want fusion to be efficient,
we have to get rid of the constructors and just keep those parts of the
seed actually required. This is what SpecConstr does but, alas, not
always.

1. No SpecConstr for Bools


Consider the following two functions:

```wiki
foo :: Bool -> Int -> Int
foo True  0 = 0
foo True  n = foo True (n-1)
foo False n = foo True n

bar :: Either Int Int -> Int
bar (Left  0) = 0
bar (Left  n) = bar (Left (n-1))
bar (Right n) = bar (Left n)
```


SpecConstr rewrites bar to

```wiki
bar   x = I# ($wbar x)

$wbar (Left (I# 0)) = 0
$wbar (Left (I# n)) = $s$wbar (n -# 1)
$wbar (Right n)     = $s$wbar1 n

$s$wbar1 (I# 0)     = 0
$s$wbar1 (I# n)     = $s$wbar (n -# 1)

$s$wbar (I# 0)      = 0
$s$wbar (I# n)      = $s$wbar (n -# 1)
```


This is great (although I do wonder why $s$wbar1 hasn't been inlined).
However, for foo we get

```wiki
foo b (I# n) = I# ($wfoo b n)
$wfoo True  0 = 0
$wfoo True  n = $wfoo True (n -# 1)
$wfoo False n = $wfoo True n
```


Here, we have a conditional in every iteration which is definitely not
what we want! In particular, it prevents Bools (and, I guess,
enumerations in general) from ever being used in seeds.

1. No SpecConstr for mutually recursive functions


This one comes up a lot in surprising circumstances. Consider

```wiki
foo :: Maybe Int -> Int
foo Nothing  = 0
foo (Just 0) = foo Nothing
foo (Just n) = foo (Just (n-1))
```


This does not look mutually recursive, but of course it is immediately
transformed into

```wiki
lvl = foo Nothing
foo Nothing  = 0
foo (Just 0) = lvl
foo (Just n) = foo (Just (n-1))
```


which isn't getting optimised at all.

1. No fixed point analysis


I think we talked about this briefly at some point; in any case, this
turns out to be one of the biggest problems. Consider

```wiki
foo :: Maybe Int -> Int -> Int
foo   (Just m) 0 = 0
foo x@(Just m) n = foo x (n-m)
```


SpecConstr doesn't get rid of the Just because it assumes that x must be
boxed which is obviously not the case here. This actually happens with
seeds all the time because we often pass a part of the seed unchanged to
the next iteration. We would still like them to be unboxed as the boxed
version is never needed.


The following are some random ramblings about how this could perhaps be
fixed quickly. I have tried several things to help the compiler here.
The obvious:

```wiki
foo (Just m) n = foo (Just m) (n-m)
```


doesn't work because CSE spots this and turns it into the previous
version. The next thing I tried was a hack:

```wiki
foo (Just m) n = let {-# INLINE x #-}
                     x = Just m
                 in foo x (n-m)
```


This is rewritten to

```wiki
foo (Just m) n = foo (__inline_me (Just m)) (n-m)
```


and CSE doesn't look inside the `__inline_me`. Unfortunately, neither does
SpecConstr. After teaching SpecConstr (and the rule matcher) to ignore
Core notes (I believe this is what is wanted in general, although I'm
not really sure about SCCs) this actually worked (although m still
wasn't getting unboxed, more about this below).


Unfortunately, things are not so simple. First, inline notes tend to
disappear a lot so as a quick hack, I made CSE ignore everything under a
"nocse" core note. More problematic is the fact that we usually do not
know the actual type of a (part of a) seed; stream transformers augment
seeds but cannot look inside of them. So just to see if this is
feasible, I tried to write a generic rebox function, as in

```wiki
class Rebox a where
  rebox :: a -> a
instance Rebox Int where
  {-# INLINE rebox #-}
  rebox (I# i#) = I# ({-# CORE "nocse" #-} i#)
instance (Rebox a, Rebox b) => Rebox (a,b) where
  {-# INLINE rebox #-}
  rebox (x,y) = (rebox x, rebox y)
...
```


Then, we could have

```wiki
data Stream a = forall s. Rebox s => Stream ...
```


and use rebox on all parts of the seed which are passed unchanged to the
next iteration or, even better, on the entire seed in every consumer.
Note that all calls to rebox are inlined because the optimiser knows all
the types involved. We also want to stop the reboxing at certain points
in the tree. For instance, in 

```wiki
scanlS :: (a -> b -> a) -> a -> Stream b -> Stream a
```


the a is part of the seed but we do \*not\* want to rebox it in each
iteration. The class-based code above allows us to express this.


Anyway, this did work to some extent, but not really well. Still,
perhaps playing around with a rebox pragma might be a feasible idea if
it was better integrated into the compiler. In particular, the
strictness analyser could infer U instread of S for things under the
pragma and SpecConstr could look at unfoldings. I'm not at all sure
about this, though. I might try to implement it and play around with it
at some point (but not soon!).


A related problem is

```wiki
foo :: Int -> Int -> Int
foo 0 n = 0
foo m n = foo (m-n) n
```


Here, n isn't getting unboxed although it could be if m is not 0 in the
first iteration. Perhaps unrolling the loop once could help here.

1. Join points


Unfortunately, I have lost the example, but when trying out the reboxing
stuff I saw the following happen quite a lot. I can try to find an
example if you want.


Suppose we have (\*after\* worker/wrapper, i.e., the join point must be
introduced by the subsequent simplifier run) something like

```wiki
foo p = let join (x,y) = ... foo (x,y) ...
        in
        case ... of
          ... -> join (x1,y1)
          ... -> join (x2,y2)
```


SpecConstr would turn this into

```wiki
foo' x y = let join (x,y) = ... foo x y ...
           in
           case ... of
             ... -> join (x1,y1)
             ... -> join (x2,y2)
```


This doesn't buy us a lot - we still construct and immediately
deconstruct a pair in each iteration. This is a fairly obscure case,
though.

1. Inlining vs. rewriting


One thing I've noticed when trying out stream fusion for lists is the
following. If we define

```wiki
map f = unstream . mapS f . stream
```


we \*always\* want to inline map (especially if we are going to rewrite it
back if fusion doesn't happen). But if stream is lazy (which it is), in

```wiki
foo f g = map f . map g
```


the two map won't be inlined, I assume because the simplifier sees no
reason to do it. I remember that you made the simplifier keener to
inline in contexts in which rules might apply but this doesn't work here
because map doesn't have a rewrite rule - it's the stream/unstream rule
we want to fire.


So just saying {-\# INLINE map \#-} doesn't help us. So what do we do? One
solution is to have an additional rewrite rule:

```wiki
  map f = unstream . mapS f . stream
```


But this results in a lot of code duplication, especially for functions
which aren't quite as small (i.e., stream transformers such as mapS
which we always want to inline). Neither does just having the rule and
defining map as

```wiki
map = undefined
```


help since GHC just treats map as a diverging function.


This problem is actually even worse with stream transformers which we
only want to inline quite late in the game as information about their
strictness, arity and so on should still be available in the earlier
phases. This means that we want GHC to see their definitions and then
inline them unconditionally at some point.


It would be quite handy to have a pragma which means "inline
unconditionally", for instance REWRITE. It would support the same phase
annotations etc. but behave as if instead of

```wiki
{-# REWRITE [n] foo #-}
foo x = e

we wrote

{-#  NOINLINE [n] foo #-}
foo x = e

{-# RULES
foo = \x -> e
#-}
```


It doesn't have to be implemented via rules, of course. I briefly talked
to Don about this and he said that he'd like to have this as well for
the ByteString library.


One question here is: do we want the above rule or

```wiki
foo = __inline_me(\x -> e)
```


In general, I have to say that the quality of the loops produced by
fusion depends on the optimiser doing a \*really\* good job which is quite
fragile. I've been thinking about how to help the compiler even more but
don't have anything worth talking about yet. 
