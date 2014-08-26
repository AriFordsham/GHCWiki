
This page is intended for practical notes on why list functions and rules are written as they are, why they're not written other ways, ideas about what will/won't fuse properly and why, and descriptions of issues affecting fusion. It will also have open questions of various sorts.


Q: Why are functions written back to other forms when they don't fuse?


A1: Sometimes, the fancy fusing version is somehow worse than the simpler one if fusion doesn't happen.


A2: Functions must be inlined in order to fuse. When fusion doesn't happen, this creates duplicate code, often with no benefit. In fact, it may create multiple copies of the same function at the top level.


Q: Why are functions written back to recursive forms when they don't fuse, rather than ones that use higher-order functions?


Guess: by the time the writing-back happens, it may be too late in simplification for the compiler to optimize the form using higher-order functions properly. However, ti may be possible to get around this sometimes by using a NOINLINE form rather than an INLINE one. If you write back to a NOINLINE form, I would guess that you should get an already-optimized version, whereas if you write back to an INLINE form, you will get that form as it is, too late to optimize. So there may be some room to improve this in some cases.


Q: Why are functions rewritten to forms using weird-looking functions like `mapFB`?


A: If the function doesn't fuse and it needs to be rewritten back to something like its original form, we need to have enough to match on. One of the more reliable ways to do this is to use some `NOINLINE` functions in the rewritten form that we can then match on.


Q: Why isn't `map` written to inline when given one argument like `foldr` is written to inline when given two?


Guess: it may be that there isn't enough opportunity for inlining to do anything useful in that case, since `map` isn't doing anything with the results of the calls except wrapping them up in conses.


Q: Why does `repeat` fuse, but not `cycle`?


A: See [\#9398](https://gitlab.haskell.org//ghc/ghc/issues/9398). This seems almost to work out, but then it doesn't quite—things that are unboxed when the current implementation is used aren't, and it can be very bad.


Q: Why isn't `scanr` a good producer?


It's actually possible to rewrite `scanr` using `build` and `foldr`, but there's a wrinkle: the way it's currently written, `scanr` can *inspect* the list as it creates it. This becomes impossible once `build` is in play. To work around that, you can hang on to a second copy of the head of the list. But then you need somewhere to \*put\* that. Whoops, that's allocation.


Q: Why does making one thing fuse sometimes make something else not fuse?


A: Because the whole system is built around inlining, and no one really knows how to make that Do The Right Thing every time. Also, no one knows a better way to avoid basing it on inlining.


Q: Now can full laziness interfere with fusion?


A: Full laziness can pull a piece of an expression up to the top level, away from its context. A `build` form that's been pulled to the top level currently will not be seen by the RULES engine when it's inspecting a `foldr` form containing its (automatically generated) name. The first (partial) full laziness pass happens before any inlining and before any specialization, and therefore before a great many fusion opportunities have been revealed. The specialization issue affects `enumFromTo` and related functions, while the inlining one causes general difficulty. One workaround for the latter is to use `RULES` to "manually" inline a function; this is what many of the "translate to" rules effectively do, but many things aren't covered. For example, `($)` and `(.)` aren't inlined before full laziness tries to rip expressions using them apart.
