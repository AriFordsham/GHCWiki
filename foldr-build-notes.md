
This page is intended for practical notes on why list functions and rules are written as they are, why they're not written other ways, ideas about what will/won't fuse properly and why, and descriptions of issues affecting fusion. It will also have open questions of various sorts.


Q: Why are functions written back to other forms when they don't fuse?


A1: Sometimes, the fancy fusing version is somehow worse than the simpler one if fusion doesn't happen.


A2: Functions must be inlined in order to fuse. When fusion doesn't happen, this creates duplicate code, often with no benefit. In fact, it may create multiple copies of the same function at the top level.


Q: Why are functions written back to recursive forms when they don't fuse, rather than pretty ones?


Guess: by the time the writing-back happens, it may be too late in simplification for the compiler to optimize the pretty form properly. However, ti may be possible to get around this sometimes by using a NOINLINE form rather than an INLINE one. If you write back to a NOINLINE form, I would guess that you should get an already-optimized version, whereas if you write back to an INLINE form, you will get that form as it is, too late to optimize. So there may be some room to improve this in some cases.


Q: Why isn't map written to inline when given one argument like foldr is written to inline when given two?


Q: Why does `repeat` fuse, but not `cycle`?


A: See [\#9398](https://gitlab.haskell.org//ghc/ghc/issues/9398). This seems almost to work out, but then it doesn't quite—things that are unboxed when the current implementation is used aren't, and it can be very bad.


Q: Why isn't `scanr` a good producer?


It's actually possible to rewrite `scanr` using `build` and `foldr`, but there's a wrinkle: the way it's currently written, `scanr` can *inspect* the list as it creates it. This becomes impossible one `build` is in play. To work around that, you can hang on to a second copy of the head of the list. But then you need somewhere to \*put\* that. Whoops, that's allocation.


Q: Why does making one thing fuse sometimes make something else not fuse?


A: Because the whole system is built around inlining, and no one really knows how to make that Do The Right Thing every time. Also, no one knows a better way to avoid basing it on inlining.
