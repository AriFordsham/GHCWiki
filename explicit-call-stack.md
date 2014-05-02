# Locating the cause of a crash


There has been a vigorous thread on error attribution ("I get a `head []` error; but who called `head`?").  This page summarises various approaches.


Relevant tickets: [\#960](https://gitlab.haskell.org//ghc/ghc/issues/960), [\#1441](https://gitlab.haskell.org//ghc/ghc/issues/1441), [\#9049](https://gitlab.haskell.org//ghc/ghc/issues/9049).

## Approaches


Here are the approaches we have under consideration

- **(PROF) Use profiling**.  Almost all the same issues arise in answering the question "where should I attribute this allocation cost?" as arise when answering "where did this error arise"?  If you

  - Compile with profiling
  - Run with `+RTS -xc`

>
> then any crash (call to `error`) will yield an informative backtrace.  The backtrace gives you a stack of calls that looks very like what you'd get in a call-by-value language.  (Lots of papers about profiling in a lazy language, dating right back to [ Formally based profiling for higher order functional languages](http://research.microsoft.com/~simonpj/papers/1997_profiling_TOPLAS.ps.gz) give the background.)

>
> You need to compile your whole program with profiling, and you need a profiled version of the packages you install. You can do the latter by adding `--enable-pofiling` to Cabal, and you can put that in your `~/.cabal` file.

>
> Programs run slower, of course, but that may not matter when debugging.  But a crash in a production system will generate no useful information.

- **(DYN) Walk the call stack, generating a dynamic backtrace.**  This is what every other language does; it works in a production system (ie all debug flags off); and it carries zero runtime overhead unless a crash actually happens.

>
> One difficulty is that the backtrace is unintuitive, because of lazy evaluation, but it is still massively better than nothing.  Another difficulty is that GHC shakes the program around during optimisation, so it is hard to say what code comes from where.

>
> Addressing these challenges is the subject of Peter Wortman's PhD.  He has a paper [ Causality of Optimized Haskell: What is burning our cycles?](http://eprints.whiterose.ac.uk/77401/), and an implementation is well advanced (not quite yet in HEAD, May 2014).

- **(NEEDLE) Finding the needle**.  This is a cross between (PROF) and (DYN).  It transforms the program, but in a less invasive way than for full profiling.  Lots more details on [ExplicitCallStack/FindingTheNeedle](explicit-call-stack/finding-the-needle).  We don't currently plan to implement this in HEAD: it is not clear that, given (PROF) and (DYN), it's worth a third path, and one that is non-trivial to implement (as you'll see from the paper).

- **(IMPLICIT)** Implicit locations.  Currently (May 2014) I'm just floating this idea: [ExplicitCallStack/ImplicitLocations](explicit-call-stack/implicit-locations).  Its main merit is its extreme simplicity.

## Other relevant writings

- [ Simon Hengel's "rewrite with location" proposal](https://github.com/sol/rewrite-with-location), see also [ Michael Snoyman's post](http://www.haskell.org/pipermail/haskell-cafe/2013-February/106617.html)
- [ Lennart Augustsson's post](http://augustss.blogspot.se/2014/04/haskell-error-reporting-with-locations_5.html)
- [ The Haskell cafe thread](http://www.haskell.org/pipermail/haskell-cafe/2006-November/019549.html)
- [ http://www.cse.unsw.edu.au/\~dons/loch.html](http://www.cse.unsw.edu.au/~dons/loch.html)
- [ HAT](http://haskell.org/hat)