## Fixing demand analysis for exceptions


There are a couple different problems we have to deal with.

1. [\#13330](https://gitlab.haskell.org//ghc/ghc/issues/13330) was caused by an ugly and somewhat broken hack trying to analyze `catch#` as stricter than it really is. It would be very nice if the *good ideas* that went into that ugly hack could be extracted and repaired to produce a more aggressive analysis that's still correct.

1. [\#13380](https://gitlab.haskell.org//ghc/ghc/issues/13380) reveals something of a disagreement about how we should view the result of `raiseIO#` (used to implement `throwIO`). Simon Marlow and David Feuer feel pretty strongly that `throwIO` should be viewed as producing an entirely deterministic, well-behaved `IO` action, and that the exception resulting from it should never be mixed up with an imprecise exception. Reid Barton and Simon Peyton Jones seem to wonder if that precision is worth the potential performance cost.


Assuming that I (David F.) and Simon M. win this debate, the key problem here is that we analyze `raiseIO# e s` as `ThrowsExn`, the same way we analyze something that either diverges or throws an imprecise exception. Assuming we change this, we want to take some care to recover dead code elimination that the current analysis allows. In particular, given

```
case raiseIO# e s of(# s', a #)->EXPR
```


we surely want to consider `EXPR` to be dead code, even though we don't want to consider `raiseIO# e s` to be precisely bottom.

### Important conventions below


In the rest of this page, I will squash all `Exception` types down to `SomeException`, to avoid all the conversion mess. So instead of `Exception e => ...`, I will simply assume that `e` is `SomeException`.


Furthermore, for the sake of readability, I uniformly substitute `Either a b` in place of `(# a | b #)`.


By a **precise** exception, I mean an exception produced by `raiseIO#` (the primop version of `throwIO`).


By an **imprecise** exception, I basically mean an exception produced by `throw` (as described in [ A Semantics for Imprecise Exceptions](https://www.microsoft.com/en-us/research/publication/a-semantics-for-imprecise-exceptions/)).

### Semantics of precise exceptions


I (David Feuer) believe that precise exceptions should implement the following model.

```
newtypeIO a =IO{unIO ::State#RealWorld->(#State#RealWorld,EitherSomeException a #)instanceMonadIOwhere
  return a =IO$\s ->(# s,Right a #)
  m >>= f =IO$\s ->case unIO m s of(# s',Left e #)->(# s',Left e #)(# s',Right a #)-> unIO (f a) s'

throwIO::SomeException->IO a
throwIO e =IO$\s ->(# s,Left e #)-- The name 'catchIO' is, sadly, taken by a less interesting function alreadycatchThrowIO::IO a ->(SomeException->IO a)->IO a
catchThrowIO m f =IO$\s ->case unIO m s of(# s',Left e #)-> unIO (f e) s'
    good -> good
```


Side note: I believe we likely should expose an actual *catchThrowIO* function. Since it doesn't catch imprecise exceptions, it can be treated much more aggressively. For example, `catchThrowIO (putStrLn x) (\_ -> print 2)` can safely be analyzed as strict in `x`, whereas the equivalent expression using `catch` cannot.


To achieve something like this, we need to fix [\#13380](https://gitlab.haskell.org//ghc/ghc/issues/13380).

### `catch#` strictness


How strict can `catch# m f s` be? We know several things:

1. If `m s` diverges (without throwing an exception), then `catch# m f s` diverges.

1. If `m s` certainly executes successfully, then `catch# m f s = m s`.

1. If `m s` is strict in some value `x`, and `x` certainly does not throw an exception (i.e., it either evaluates successfully to WHNF or diverges), then it is safe to consider `catch# m f s` strict in `x`.

1. If `m s` and `f e s` are both strict in some value `x`, then it is safe to consider `catch# m f s` strict in `x`.

1. If `m s` certainly throws an exception (either imprecise or precise) that it does not itself catch, then `f` is certainly called.

### `catchRetry#`

`catchRetry#` is used to implement `orElse` for `STM`. I *believe* that it functions (from the perspective of demand analysis) very much like the hypothetical `catchThrowIO`, and that we can probably treat them similarly.

### Open tickets relating to exceptions

<table><tr><th>[\#7411](https://gitlab.haskell.org//ghc/ghc/issues/7411)</th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th>[\#11829](https://gitlab.haskell.org//ghc/ghc/issues/11829)</th>
<td>C++ does not catch exceptions when used with Haskell-main and linked by ghc</td></tr>
<tr><th>[\#12096](https://gitlab.haskell.org//ghc/ghc/issues/12096)</th>
<td>Attach stacktrace information to SomeException</td></tr>
<tr><th>[\#12696](https://gitlab.haskell.org//ghc/ghc/issues/12696)</th>
<td>Exception gives not enough information to be useful</td></tr>
<tr><th>[\#13357](https://gitlab.haskell.org//ghc/ghc/issues/13357)</th>
<td>Check demand signatures for catchRetry\# and catchSTM\#</td></tr>
<tr><th>[\#13370](https://gitlab.haskell.org//ghc/ghc/issues/13370)</th>
<td>exprIsBottom inconsistent with strictness analyser</td></tr>
<tr><th>[\#13380](https://gitlab.haskell.org//ghc/ghc/issues/13380)</th>
<td>raiseIO\# result looks wrong</td></tr>
<tr><th>[\#14998](https://gitlab.haskell.org//ghc/ghc/issues/14998)</th>
<td>Sort out the strictness mess for exceptions</td></tr></table>

### Closed tickets relating to exceptions

<table><tr><th>[\#942](https://gitlab.haskell.org//ghc/ghc/issues/942)</th>
<td>Windows programs throw uncaught Invalid HANDLE exception on exit</td></tr>
<tr><th>[\#1905](https://gitlab.haskell.org//ghc/ghc/issues/1905)</th>
<td>runProcess: misbehaving exception on nonexistent working directory</td></tr>
<tr><th>[\#2211](https://gitlab.haskell.org//ghc/ghc/issues/2211)</th>
<td>Installing latest GHC-6.8.2 stable: pwd with floating point exception</td></tr>
<tr><th>[\#3983](https://gitlab.haskell.org//ghc/ghc/issues/3983)</th>
<td>-O2 makes exception disappear</td></tr>
<tr><th>[\#4021](https://gitlab.haskell.org//ghc/ghc/issues/4021)</th>
<td>Problem of Interaction Between the FreeBSD Kernel and the GHC RTS</td></tr>
<tr><th>[\#4343](https://gitlab.haskell.org//ghc/ghc/issues/4343)</th>
<td>Add throwSTM and generalize catchSTM</td></tr>
<tr><th>[\#5611](https://gitlab.haskell.org//ghc/ghc/issues/5611)</th>
<td>Asynchronous exception discarded after safe FFI call</td></tr>
<tr><th>[\#5626](https://gitlab.haskell.org//ghc/ghc/issues/5626)</th>
<td>Miscompilation, exception omitted with -O</td></tr>
<tr><th>[\#10435](https://gitlab.haskell.org//ghc/ghc/issues/10435)</th>
<td>catastrophic exception-handling disablement on Windows Server 2008 R2</td></tr>
<tr><th>[\#10712](https://gitlab.haskell.org//ghc/ghc/issues/10712)</th>
<td>Regression: make TEST=exceptionsrun001 WAY=optasm is failing</td></tr>
<tr><th>[\#11222](https://gitlab.haskell.org//ghc/ghc/issues/11222)</th>
<td>Teach strictness analysis about \`catch\`-like operations</td></tr>
<tr><th>[\#11555](https://gitlab.haskell.org//ghc/ghc/issues/11555)</th>
<td>catch _\|_ breaks at -O1</td></tr>
<tr><th>[\#13330](https://gitlab.haskell.org//ghc/ghc/issues/13330)</th>
<td>forkIO has inconsistent behavior under optimization</td></tr>
<tr><th>[\#13348](https://gitlab.haskell.org//ghc/ghc/issues/13348)</th>
<td>Consider making throw and throwIO strict</td></tr>
<tr><th>[\#13977](https://gitlab.haskell.org//ghc/ghc/issues/13977)</th>
<td>ExnStr doesn't propagate "outwards"</td></tr>
<tr><th>[\#15226](https://gitlab.haskell.org//ghc/ghc/issues/15226)</th>
<td>GHC doesn't know that seq\# produces something in WHNF</td></tr></table>

### GHC Commentary relating to exceptions

- [Commentary/CmmExceptions](commentary/cmm-exceptions)
- [Commentary/Rts/AsyncExceptions](commentary/rts/async-exceptions)
- [Exceptions/StackTraces](exceptions/stack-traces)