# Exceptions in GHC


Relevant pages:

- [Exceptions/PreciseExceptions](exceptions/precise-exceptions)
- [FixingExceptions](fixing-exceptions)
- [Commentary/CmmExceptions](commentary/cmm-exceptions)
- [Commentary/Rts/AsyncExceptions](commentary/rts/async-exceptions)

## Tickets



Use Keyword = `Exceptions` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7411">#7411</a></th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11829">#11829</a></th>
<td>C++ does not catch exceptions when used with Haskell-main and linked by ghc</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12096">#12096</a></th>
<td>Attach stacktrace information to SomeException</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12696">#12696</a></th>
<td>Exception gives not enough information to be useful</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13357">#13357</a></th>
<td>Check demand signatures for catchRetry# and catchSTM#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13370">#13370</a></th>
<td>exprIsBottom inconsistent with strictness analyser</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13380">#13380</a></th>
<td>raiseIO# result looks wrong</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14998">#14998</a></th>
<td>Sort out the strictness mess for exceptions</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/942">#942</a></th>
<td>Windows programs throw uncaught Invalid HANDLE exception on exit</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1905">#1905</a></th>
<td>runProcess: misbehaving exception on nonexistent working directory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2211">#2211</a></th>
<td>Installing latest GHC-6.8.2 stable: pwd with floating point exception</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3983">#3983</a></th>
<td>-O2 makes exception disappear</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4021">#4021</a></th>
<td>Problem of Interaction Between the FreeBSD Kernel and the GHC RTS</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4343">#4343</a></th>
<td>Add throwSTM and generalize catchSTM</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5611">#5611</a></th>
<td>Asynchronous exception discarded after safe FFI call</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5626">#5626</a></th>
<td>Miscompilation, exception omitted with -O</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10435">#10435</a></th>
<td>catastrophic exception-handling disablement on Windows Server 2008 R2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10712">#10712</a></th>
<td>Regression: make TEST=exceptionsrun001 WAY=optasm is failing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11222">#11222</a></th>
<td>Teach strictness analysis about `catch`-like operations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11555">#11555</a></th>
<td>catch _|_ breaks at -O1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13330">#13330</a></th>
<td>forkIO has inconsistent behavior under optimization</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13348">#13348</a></th>
<td>Consider making throw and throwIO strict</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13977">#13977</a></th>
<td>ExnStr doesn&apos;t propagate &quot;outwards&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15226">#15226</a></th>
<td>GHC doesn&apos;t know that seq# produces something in WHNF</td></tr></table>



