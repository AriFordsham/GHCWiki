# Float out and full laziness


This is a status page for full laziness, implemented by the float-out transformation.


The relevant modules in GHC are `SetLevels` and `FloatOut`.

## Tickets



Use Keyword = `FloatOut` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/917">#917</a></th>
<td>-O introduces space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7206">#7206</a></th>
<td>Implement cheap build</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7367">#7367</a></th>
<td>float-out causes extra allocation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8457">#8457</a></th>
<td>-ffull-laziness does more harm than good</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14564">#14564</a></th>
<td>CAF isn&apos;t floated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15606">#15606</a></th>
<td>Don&apos;t float out lets in between lambdsa</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16039">#16039</a></th>
<td>&apos;GHC.Magic.noinline &lt;var&gt;&apos; should not float out</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1945">#1945</a></th>
<td>Full laziness is sometimes a pessimisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3273">#3273</a></th>
<td>memory leak due to optimisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4276">#4276</a></th>
<td>-O0 runs in constant space, -O1 and -O2 don&apos;t</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5729">#5729</a></th>
<td>ForeignPtr leak in ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10535">#10535</a></th>
<td>Float out causes major space leak</td></tr></table>



