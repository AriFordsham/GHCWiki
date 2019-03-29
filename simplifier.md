# The mighty simplifier


This page summarises the state of play for GHC's simplifier, including inlining heuristics.

## Tickets



Use Keyword = `Simplifier` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7411">#7411</a></th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9279">#9279</a></th>
<td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13011">#13011</a></th>
<td>Simplifier ticks exhausted: a 10-line case</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13715">#13715</a></th>
<td>test dynamic-paper for way profasm fails with Simplifier ticks exhausted</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13851">#13851</a></th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15056">#15056</a></th>
<td>Wrappers inlined too late</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15717">#15717</a></th>
<td>Performance regression in for_ alternatives from GHC 8.2.2 to newer GHCs</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16296">#16296</a></th>
<td>OccurAnal should not break the linter assumptions</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/4237">#4237</a></th>
<td>-dcore-lint error after simplifier iteration 1 when profiling</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/4462">#4462</a></th>
<td>-dcore-lint error in simplifier phase 0 when profiling</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8168">#8168</a></th>
<td>ghc &quot;Simplifier ticks exhausted&quot; &quot;When trying UnfoldingDone&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10372">#10372</a></th>
<td>panic! compiling Y combinator with optimizations</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16288">#16288</a></th>
<td>Core Lint error: Occurrence is GlobalId, but binding is LocalId</td></tr></table>



