# The mighty simplifier


This page summarises the state of play for GHC's simplifier, including inlining heuristics.

## Tickets


Use Keyword = `Simplifier` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#7411](https://gitlab.haskell.org//ghc/ghc/issues/7411)</th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th>[\#9279](https://gitlab.haskell.org//ghc/ghc/issues/9279)</th>
<td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
<tr><th>[\#13011](https://gitlab.haskell.org//ghc/ghc/issues/13011)</th>
<td>Simplifier ticks exhausted: a 10-line case</td></tr>
<tr><th>[\#13715](https://gitlab.haskell.org//ghc/ghc/issues/13715)</th>
<td>test dynamic-paper for way profasm fails with Simplifier ticks exhausted</td></tr>
<tr><th>[\#13851](https://gitlab.haskell.org//ghc/ghc/issues/13851)</th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th>[\#15056](https://gitlab.haskell.org//ghc/ghc/issues/15056)</th>
<td>Wrappers inlined too late</td></tr>
<tr><th>[\#15717](https://gitlab.haskell.org//ghc/ghc/issues/15717)</th>
<td>Performance regression in for_ alternatives from GHC 8.2.2 to newer GHCs</td></tr>
<tr><th>[\#16296](https://gitlab.haskell.org//ghc/ghc/issues/16296)</th>
<td>OccurAnal should not break the linter assumptions</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#4237](https://gitlab.haskell.org//ghc/ghc/issues/4237)</th>
<td>-dcore-lint error after simplifier iteration 1 when profiling</td></tr>
<tr><th>[\#4462](https://gitlab.haskell.org//ghc/ghc/issues/4462)</th>
<td>-dcore-lint error in simplifier phase 0 when profiling</td></tr>
<tr><th>[\#8168](https://gitlab.haskell.org//ghc/ghc/issues/8168)</th>
<td>ghc "Simplifier ticks exhausted" "When trying UnfoldingDone"</td></tr>
<tr><th>[\#10372](https://gitlab.haskell.org//ghc/ghc/issues/10372)</th>
<td>panic! compiling Y combinator with optimizations</td></tr>
<tr><th>[\#16288](https://gitlab.haskell.org//ghc/ghc/issues/16288)</th>
<td>Core Lint error: Occurrence is GlobalId, but binding is LocalId</td></tr></table>