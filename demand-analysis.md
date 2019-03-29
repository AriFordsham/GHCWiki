# Demand analysis


This page summarises the state of play for GHC's demand analysis (aka strictness analysis).


See also

- [CPR analysis status page](nested-cpr).  CPR analysis is integrated with demand analysis.
- [Late demand analysis](late-dmd): notes on the final run of the demand analyser, late in the pipeline.
- [DemandAnalysis/SplitOffUsage](demand-analysis/split-off-usage): WIP. Some examples and arguments for why we should consider splitting off usage analysis from strictness analysis
- Rather out-of-date commentary: [Commentary/Compiler/StrictnessAnalysis](commentary/compiler/strictness-analysis)

## Tickets



Use Keyword = `DemandAnalysis` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/1171">#1171</a></th>
<td>GHC doesn&apos;t respect the imprecise exceptions semantics</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/3138">#3138</a></th>
<td>Returning a known constructor: GHC generates terrible code for cmonad</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/4941">#4941</a></th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5302">#5302</a></th>
<td>Unused arguments in join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5775">#5775</a></th>
<td>Inconsistency in demand analysis</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/6070">#6070</a></th>
<td>Fun with the demand analyser</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10069">#10069</a></th>
<td>CPR related performance issue</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10918">#10918</a></th>
<td>Float once-used let binding into a recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11126">#11126</a></th>
<td>Entered absent arg in a Repa program</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12364">#12364</a></th>
<td>Demand analysis for sum types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14259">#14259</a></th>
<td>Worker/Wrapper for sum return</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14816">#14816</a></th>
<td>Missed Called Arity opportunity?</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14998">#14998</a></th>
<td>Sort out the strictness mess for exceptions</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16284">#16284</a></th>
<td>Abortion of fixed-point iteration in Demand Analyser discards sound results</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/4267">#4267</a></th>
<td>Strictness analyser is to conservative about passing a boxed parameter</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5949">#5949</a></th>
<td>Demand analysis attributes manifestly wrong demand type</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7782">#7782</a></th>
<td>flag to run the demand analysis a second time</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15226">#15226</a></th>
<td>GHC doesn&apos;t know that seq# produces something in WHNF</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15627">#15627</a></th>
<td>Absent unlifted bindings</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15696">#15696</a></th>
<td>Derived Ord instance for enumerations with more than 8 elements seems to be incorrect</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16029">#16029</a></th>
<td>Inadequate absence analysis</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16197">#16197</a></th>
<td>Strictness is not preserved under -O1</td></tr></table>



