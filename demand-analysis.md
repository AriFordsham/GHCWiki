# Demand analysis


This page summarises the state of play for GHC's demand analysis (aka strictness analysis).


See also

- [CPR analysis status page](nested-cpr).  CPR analysis is integrated with demand analysis.
- [Late demand analysis](late-dmd): notes on the final run of the demand analyser, late in the pipeline.

- Rather out-of-date commentary: [Commentary/Compiler/StrictnessAnalysis](commentary/compiler/strictness-analysis)

## Tickets


Use Keyword = `DemandAnalysis` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#1171](https://gitlab.haskell.org//ghc/ghc/issues/1171)</th>
<td>GHC doesn't respect the imprecise exceptions semantics</td></tr>
<tr><th>[\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138)</th>
<td>Returning a known constructor: GHC generates terrible code for cmonad</td></tr>
<tr><th>[\#4941](https://gitlab.haskell.org//ghc/ghc/issues/4941)</th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th>[\#5302](https://gitlab.haskell.org//ghc/ghc/issues/5302)</th>
<td>Unused arguments in join points</td></tr>
<tr><th>[\#5775](https://gitlab.haskell.org//ghc/ghc/issues/5775)</th>
<td>Inconsistency in demand analysis</td></tr>
<tr><th>[\#6070](https://gitlab.haskell.org//ghc/ghc/issues/6070)</th>
<td>Fun with the demand analyser</td></tr>
<tr><th>[\#10069](https://gitlab.haskell.org//ghc/ghc/issues/10069)</th>
<td>CPR related performance issue</td></tr>
<tr><th>[\#10918](https://gitlab.haskell.org//ghc/ghc/issues/10918)</th>
<td>Float once-used let binding into a recursive function</td></tr>
<tr><th>[\#11126](https://gitlab.haskell.org//ghc/ghc/issues/11126)</th>
<td>Entered absent arg in a Repa program</td></tr>
<tr><th>[\#12364](https://gitlab.haskell.org//ghc/ghc/issues/12364)</th>
<td>Demand analysis for sum types</td></tr>
<tr><th>[\#14259](https://gitlab.haskell.org//ghc/ghc/issues/14259)</th>
<td>Worker/Wrapper for sum return</td></tr>
<tr><th>[\#14816](https://gitlab.haskell.org//ghc/ghc/issues/14816)</th>
<td>Missed Called Arity opportunity?</td></tr>
<tr><th>[\#14998](https://gitlab.haskell.org//ghc/ghc/issues/14998)</th>
<td>Sort out the strictness mess for exceptions</td></tr>
<tr><th>[\#16284](https://gitlab.haskell.org//ghc/ghc/issues/16284)</th>
<td>Abortion of fixed-point iteration in Demand Analyser discards sound results</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#4267](https://gitlab.haskell.org//ghc/ghc/issues/4267)</th>
<td>Strictness analyser is to conservative about passing a boxed parameter</td></tr>
<tr><th>[\#5949](https://gitlab.haskell.org//ghc/ghc/issues/5949)</th>
<td>Demand analysis attributes manifestly wrong demand type</td></tr>
<tr><th>[\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782)</th>
<td>flag to run the demand analysis a second time</td></tr>
<tr><th>[\#15226](https://gitlab.haskell.org//ghc/ghc/issues/15226)</th>
<td>GHC doesn't know that seq\# produces something in WHNF</td></tr>
<tr><th>[\#15627](https://gitlab.haskell.org//ghc/ghc/issues/15627)</th>
<td>Absent unlifted bindings</td></tr>
<tr><th>[\#15696](https://gitlab.haskell.org//ghc/ghc/issues/15696)</th>
<td>Derived Ord instance for enumerations with more than 8 elements seems to be incorrect</td></tr>
<tr><th>[\#16029](https://gitlab.haskell.org//ghc/ghc/issues/16029)</th>
<td>Inadequate absence analysis</td></tr>
<tr><th>[\#16197](https://gitlab.haskell.org//ghc/ghc/issues/16197)</th>
<td>Strictness is not preserved under -O1</td></tr></table>