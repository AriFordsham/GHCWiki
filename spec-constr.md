# [SpecConstr](spec-constr)

`SpecConstr` is a GHC optimisation that specialises functions for particular values of their arguments.

- [Here is the paper about SpecConstr](https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions-2/)

## Tickets



Use Keyword = `SpecConstr` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/855">#855</a></th>
<td>Improvements to SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2255">#2255</a></th>
<td>Improve SpecConstr for free variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2598">#2598</a></th>
<td>Avoid excessive specialisation in SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2642">#2642</a></th>
<td>Improve SpecConstr for join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3767">#3767</a></th>
<td>SpecConstr for join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3831">#3831</a></th>
<td>SpecConstr should exploit cases where there is exactly one call pattern</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4941">#4941</a></th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4945">#4945</a></th>
<td>Another SpecConstr infelicity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5059">#5059</a></th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5075">#5075</a></th>
<td>CPR optimisation for sum types if only one constructor is used</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10346">#10346</a></th>
<td>Cross-module SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10626">#10626</a></th>
<td>Missed opportunity for SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11668">#11668</a></th>
<td>SPEC has a runtime cost if constructor specialization isn&apos;t performed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13014">#13014</a></th>
<td>Seemingly unnecessary marking of a SpecConstr specialization as a loopbreaker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13346">#13346</a></th>
<td>Run nofib with -fspec-constr-keen</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13681">#13681</a></th>
<td>Remove deprecated ForceSpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13694">#13694</a></th>
<td>CSE runs before SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13867">#13867</a></th>
<td>Silly definitions remain after SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14565">#14565</a></th>
<td>Performance degrades from -O1 to -O2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14844">#14844</a></th>
<td>SpecConstr also non-recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14951">#14951</a></th>
<td>SpecConstr needs two runs when one should suffice</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15069">#15069</a></th>
<td>Missed SpecConstr opportunity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15519">#15519</a></th>
<td>Minor code refactoring leads to drastic performance degradation</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7865">#7865</a></th>
<td>SpecConstr duplicating computations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7944">#7944</a></th>
<td>GHC goes into an apparently infinite loop at -O2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13410">#13410</a></th>
<td>GHC HEAD regression: Template variable unbound in rewrite rule</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14936">#14936</a></th>
<td>GHC 8.4 performance regressions when using newtypes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14955">#14955</a></th>
<td>Musings on manual type class desugaring</td></tr></table>



