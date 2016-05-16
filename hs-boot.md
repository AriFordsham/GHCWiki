# hs-boot files jiggery pokery

`hs-boot` files are used by GHC to support mutually-recursive modules

## Status


Use Keyword = `hs-boot` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#1012](https://gitlab.haskell.org//ghc/ghc/issues/1012)</th>
<td>ghc panic with mutually recursive modules and template haskell</td></tr>
<tr><th>[\#10333](https://gitlab.haskell.org//ghc/ghc/issues/10333)</th>
<td>hs-boot modification doesn't induce recompilation</td></tr>
<tr><th>[\#12063](https://gitlab.haskell.org//ghc/ghc/issues/12063)</th>
<td>Knot-tying failure when type-synonym refers to non-existent data</td></tr>
<tr><th>[\#13180](https://gitlab.haskell.org//ghc/ghc/issues/13180)</th>
<td>Confusing error when hs-boot abstract data implemented using synonym</td></tr>
<tr><th>[\#13299](https://gitlab.haskell.org//ghc/ghc/issues/13299)</th>
<td>Typecheck multiple modules at the same time</td></tr>
<tr><th>[\#13347](https://gitlab.haskell.org//ghc/ghc/issues/13347)</th>
<td>Abstract classes in hs-boot should not be treated as injective</td></tr>
<tr><th>[\#13981](https://gitlab.haskell.org//ghc/ghc/issues/13981)</th>
<td>Family instance consistency checks happens too early when hs-boot defined type occurs on LHS</td></tr>
<tr><th>[\#14092](https://gitlab.haskell.org//ghc/ghc/issues/14092)</th>
<td>hs-boot unfolding visibility not consistent between --make and -c</td></tr>
<tr><th>[\#14103](https://gitlab.haskell.org//ghc/ghc/issues/14103)</th>
<td>Retypechecking the loop in --make mode is super-linear when there are many .hs-boot modules</td></tr>
<tr><th>[\#16127](https://gitlab.haskell.org//ghc/ghc/issues/16127)</th>
<td>Panic: piResultTys1 in compiler/types/Type.hs:1022:5</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#2412](https://gitlab.haskell.org//ghc/ghc/issues/2412)</th>
<td>Interaction between type synonyms and .hs-boot causes panic "tcIfaceGlobal (local): not found"</td></tr>
<tr><th>[\#4003](https://gitlab.haskell.org//ghc/ghc/issues/4003)</th>
<td>tcIfaceGlobal panic building HEAD with 6.12.2</td></tr>
<tr><th>[\#7672](https://gitlab.haskell.org//ghc/ghc/issues/7672)</th>
<td>boot file entities are sometimes invisible and are not (semantically) unified with corresponding entities in implementing module</td></tr>
<tr><th>[\#10083](https://gitlab.haskell.org//ghc/ghc/issues/10083)</th>
<td>ghc: panic! (the 'impossible' happened)</td></tr>
<tr><th>[\#11062](https://gitlab.haskell.org//ghc/ghc/issues/11062)</th>
<td>Type families + hs-boot files = panic (type family consistency check too early)</td></tr>
<tr><th>[\#12035](https://gitlab.haskell.org//ghc/ghc/issues/12035)</th>
<td>hs-boot knot tying insufficient for ghc --make</td></tr>
<tr><th>[\#12042](https://gitlab.haskell.org//ghc/ghc/issues/12042)</th>
<td>Infinite loop with type synonyms and hs-boot</td></tr>
<tr><th>[\#12064](https://gitlab.haskell.org//ghc/ghc/issues/12064)</th>
<td>tcIfaceGlobal error with existentially quantified types</td></tr>
<tr><th>[\#13140](https://gitlab.haskell.org//ghc/ghc/issues/13140)</th>
<td>Handle subtyping relation for roles in Backpack</td></tr>
<tr><th>[\#13591](https://gitlab.haskell.org//ghc/ghc/issues/13591)</th>
<td>"\*\*\* Exception: expectJust showModule" in ghci with hs-boot</td></tr>
<tr><th>[\#13710](https://gitlab.haskell.org//ghc/ghc/issues/13710)</th>
<td>panic with boot and -jX</td></tr>
<tr><th>[\#13803](https://gitlab.haskell.org//ghc/ghc/issues/13803)</th>
<td>Panic while forcing the thunk for TyThing IsFile (regression)</td></tr>
<tr><th>[\#14075](https://gitlab.haskell.org//ghc/ghc/issues/14075)</th>
<td>GHC panic with parallel make</td></tr>
<tr><th>[\#14080](https://gitlab.haskell.org//ghc/ghc/issues/14080)</th>
<td>GHC panic while forcing the thunk for TyThing IsFile (regression)</td></tr>
<tr><th>[\#14396](https://gitlab.haskell.org//ghc/ghc/issues/14396)</th>
<td>Hs-boot woes during family instance consistency checks</td></tr>
<tr><th>[\#14531](https://gitlab.haskell.org//ghc/ghc/issues/14531)</th>
<td>tcIfaceGlobal (local): not found</td></tr></table>