# The Static Argument Transformation (SAT)


This page summarises progress on the Static Argument Transformation.


See:

- [ Andre Sansos's thesis](https://www.microsoft.com/en-us/research/publication/compilation-transformation-non-strict-functional-languages/) which has a whole chapter.
- [ Danvy's lambda-dropping paper](http://ojs.statsbiblioteket.dk/index.php/brics/article/view/18785)


In comment:10 of [\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059), Max notes that SAT provides 20-30% wins in nofib, wow! 

## Tickets


Use Keyword = `StaticArgumentTransformation` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#888](https://gitlab.haskell.org//ghc/ghc/issues/888)</th>
<td>Implement the static argument transformation</td></tr>
<tr><th>[\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059)</th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th>[\#9374](https://gitlab.haskell.org//ghc/ghc/issues/9374)</th>
<td>Investigate Static Argument Transformation</td></tr>
<tr><th>[\#13502](https://gitlab.haskell.org//ghc/ghc/issues/13502)</th>
<td>Static argument transformation should also run after specialisation</td></tr>
<tr><th>[\#13966](https://gitlab.haskell.org//ghc/ghc/issues/13966)</th>
<td>Skip-less stream fusion: a missed opportunity</td></tr>
<tr><th>[\#14211](https://gitlab.haskell.org//ghc/ghc/issues/14211)</th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th>[\#14231](https://gitlab.haskell.org//ghc/ghc/issues/14231)</th>
<td>Core lint error "in result of Static argument"</td></tr>
<tr><th>[\#14649](https://gitlab.haskell.org//ghc/ghc/issues/14649)</th>
<td>ghc panic: mergeSATInfo</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#9545](https://gitlab.haskell.org//ghc/ghc/issues/9545)</th>
<td>Evaluate Takano Akio's foldrW/buildW fusion framework as a possible replacement for foldr/build</td></tr></table>