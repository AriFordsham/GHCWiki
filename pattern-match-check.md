# Exhaustiveness/Redundancy Check


As stated in [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595), GHC's overlapping/non-exhaustive pattern checking is old and
crufty and misbehaves with several GHC's extensions, notably GADTs. In this page
we describe the problem and the algorithm.  It forms part of GHC 8.0.

## Status


The ticket [\#11528](https://gitlab.haskell.org//ghc/ghc/issues/11528) tracks our aspiration to return to a more elegant (but currently less performant) implementation.


Tickets should include `PatternMatchWarnings` in their Keywords to appear in these summary lists.

**Open Tickets:**

<table><tr><th>[\#10116](https://gitlab.haskell.org//ghc/ghc/issues/10116)</th>
<td>Closed type families: Warn if it doesn't handle all cases</td></tr>
<tr><th>[\#11195](https://gitlab.haskell.org//ghc/ghc/issues/11195)</th>
<td>New pattern-match check can be non-performant</td></tr>
<tr><th>[\#11253](https://gitlab.haskell.org//ghc/ghc/issues/11253)</th>
<td>Duplicate warnings for pattern guards and relevant features (e.g. View Patterns)</td></tr>
<tr><th>[\#11503](https://gitlab.haskell.org//ghc/ghc/issues/11503)</th>
<td>TypeError woes (incl. pattern match checker)</td></tr>
<tr><th>[\#11528](https://gitlab.haskell.org//ghc/ghc/issues/11528)</th>
<td>Representation of value set abstractions as trees causes performance issues</td></tr>
<tr><th>[\#11822](https://gitlab.haskell.org//ghc/ghc/issues/11822)</th>
<td>Pattern match checker exceeded (2000000) iterations</td></tr>
<tr><th>[\#12694](https://gitlab.haskell.org//ghc/ghc/issues/12694)</th>
<td>GHC HEAD no longer reports inaccessible code</td></tr>
<tr><th>[\#12949](https://gitlab.haskell.org//ghc/ghc/issues/12949)</th>
<td>Pattern coverage checker ignores dictionary arguments</td></tr>
<tr><th>[\#13021](https://gitlab.haskell.org//ghc/ghc/issues/13021)</th>
<td>Inaccessible RHS warning is confusing for users</td></tr>
<tr><th>[\#13363](https://gitlab.haskell.org//ghc/ghc/issues/13363)</th>
<td>Wildcard patterns and COMPLETE sets can lead to misleading redundant pattern-match warnings</td></tr>
<tr><th>[\#13717](https://gitlab.haskell.org//ghc/ghc/issues/13717)</th>
<td>Pattern synonym exhaustiveness checks don't play well with EmptyCase</td></tr>
<tr><th>[\#13766](https://gitlab.haskell.org//ghc/ghc/issues/13766)</th>
<td>Confusing "redundant pattern match" in 8.0, no warning at all in 8.2</td></tr>
<tr><th>[\#13964](https://gitlab.haskell.org//ghc/ghc/issues/13964)</th>
<td>Pattern-match warnings for datatypes with COMPLETE sets break abstraction</td></tr>
<tr><th>[\#13965](https://gitlab.haskell.org//ghc/ghc/issues/13965)</th>
<td>COMPLETE sets nerf redundant pattern-match warnings</td></tr>
<tr><th>[\#14059](https://gitlab.haskell.org//ghc/ghc/issues/14059)</th>
<td>COMPLETE sets don't work at all with data family instances</td></tr>
<tr><th>[\#14133](https://gitlab.haskell.org//ghc/ghc/issues/14133)</th>
<td>COMPLETE pragmas seem to be ignored when using view patterns</td></tr>
<tr><th>[\#14253](https://gitlab.haskell.org//ghc/ghc/issues/14253)</th>
<td>Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable</td></tr>
<tr><th>[\#14838](https://gitlab.haskell.org//ghc/ghc/issues/14838)</th>
<td>missing "incomplete-patterns" warning for TH-generated functions</td></tr>
<tr><th>[\#14851](https://gitlab.haskell.org//ghc/ghc/issues/14851)</th>
<td>"Pattern match has inaccessible right hand side" with TypeRep</td></tr>
<tr><th>[\#14899](https://gitlab.haskell.org//ghc/ghc/issues/14899)</th>
<td>Significant compilation time regression between 8.4 and HEAD due to coverage checking</td></tr>
<tr><th>[\#14987](https://gitlab.haskell.org//ghc/ghc/issues/14987)</th>
<td>Memory usage exploding for complex pattern matching</td></tr>
<tr><th>[\#15014](https://gitlab.haskell.org//ghc/ghc/issues/15014)</th>
<td>Exhaustivity check should suggest when COMPLETE could be helpful</td></tr>
<tr><th>[\#15554](https://gitlab.haskell.org//ghc/ghc/issues/15554)</th>
<td>COMPLETE pragmas make overlapping-patterns warnings behave oddly</td></tr>
<tr><th>[\#15681](https://gitlab.haskell.org//ghc/ghc/issues/15681)</th>
<td>Take exhaustiveness checking into consideration when using MonadFailDesugaring</td></tr>
<tr><th>[\#15713](https://gitlab.haskell.org//ghc/ghc/issues/15713)</th>
<td>Bogus -Woverlapping-patterns warning with OverloadedStrings</td></tr>
<tr><th>[\#15744](https://gitlab.haskell.org//ghc/ghc/issues/15744)</th>
<td>Existence of complete pattern synonym hides unrelated incomplete pattern warning</td></tr>
<tr><th>[\#15753](https://gitlab.haskell.org//ghc/ghc/issues/15753)</th>
<td>Inconsistent pattern-match warnings when using guards versus case expressions</td></tr>
<tr><th>[\#15885](https://gitlab.haskell.org//ghc/ghc/issues/15885)</th>
<td>Enhancing COMPLETE pragma to support pattern synonyms with polymorphic (output) types</td></tr>
<tr><th>[\#16128](https://gitlab.haskell.org//ghc/ghc/issues/16128)</th>
<td>Pattern match checker should shortcut on simple cases</td></tr>
<tr><th>[\#16278](https://gitlab.haskell.org//ghc/ghc/issues/16278)</th>
<td>Exhaustivity checking GADT with free variables</td></tr>
<tr><th>[\#16289](https://gitlab.haskell.org//ghc/ghc/issues/16289)</th>
<td>GHC thinks pattern match is exhaustive</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#322](https://gitlab.haskell.org//ghc/ghc/issues/322)</th>
<td>fromInteger-related pattern match overlap warnings</td></tr>
<tr><th>[\#595](https://gitlab.haskell.org//ghc/ghc/issues/595)</th>
<td>Overhaul GHC's overlapping/non-exhaustive pattern checking</td></tr>
<tr><th>[\#2204](https://gitlab.haskell.org//ghc/ghc/issues/2204)</th>
<td>Improve 'patterns not matched' warnings</td></tr>
<tr><th>[\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927)</th>
<td>Incomplete/overlapped pattern warnings + GADTs = inadequate</td></tr>
<tr><th>[\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139)</th>
<td>Spurious non-exhaustive pattern match warnings are given using GADTs</td></tr>
<tr><th>[\#5724](https://gitlab.haskell.org//ghc/ghc/issues/5724)</th>
<td>Confusing warning message for incomplete patterns</td></tr>
<tr><th>[\#5728](https://gitlab.haskell.org//ghc/ghc/issues/5728)</th>
<td>Warnings from -fwarn-incomplete-record-updates even with all constructors matched</td></tr>
<tr><th>[\#5762](https://gitlab.haskell.org//ghc/ghc/issues/5762)</th>
<td>GHC gives incorrect warnings with simple applications of the view patterns extension</td></tr>
<tr><th>[\#6124](https://gitlab.haskell.org//ghc/ghc/issues/6124)</th>
<td>Spurious non-exhaustive warning with GADT and newtypes</td></tr>
<tr><th>[\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669)</th>
<td>Empty case causes warning</td></tr>
<tr><th>[\#8016](https://gitlab.haskell.org//ghc/ghc/issues/8016)</th>
<td>case expression with mixed use of Num instances cause spurious overlap warning</td></tr>
<tr><th>[\#8494](https://gitlab.haskell.org//ghc/ghc/issues/8494)</th>
<td>Warn if a pattern guard obviates all others</td></tr>
<tr><th>[\#8710](https://gitlab.haskell.org//ghc/ghc/issues/8710)</th>
<td>Overlapping patterns warning misplaced</td></tr>
<tr><th>[\#8853](https://gitlab.haskell.org//ghc/ghc/issues/8853)</th>
<td>Surprising mention of unboxed integers in pattern exhaustiveness warning</td></tr>
<tr><th>[\#8970](https://gitlab.haskell.org//ghc/ghc/issues/8970)</th>
<td>Non-exhaustive pattern match warning with DataKinds and TypeFamilies</td></tr>
<tr><th>[\#9113](https://gitlab.haskell.org//ghc/ghc/issues/9113)</th>
<td>Template Haskell should warn about non-exhaustive pattern matches</td></tr>
<tr><th>[\#9951](https://gitlab.haskell.org//ghc/ghc/issues/9951)</th>
<td>OverloadedLists breaks exhaustiveness check</td></tr>
<tr><th>[\#10393](https://gitlab.haskell.org//ghc/ghc/issues/10393)</th>
<td>Bogus warning with OverloadedLists</td></tr>
<tr><th>[\#10746](https://gitlab.haskell.org//ghc/ghc/issues/10746)</th>
<td>No non-exhaustive pattern match warning given for empty case analysis</td></tr>
<tr><th>[\#11160](https://gitlab.haskell.org//ghc/ghc/issues/11160)</th>
<td>New exhaustiveness checker breaks ghcirun004</td></tr>
<tr><th>[\#11161](https://gitlab.haskell.org//ghc/ghc/issues/11161)</th>
<td>New exhaustiveness checker breaks concurrent/prog001</td></tr>
<tr><th>[\#11162](https://gitlab.haskell.org//ghc/ghc/issues/11162)</th>
<td>T783 regresses severely in allocations with new pattern match checker</td></tr>
<tr><th>[\#11163](https://gitlab.haskell.org//ghc/ghc/issues/11163)</th>
<td>New exhaustiveness checker breaks T5642</td></tr>
<tr><th>[\#11276](https://gitlab.haskell.org//ghc/ghc/issues/11276)</th>
<td>GHC hangs/takes an exponential amount of time with simple program</td></tr>
<tr><th>[\#11302](https://gitlab.haskell.org//ghc/ghc/issues/11302)</th>
<td>GHC HEAD uses up all memory while compiling \`genprimcode\`</td></tr>
<tr><th>[\#11303](https://gitlab.haskell.org//ghc/ghc/issues/11303)</th>
<td>Pattern matching against sets of strings sharing a prefix blows up pattern checker</td></tr>
<tr><th>[\#11316](https://gitlab.haskell.org//ghc/ghc/issues/11316)</th>
<td>Too many guards warning causes issues</td></tr>
<tr><th>[\#11374](https://gitlab.haskell.org//ghc/ghc/issues/11374)</th>
<td>\`-Woverlapping-patterns\` induced memory-blowup</td></tr>
<tr><th>[\#11390](https://gitlab.haskell.org//ghc/ghc/issues/11390)</th>
<td>GHC does not warn about redundant patterns</td></tr>
<tr><th>[\#11806](https://gitlab.haskell.org//ghc/ghc/issues/11806)</th>
<td>GHC does not warn for mistakenly empty case</td></tr>
<tr><th>[\#11984](https://gitlab.haskell.org//ghc/ghc/issues/11984)</th>
<td>Pattern match incompleteness / inaccessibility discrepancy</td></tr>
<tr><th>[\#12158](https://gitlab.haskell.org//ghc/ghc/issues/12158)</th>
<td>ghc: panic! (the 'impossible' happened)  translateConPatVec: lookup</td></tr>
<tr><th>[\#13517](https://gitlab.haskell.org//ghc/ghc/issues/13517)</th>
<td>No warnings produced, yet the pattern matching fails at runtime.</td></tr>
<tr><th>[\#14098](https://gitlab.haskell.org//ghc/ghc/issues/14098)</th>
<td>Incorrect pattern match warning on nested GADTs</td></tr>
<tr><th>[\#14546](https://gitlab.haskell.org//ghc/ghc/issues/14546)</th>
<td>-Woverlapping-patterns warns on wrong patterns for Int</td></tr>
<tr><th>[\#14547](https://gitlab.haskell.org//ghc/ghc/issues/14547)</th>
<td>Wrong warning by -Wincomplete-patterns</td></tr>
<tr><th>[\#14773](https://gitlab.haskell.org//ghc/ghc/issues/14773)</th>
<td>MultiWayIf makes it easy to write partial programs that are not catched by -Wall</td></tr>
<tr><th>[\#14813](https://gitlab.haskell.org//ghc/ghc/issues/14813)</th>
<td>EmptyCase thinks pattern match involving type family is not exhaustive, when it actually is</td></tr>
<tr><th>[\#15305](https://gitlab.haskell.org//ghc/ghc/issues/15305)</th>
<td>Erroneous "non-exhaustive pattern match" using nested GADT with strictness annotation</td></tr>
<tr><th>[\#15385](https://gitlab.haskell.org//ghc/ghc/issues/15385)</th>
<td>-Wincomplete-patterns gets confused when combining GADTs and pattern guards</td></tr>
<tr><th>[\#15398](https://gitlab.haskell.org//ghc/ghc/issues/15398)</th>
<td>GADT deriving Ord generates inaccessible code in a pattern with constructor.</td></tr>
<tr><th>[\#15450](https://gitlab.haskell.org//ghc/ghc/issues/15450)</th>
<td>Inconsistency w.r.t. coverage checking warnings for EmptyCase under unsatisfiable constraints</td></tr>
<tr><th>[\#15584](https://gitlab.haskell.org//ghc/ghc/issues/15584)</th>
<td>nonVoid is too conservative w.r.t. strict argument types</td></tr>
<tr><th>[\#15884](https://gitlab.haskell.org//ghc/ghc/issues/15884)</th>
<td>Completeness of View Patterns With a Complete Set of Output Patterns</td></tr>
<tr><th>[\#15886](https://gitlab.haskell.org//ghc/ghc/issues/15886)</th>
<td>Spurious warning about incomplete pattern with PatternSynonyms</td></tr>
<tr><th>[\#16129](https://gitlab.haskell.org//ghc/ghc/issues/16129)</th>
<td>Incorrect non-exhaustive pattern warning with PatternSynonyms</td></tr>
<tr><th>[\#16377](https://gitlab.haskell.org//ghc/ghc/issues/16377)</th>
<td>\`TypeError\` in a pattern should flag inaccessible code</td></tr></table>

**Background**:

- The paper on which the previous approach were based [ Two techniques for compiling lazy pattern matching](http://moscova.inria.fr/~maranget/papers/lazy-pats-derniere.ps.gz)
- Peter Sestoft's paper for negative patterns [ ML's pattern matching compilation and partial evaluation](http://lambda.csail.mit.edu/~chet/papers/others/s/sestoft/sestoft96ml.pdf)

**Our solution**

- Our paper, describing the algorithm we implemented [ GADTs meet their match](http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf).
- [PatternMatchCheckImplementation](pattern-match-check-implementation) talks about the implementation in GHC.

**Related tickets** (ones that are closed are still useful examples in the wild; they were only closed as duplicates):

- [\#29](https://gitlab.haskell.org//ghc/ghc/issues/29)
- [\#322](https://gitlab.haskell.org//ghc/ghc/issues/322)
- [\#366](https://gitlab.haskell.org//ghc/ghc/issues/366)
- [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595)
- [\#851](https://gitlab.haskell.org//ghc/ghc/issues/851)
- [\#1307](https://gitlab.haskell.org//ghc/ghc/issues/1307)
- [\#2006](https://gitlab.haskell.org//ghc/ghc/issues/2006)
- [\#2204](https://gitlab.haskell.org//ghc/ghc/issues/2204)
- [\#3078](https://gitlab.haskell.org//ghc/ghc/issues/3078)
- [\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927)
- [\#4139](https://gitlab.haskell.org//ghc/ghc/issues/4139)
- [\#5724](https://gitlab.haskell.org//ghc/ghc/issues/5724)
- [\#5728](https://gitlab.haskell.org//ghc/ghc/issues/5728)
- [\#5762](https://gitlab.haskell.org//ghc/ghc/issues/5762)
- [\#6124](https://gitlab.haskell.org//ghc/ghc/issues/6124)
- [\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669)
- [\#8016](https://gitlab.haskell.org//ghc/ghc/issues/8016)
- [\#8494](https://gitlab.haskell.org//ghc/ghc/issues/8494)
- [\#8779](https://gitlab.haskell.org//ghc/ghc/issues/8779)
- [\#8853](https://gitlab.haskell.org//ghc/ghc/issues/8853)
- [\#8970](https://gitlab.haskell.org//ghc/ghc/issues/8970)
- [\#9113](https://gitlab.haskell.org//ghc/ghc/issues/9113)
- [\#9951](https://gitlab.haskell.org//ghc/ghc/issues/9951)
- [\#10116](https://gitlab.haskell.org//ghc/ghc/issues/10116)
- [\#10600](https://gitlab.haskell.org//ghc/ghc/issues/10600)
- [\#10746](https://gitlab.haskell.org//ghc/ghc/issues/10746)

# The main problem we wish to solve


Since GHC's exhaustiveness/redundancy checker is outdated, it does not take into
account constraints introduced by GADT matches when reporting warnings. This is
illustrated in the following example ([\#3927](https://gitlab.haskell.org//ghc/ghc/issues/3927)):

```wiki
data T a where
  T1 :: T Int
  T2 :: T Bool

f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
```


Even though the above definition for `f` is exhaustive, we get a warning of the
form:

```wiki
    Pattern match(es) are non-exhaustive
    In an equation for `f':
        Patterns not matched:
            T1 T2
            T2 T1
```


Obviously, both pattern vectors issued as not matched, are ill-typed, because
they both generate the inconsistent constraint `Int ~ Bool`. This becomes more
clear if we rewrite the definition of `T` in the equivalent form:

```wiki
data T a where
  T1 :: forall a. (a ~ Int)  => T a
  T2 :: forall a. (a ~ Bool) => T a
```


Additionally, if we add one more branch to `f`:

```wiki
f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
f _  _  = undefined -- inaccessible
```


we get no warning about the redundancy of the last clause.

# General approach


Note that improving the redundancy check is quite more challenging than the
exhaustiveness check. For exhaustiveness it is sufficient to collect all potentially
missing vectors using the previous syntax-based approach and then filter out the
ill-typed. Nevertheless, for redundancy we need to compute the cases that are
covered by every clause and then filter out the ill-typed. The difficulty lies in
the fact that what the last branch of `f` covers depends on what remains uncovered
by the above two clauses. This indicates that for the redundancy check we need an
incremental way of computing uncovered vectors.

# The solution


Until now, the algorithm used by GHC was based on a technique originally introduced
for compilation of pattern matching in decision trees (see paper above). Hence, it
used a column-based approach to traverse the pattern matrix, which cannot be used
incrementally as we descibed above.


Instead, we traverse the pattern matrix line-by-line. The general approach is the
following: We start with everything considered as missing and then, for every clause
we compute:

- Which cases it covers
- If it forces the evaluation of arguments (see Laziness below)
- Which cases are left unhandled


For example, for function `f` above we have:

- initial_missing = `[[_ _]]`

  ```wiki
  f T1 T1 = True -- first clause
  ```
- Covers `[[T1 T1]]`
- Forces the evaluation of the 1st argument
- If 1st argument is `T1` forces the evaluation of the 2nd argument
- Remain uncovered `[[T2 _], [T1 T2]]`

  ```wiki
  f T2 T2 = False -- second clause
  ```
- Covers `[[T2 T2]]`
- If 1st argument is `T2` forces the evaluation of the 2nd argument
- Remain uncovered `[[T2 T1], [T1 T2]]`

  ```wiki
  f _  _  = undefined -- third clause (inaccessible)
  ```
- Covers: `[[T2 T1], [T1 T2]]`
- Doesn't force anything
- Remain uncovered: `[]`


Now we can easily check both covered and uncovered cases and filter out the
ill-typed, before deciding if the match is exhaustive and which clauses are
useful.

# Laziness


Even without GADTs, the previous algorithm was not exactly laziness-aware. For
example, for function `g` below

```wiki
g :: Bool -> Bool -> Bool
g _    True = True
g True True = True
g _    _    = False
```


We would get a warning

```wiki
    Pattern match(es) are overlapped
    In an equation for `g': g True True = ...
```


Yet, this is not correct. The second clause may be totally overlapped by the
first clause but it is not actually redundant. If we call `g` we get:

```wiki
ghci> g undefined False
*** Exception: Prelude.undefined
```


because the second clause forces the evaluation of the first argument. Yet, if
we remove it we get:

```wiki
ghci> g undefined False
False
```


That is, we changed the semantics of `g`.
