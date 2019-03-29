# Exhaustiveness/Redundancy Check


As stated in [\#595](https://gitlab.haskell.org/ghc/ghc/issues/595), GHC's overlapping/non-exhaustive pattern checking is old and
crufty and misbehaves with several GHC's extensions, notably GADTs. In this page
we describe the problem and the algorithm.  It forms part of GHC 8.0.

## Status


The ticket [\#11528](https://gitlab.haskell.org/ghc/ghc/issues/11528) tracks our aspiration to return to a more elegant (but currently less performant) implementation.



Tickets should include `PatternMatchWarnings` in their Keywords to appear in these summary lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10116">#10116</a></th>
<td>Closed type families: Warn if it doesn&apos;t handle all cases</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11195">#11195</a></th>
<td>New pattern-match check can be non-performant</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11253">#11253</a></th>
<td>Duplicate warnings for pattern guards and relevant features (e.g. View Patterns)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11503">#11503</a></th>
<td>TypeError woes (incl. pattern match checker)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11528">#11528</a></th>
<td>Representation of value set abstractions as trees causes performance issues</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11822">#11822</a></th>
<td>Pattern match checker exceeded (2000000) iterations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12694">#12694</a></th>
<td>GHC HEAD no longer reports inaccessible code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12949">#12949</a></th>
<td>Pattern coverage checker ignores dictionary arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13021">#13021</a></th>
<td>Inaccessible RHS warning is confusing for users</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13363">#13363</a></th>
<td>Wildcard patterns and COMPLETE sets can lead to misleading redundant pattern-match warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13717">#13717</a></th>
<td>Pattern synonym exhaustiveness checks don&apos;t play well with EmptyCase</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13766">#13766</a></th>
<td>Confusing &quot;redundant pattern match&quot; in 8.0, no warning at all in 8.2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13964">#13964</a></th>
<td>Pattern-match warnings for datatypes with COMPLETE sets break abstraction</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13965">#13965</a></th>
<td>COMPLETE sets nerf redundant pattern-match warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14059">#14059</a></th>
<td>COMPLETE sets don&apos;t work at all with data family instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14133">#14133</a></th>
<td>COMPLETE pragmas seem to be ignored when using view patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14253">#14253</a></th>
<td>Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14838">#14838</a></th>
<td>missing &quot;incomplete-patterns&quot; warning for TH-generated functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14851">#14851</a></th>
<td>&quot;Pattern match has inaccessible right hand side&quot; with TypeRep</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14899">#14899</a></th>
<td>Significant compilation time regression between 8.4 and HEAD due to coverage checking</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14987">#14987</a></th>
<td>Memory usage exploding for complex pattern matching</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15014">#15014</a></th>
<td>Exhaustivity check should suggest when COMPLETE could be helpful</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15554">#15554</a></th>
<td>COMPLETE pragmas make overlapping-patterns warnings behave oddly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15681">#15681</a></th>
<td>Take exhaustiveness checking into consideration when using MonadFailDesugaring</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15713">#15713</a></th>
<td>Bogus -Woverlapping-patterns warning with OverloadedStrings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15744">#15744</a></th>
<td>Existence of complete pattern synonym hides unrelated incomplete pattern warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15753">#15753</a></th>
<td>Inconsistent pattern-match warnings when using guards versus case expressions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15885">#15885</a></th>
<td>Enhancing COMPLETE pragma to support pattern synonyms with polymorphic (output) types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16128">#16128</a></th>
<td>Pattern match checker should shortcut on simple cases</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16278">#16278</a></th>
<td>Exhaustivity checking GADT with free variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16289">#16289</a></th>
<td>GHC thinks pattern match is exhaustive</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/322">#322</a></th>
<td>fromInteger-related pattern match overlap warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/595">#595</a></th>
<td>Overhaul GHC&apos;s overlapping/non-exhaustive pattern checking</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2204">#2204</a></th>
<td>Improve &apos;patterns not matched&apos; warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3927">#3927</a></th>
<td>Incomplete/overlapped pattern warnings + GADTs = inadequate</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4139">#4139</a></th>
<td>Spurious non-exhaustive pattern match warnings are given using GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5724">#5724</a></th>
<td>Confusing warning message for incomplete patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5728">#5728</a></th>
<td>Warnings from -fwarn-incomplete-record-updates even with all constructors matched</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5762">#5762</a></th>
<td>GHC gives incorrect warnings with simple applications of the view patterns extension</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6124">#6124</a></th>
<td>Spurious non-exhaustive warning with GADT and newtypes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7669">#7669</a></th>
<td>Empty case causes warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8016">#8016</a></th>
<td>case expression with mixed use of Num instances cause spurious overlap warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8494">#8494</a></th>
<td>Warn if a pattern guard obviates all others</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8710">#8710</a></th>
<td>Overlapping patterns warning misplaced</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8853">#8853</a></th>
<td>Surprising mention of unboxed integers in pattern exhaustiveness warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8970">#8970</a></th>
<td>Non-exhaustive pattern match warning with DataKinds and TypeFamilies</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9113">#9113</a></th>
<td>Template Haskell should warn about non-exhaustive pattern matches</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9951">#9951</a></th>
<td>OverloadedLists breaks exhaustiveness check</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10393">#10393</a></th>
<td>Bogus warning with OverloadedLists</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10746">#10746</a></th>
<td>No non-exhaustive pattern match warning given for empty case analysis</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11160">#11160</a></th>
<td>New exhaustiveness checker breaks ghcirun004</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11161">#11161</a></th>
<td>New exhaustiveness checker breaks concurrent/prog001</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11162">#11162</a></th>
<td>T783 regresses severely in allocations with new pattern match checker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11163">#11163</a></th>
<td>New exhaustiveness checker breaks T5642</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11276">#11276</a></th>
<td>GHC hangs/takes an exponential amount of time with simple program</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11302">#11302</a></th>
<td>GHC HEAD uses up all memory while compiling `genprimcode`</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11303">#11303</a></th>
<td>Pattern matching against sets of strings sharing a prefix blows up pattern checker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11316">#11316</a></th>
<td>Too many guards warning causes issues</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11374">#11374</a></th>
<td>`-Woverlapping-patterns` induced memory-blowup</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11390">#11390</a></th>
<td>GHC does not warn about redundant patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11806">#11806</a></th>
<td>GHC does not warn for mistakenly empty case</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11984">#11984</a></th>
<td>Pattern match incompleteness / inaccessibility discrepancy</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12158">#12158</a></th>
<td>ghc: panic! (the &apos;impossible&apos; happened)  translateConPatVec: lookup</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13517">#13517</a></th>
<td>No warnings produced, yet the pattern matching fails at runtime.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14098">#14098</a></th>
<td>Incorrect pattern match warning on nested GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14546">#14546</a></th>
<td>-Woverlapping-patterns warns on wrong patterns for Int</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14547">#14547</a></th>
<td>Wrong warning by -Wincomplete-patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14773">#14773</a></th>
<td>MultiWayIf makes it easy to write partial programs that are not catched by -Wall</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14813">#14813</a></th>
<td>EmptyCase thinks pattern match involving type family is not exhaustive, when it actually is</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15305">#15305</a></th>
<td>Erroneous &quot;non-exhaustive pattern match&quot; using nested GADT with strictness annotation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15385">#15385</a></th>
<td>-Wincomplete-patterns gets confused when combining GADTs and pattern guards</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15398">#15398</a></th>
<td>GADT deriving Ord generates inaccessible code in a pattern with constructor.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15450">#15450</a></th>
<td>Inconsistency w.r.t. coverage checking warnings for EmptyCase under unsatisfiable constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15584">#15584</a></th>
<td>nonVoid is too conservative w.r.t. strict argument types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15884">#15884</a></th>
<td>Completeness of View Patterns With a Complete Set of Output Patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15886">#15886</a></th>
<td>Spurious warning about incomplete pattern with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16129">#16129</a></th>
<td>Incorrect non-exhaustive pattern warning with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16377">#16377</a></th>
<td>`TypeError` in a pattern should flag inaccessible code</td></tr></table>




**Background**:

- The paper on which the previous approach was based [Two techniques for compiling lazy pattern matching](http://moscova.inria.fr/~maranget/papers/lazy-pats-derniere.ps.gz)
- Peter Sestoft's paper for negative patterns [ML's pattern matching compilation and partial evaluation](http://lambda.csail.mit.edu/~chet/papers/others/s/sestoft/sestoft96ml.pdf)

**Our solution**

- Our paper, describing the algorithm we implemented [GADTs meet their match](http://people.cs.kuleuven.be/~george.karachalias/papers/p424-karachalias.pdf).
- [PatternMatchCheckImplementation](pattern-match-check-implementation) talks about the implementation in GHC.

**Related tickets** (ones that are closed are still useful examples in the wild; they were only closed as duplicates):

- [\#29](https://gitlab.haskell.org/ghc/ghc/issues/29)
- [\#322](https://gitlab.haskell.org/ghc/ghc/issues/322)
- [\#366](https://gitlab.haskell.org/ghc/ghc/issues/366)
- [\#595](https://gitlab.haskell.org/ghc/ghc/issues/595)
- [\#851](https://gitlab.haskell.org/ghc/ghc/issues/851)
- [\#1307](https://gitlab.haskell.org/ghc/ghc/issues/1307)
- [\#2006](https://gitlab.haskell.org/ghc/ghc/issues/2006)
- [\#2204](https://gitlab.haskell.org/ghc/ghc/issues/2204)
- [\#3078](https://gitlab.haskell.org/ghc/ghc/issues/3078)
- [\#3927](https://gitlab.haskell.org/ghc/ghc/issues/3927)
- [\#4139](https://gitlab.haskell.org/ghc/ghc/issues/4139)
- [\#5724](https://gitlab.haskell.org/ghc/ghc/issues/5724)
- [\#5728](https://gitlab.haskell.org/ghc/ghc/issues/5728)
- [\#5762](https://gitlab.haskell.org/ghc/ghc/issues/5762)
- [\#6124](https://gitlab.haskell.org/ghc/ghc/issues/6124)
- [\#8016](https://gitlab.haskell.org/ghc/ghc/issues/8016)
- [\#8494](https://gitlab.haskell.org/ghc/ghc/issues/8494)
- [\#8779](https://gitlab.haskell.org/ghc/ghc/issues/8779)
- [\#8853](https://gitlab.haskell.org/ghc/ghc/issues/8853)
- [\#8970](https://gitlab.haskell.org/ghc/ghc/issues/8970)
- [\#9113](https://gitlab.haskell.org/ghc/ghc/issues/9113)
- [\#9951](https://gitlab.haskell.org/ghc/ghc/issues/9951)
- [\#10116](https://gitlab.haskell.org/ghc/ghc/issues/10116)
- [\#10600](https://gitlab.haskell.org/ghc/ghc/issues/10600)

# The main problem we wish to solve


Since GHC's exhaustiveness/redundancy checker was outdated, it did not take into account constraints introduced
by GADT matches when reporting warnings. This is illustrated in the following example ([\#3927](https://gitlab.haskell.org/ghc/ghc/issues/3927)):


```
data T a where
  T1 :: T Int
  T2 :: T Bool

f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
```


Even though the above definition for `f` is exhaustive, we get a warning of the form:

```wiki
    Pattern match(es) are non-exhaustive
    In an equation for `f':
        Patterns not matched:
            T1 T2
            T2 T1
```


Obviously, both pattern vectors issued as not matched, are ill-typed, because they both generate the inconsistent
constraint `Int ~ Bool`. This becomes more clear if we rewrite the definition of `T` in the equivalent form:


```
data T a where
  T1 :: forall a. (a ~ Int)  => T a
  T2 :: forall a. (a ~ Bool) => T a
```


Additionally, if we add one more branch to `f`:


```
f :: T a -> T a -> Bool
f T1 T1 = True
f T2 T2 = False
f _  _  = undefined -- inaccessible
```


we get no warning about the redundancy of the last clause.

### Two more problems


It has not been only GADTs that have not been handled by the coverage/exhaustiveness checker. More exotic pattern
matching features like pattern guards, view patterns, overloaded literals etc. usually made the pattern match
checker give imprecise warnings because there was no systematic way to treat them. Additionally, laziness has been
fully taken into account by the previous checker. The new pattern match checker addresses these problems too.

# General approach


Note that improving the redundancy check is quite more challenging than the exhaustiveness check. For
exhaustiveness it is sufficient to collect all potentially missing vectors using the previous syntax-based approach
and then filter out the ill-typed. Nevertheless, for redundancy we need to compute the cases that are covered by
every clause and then filter out the ill-typed. The difficulty lies in the fact that what the last branch of `f`
covers depends on what remains uncovered by the above two clauses. This indicates that for the redundancy check we
need an incremental way of computing uncovered vectors.

## The solution


Until now, the algorithm used by GHC was based on a technique originally introduced for compilation of pattern
matching in decision trees (see paper above). Hence, it used a column-based approach to traverse the pattern
matrix, which cannot be used incrementally as we described above. (By pattern matrix we mean a list of matches.
Since every clause has the same length, --let's call say `m`-- a match of `n` clauses, each of length `m` defines
a pattern matrix of dimensions `n x m`. For more details see the paper [Two techniques for compiling lazy pattern matching](http://moscova.inria.fr/~maranget/papers/lazy-pats-derniere.ps.gz)).


Additionally, this pattern matrix approach assumes that each clause has the same length, which is rather restrictive
since we may have arbitrary guards accompanying a pattern vector (clauses may have different lengths).


Instead, we traverse the clauses line-by-line (one-by-one, from left to right, top-down), following the semantics
of the source language.
The general approach is the following:
We assume that initially nothing is covered (everything is uncovered) and the, for every clause we compute:

- Which cases it covers
- If it forces the evaluation of arguments (see Laziness below)
- Which cases are left unhandled


There are two major differences with previous approaches:

- We use a core pattern syntax that allows guards to appear **anywhere** in a clause. As we have shown in the
  paper, in this expressive pattern language we can encode all source features (and even more).
- The sets of covered/uncovered and divergent cases we mention above do not contain pattern vectors. Instead,
  they contain vectors accompanied by a set of type and term constraints. We illustrate:


For example, for function `f` above we have:

- initial_missing = `[(x y)]` where `x :: T a` and `y :: T a`

  ```
  f T1 T1 = True -- first clause
  ```
- Covers `[(T1 T1 |> {a ~ Int, a ~ Int})]`
- Forces the evaluation of the 1st argument
- If 1st argument is `T1` forces the evaluation of the 2nd argument
- Remain uncovered:

  ```wiki
  [ (T2 y  |> {a ~ Bool})
  , (T1 T2 |> {a ~ Int, a ~ Bool) ]
  ```


The main idea is that every vector we generate comes with a set of constraints that have to be satisfied. This set
of constraints is the ` |> {...}`-part of every vector. Now it becomes apparent that case
`(T1 T2 |> {a ~ Int, a ~ Bool)` can be removed. The type constraints it contains
(`a ~ Int, a ~ Bool`) is not satisfiable (which in turn means that it represents an inaccessible case).
Removing the second vector gives us:

```wiki
uncovered = [(T2 y  |> {a ~ Bool})]
```


With this, we now process the second clause:


```
f T2 T2 = False -- second clause
```

- Covers `[(T2 T2 |> {a ~ Bool, a ~ Bool})]`
- If 1st argument is `T2` forces the evaluation of the 2nd argument
- Remain uncovered `[(T2 T1 |> {a ~ Bool, a ~ Int})]`


Same as before, the uncovered set contains non satisfiable constraints, so we end up with

```wiki
uncovered = []
```


Finally, checking the last clause given that nothing is uncovered we get: 


```
f _  _  = undefined -- third clause (inaccessible)
```

- Covers: `[]`
- Doesn't force anything
- Remain uncovered: `[]`


Which means that we rightly now detect the last clause as redundant. Also note that the uncovered set was empty
after the first clauses, which means that the first two clauses make `f` exhaustive and the bogus warnings go
away.


Although not illustrated here, we check for satisfiability both the covered and the uncovered sets of clauses.
Finally, note that in this example we showed only the type constraints, for simplicity. We also store and check
term constraints so that we can handle guards (see section below).

# Pattern Guards



A pattern guard (`p <- e`) behaves exactly like if we had one more argument to pattern match against. For example:


```
f x :: [a] -> Bool
f x | (z:zs) <- x = ...
```


is equivalent to:


```
g x :: [a] -> [a] -> Bool
g x (z:zs) = ...

f :: [a] -> Bool
f x = g x x
```


Using this trick, we can reason about pattern guards in the same way we reason about normal pattern matching.
Consider the following example:


```
-- the above example, written using guards
f :: T a -> T a -> Bool
f c d | T1 <- c, T1 <- d = True
      | T2 <- c, T2 <- d = False
      | otherwise        = undefined -- inaccessible
```


Again, we have:

- initial_missing = `[(x y)]` where `x :: T a` and `y :: T a`

- 1st Clause: Covered (z-variables represent the **fake** arguments we match against):

  ```wiki
  [(x y |> { x ~ c, z1 ~ c, c ~ T1, a ~ Int
           , y ~ d, z2 ~ d, d ~ T1, a ~ Int })]
  ```

  Solving the constraints gives us a solution: { x = c = z1 = T1, y = d = z2 = T2 } which we can use to
  simplify the clause `(x y)` to `(T1 T1)` (exactly like we had in the non-guard example).

- 1st Clause: Uncovered:

  ```wiki
  [(x y |> { x ~ c, z1 ~ c, c ~ T2, a ~ Bool
           , x ~ c, z1 ~ c, c ~ T1, a ~ Int, y ~ d, z2 ~ d, d ~ T2, a ~ Bool })]
  ```

  Again, the second uncovered vector has non-satisfiable constraints and can be removed. Solving the constraints
  for the first uncovered vector gives us a solution: { x = c = z1 = T2 }. Using this, we can again simplify the
  clause `(x y)` to `(T2 y)`, and we end up with exactly the same state we had in the non-guard example.

- etc.


Obviously, pattern guards generate more constraints while the actual patterns of the clause are unaffected
(until we solve and substitute back our findings like we did in the above example). Hence, the expressivity of the
checker concerning guards heavily relies on the expressive power of the term oracle. Similarly, the expressivity
concerning guards totally relies on OutsideIn(X), GHC's inference engine, which we use to check for satisfiability
of type constraints.

# Laziness


The previous algorithm was not exactly laziness-aware. The two examples below illustrate some (rather pathological)
cases:

### Example 1



Consider the following function:


```
g :: Bool -> Bool -> Bool
g _    True = True
g True True = True
g _    _    = False
```


The old checker emits the following warning:

```wiki
    Pattern match(es) are overlapped
    In an equation for `g': g True True = ...
```


Even though this is correct (indeed the second clause is totally overlapped by the first clause), the above warning
until now was used to mean that the respective clause is redundant, which is not true in this case.
If we call `g` we get:

```wiki
ghci> g undefined False
*** Exception: Prelude.undefined
```


because the second clause forces the evaluation of the first argument. Yet, if we remove it we get instead:

```wiki
ghci> g undefined False
False
```


That is, we changed the semantics of `g`. What happens is that all cases that match the second clause match the
first too, which in turn means that the second clause has an inaccessible right-hand-side. Yet, pattern matching in
a lazy language like Haskell is not only used for matching but also for **evaluating**. The second clause is useful,
not for matching, but for evaluating the first argument of `g`. With our new checker these cases can be easily
detected: The covered set for the second clause is empty (it is overlapped from the above equations) but we detect
that it evaluates something. In principle, we follow the following reasoning:

- Covers = Yes, Forces = Yes ==\> Useful clause
- Covers = Yes, Forces = No  ==\> Useful clause
- Covers = No,  Forces = Yes ==\> Clause with inaccessible RHS
- Covers = No,  Forces = No  ==\> Redundant clause


By "useful" we mean that the RHS will be returned if matched.

### Example 2



Interestingly, one can trigger this behaviour indirectly, using GADTs. Consider the function `h`:


```
data F a where
  F1 :: F Int
  F2 :: F Bool

data G a where
  G1 :: G Int
  G2 :: G Char

h :: F a -> G a -> Int
h F1 G1 = 1
h _  G1 = 2 -- what about this clause?
```


Running the check on the first clause leaves us with an uncovered set:

```wiki
uncovered = [ (F2 y  |> {a ~ Bool})
            , (F1 G2 |> {a ~ Int, a ~ Char}) ]
```


and by dropping the second (has non-satisfiable type constraints) we end up with:

```wiki
uncovered = [ (F2 y  |> {a ~ Bool}) ]
```


About the second clause:

- Both the uncovered vector `(F2 y)` and the constraints make clear that the second argument is not evaluated yet.
  By matching with the second clause `(_ G1)` we certainly force the evaluation of the second argument.

- The covered set is:

  ```wiki
  covered = [ (F2 G1  |> {a ~ Bool, a ~Int}) ]
  ```


and because `{a ~ Bool, a ~Int}` is non-satisfiable, the covered set is empty. Hence, even though `h` can never
return a `2`, the second clause is not actually redundant.


Note that this case is a bit different from the one above: If we consider the type ofemits warning that the RHS is
inaccessible `h`, indeed the only well-typed **values** that can match are `F1` and `G1`. But since Haskell is not
call-by-value, `F2 undefined` is a perfectly well-typed combination of arguments! Hence, this example illustrates
the following interesting behaviour:


```
h1 :: F a -> G a -> Int
h1 F1 G1 = 1
h1 _  G1 = 2 -- emits warning that the RHS is inaccessible
             -- but h1 is considered exhaustive

h2 :: F a -> G a -> Int
h2 F1 G1 = 1
  -- emits a warning that h2 is non-exhaustive with (F2 _) being the missing case

h3 :: F a -> G a -> Int
h3 F1 G1 = 1
h3 F2 _  = 2
  -- no warning for this one
```