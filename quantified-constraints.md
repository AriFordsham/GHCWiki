# Quantified constraints


This wiki page summarises the state of play on the idea of allowing quantification in class constraints.  For example

```wiki
data Rose f a = Branch a (f (Rose f a))

instance (Eq a, forall b. (Eq b) => Eq (f b))
       => Eq (Rose f a)
  where ...
```


The new bit is the `forall` in the context of the instance declaration. This is allowed in GHC 8.6 and later using the `QuantifiedConstraints` extension.


Here are some resources

- The [GHC Proposal discussing quantified constraints](https://github.com/ghc-proposals/ghc-proposals/pull/109)

- [Derivable type classes](https://www.microsoft.com/en-us/research/publication/derivable-type-classes), Section 7, where the idea was first proposed (I think).

- #2893, a ticket about the idea

- [Quantified class constraints](http://i.cs.hku.hk/~bruno//papers/hs2017.pdf), a Haskell 2017 paper that works out the idea in some detail, and a [ Reddit thread](https://www.reddit.com/r/haskell/comments/6me3sv/quantified_class_constraints_pdf/) about it.

- [An old haskell.org Wiki page about it](http://haskell.org/haskellwiki/Quantified_contexts)
- [A Libraries thread (Dec 18)](https://mail.haskell.org/pipermail/libraries/2017-December/028377.html).

## Status


Use Keyword = `QuantifiedConstraints` to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8516">#8516</a></th>
<td>Add (-&gt;) representation and the Invariant class to GHC.Generics</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13153">#13153</a></th>
<td>Several Traversable instances have an extra fmap</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14317">#14317</a></th>
<td>Solve Coercible constraints over type constructors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14831">#14831</a></th>
<td>QuantifiedConstraints: Odd superclass constraint</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14832">#14832</a></th>
<td>QuantifiedConstraints: Adding to the context causes failure</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14860">#14860</a></th>
<td>QuantifiedConstraints: Can&apos;t quantify constraint involving type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14877">#14877</a></th>
<td>QuantifiedConstraints: Can&apos;t deduce `xx&apos; from `(xx =&gt; a, xx)&apos;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14879">#14879</a></th>
<td>QuantifiedConstraints: Big error message + can&apos;t substitute (=&gt;) with a class alias</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14896">#14896</a></th>
<td>QuantifiedConstraints: GHC does doesn&apos;t discharge constraints if they are quantified</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14937">#14937</a></th>
<td>QuantifiedConstraints: Reify implication constraints from terms lacking them</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14943">#14943</a></th>
<td>Make (=&gt;) polykinded (:: k -&gt; k -&gt; Constraint)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14958">#14958</a></th>
<td>QuantifiedConstraints: Doesn&apos;t apply implication for existential?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14968">#14968</a></th>
<td>QuantifiedConstraints: Can&apos;t be RHS of type family instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14983">#14983</a></th>
<td>Have custom type errors imply Void</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14995">#14995</a></th>
<td>QuantifiedConstraints: Incorrect pretty printing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15347">#15347</a></th>
<td>QuantifiedConstraints: Implication constraints with type families don&apos;t work</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15351">#15351</a></th>
<td>QuantifiedConstraints ignore FunctionalDependencies</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15409">#15409</a></th>
<td>Quantified constraints half-work with equality constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15639">#15639</a></th>
<td>Surprising failure combining QuantifiedConstraints with Coercible</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15888">#15888</a></th>
<td>Quantified constraints can be loopy</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16139">#16139</a></th>
<td>GHC confused about type synonym kind with QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16140">#16140</a></th>
<td>Cannot create type synonym for quantified constraint without ImpredicativeTypes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16173">#16173</a></th>
<td>Move `Data.Profunctor` from `profunctors` package to `base`</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16245">#16245</a></th>
<td>GHC panic (No skolem info) with QuantifiedConstraints and strange scoping</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16365">#16365</a></th>
<td>Inconsistency in quantified constraint solving</td></tr></table>




Closed Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2256">#2256</a></th>
<td>Incompleteness of type inference: must quantify over implication constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2893">#2893</a></th>
<td>Implement &quot;Quantified constraints&quot; proposal</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5927">#5927</a></th>
<td>A type-level &quot;implies&quot; constraint on Constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9123">#9123</a></th>
<td>Emit quantified Coercible constraints in GeneralizedNewtypeDeriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12245">#12245</a></th>
<td>Deriving Data at higher kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14070">#14070</a></th>
<td>Allow ‘unsafe’ deriving strategy, deriving code with ‘unsafeCoerce’</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14733">#14733</a></th>
<td>Won&apos;t use (forall xx. f xx) with -XQuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14734">#14734</a></th>
<td>QuantifiedConstraints conflated with impredicative polymorphism?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14735">#14735</a></th>
<td>GHC Panic with QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14744">#14744</a></th>
<td>Non-exhaustive patterns in case in GHCi with quantified class contexts</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14748">#14748</a></th>
<td>Infer context for Data instance of (data Foo f = Foo (f Bool) (f Int))</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14799">#14799</a></th>
<td>QuantifiedConstraints: Problems with Typeable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14822">#14822</a></th>
<td>-XQuantifiedConstraints: Turn term-level entailments (:-) into constraints (=&gt;)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14833">#14833</a></th>
<td>QuantifiedConstraints: GHC can&apos;t deduce (() :: Constraint)?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14835">#14835</a></th>
<td>QuantifiedConstraints: Can&apos;t deduce &quot;(a, b)&quot; from &quot;a&quot; and &quot;b&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14840">#14840</a></th>
<td>QuantifiedConstraints: Can&apos;t define class alias</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14861">#14861</a></th>
<td>QuantifiedConstraints: Can&apos;t use forall&apos;d variable in context</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14863">#14863</a></th>
<td>QuantifiedConstraints: Can&apos;t deduce `c&apos; from `(a, b)&apos; and `a |- b |- c&apos;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14878">#14878</a></th>
<td>Can&apos;t witness transitivity ((.)) of isomorphism of Constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14883">#14883</a></th>
<td>QuantifiedConstraints don&apos;t kick in when used in TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14897">#14897</a></th>
<td>QuantifiedConstraints: Can&apos;t print type of quantified constraint</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14942">#14942</a></th>
<td>QuantifiedConstraints: GHC can&apos;t infer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14961">#14961</a></th>
<td>QuantifiedConstraints: introducing classes through equality constraints fails</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14993">#14993</a></th>
<td>QuantifiedConstraints and principal types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15008">#15008</a></th>
<td>Type synonyms with hidden, determined type variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15231">#15231</a></th>
<td>UndecidableInstances validity checking is wrong in the presence of QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15244">#15244</a></th>
<td>Ambiguity checks in QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15290">#15290</a></th>
<td>QuantifiedConstraints: panic &quot;addTcEvBind NoEvBindsVar&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15316">#15316</a></th>
<td>Regarding coherence and implication loops in presence of QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15334">#15334</a></th>
<td>(forall x. c x, forall x. d x) is not equivalent to forall x. (c x, d x)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15359">#15359</a></th>
<td>Quantified constraints do not work with equality constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15507">#15507</a></th>
<td>Deriving with QuantifiedConstraints is unable to penetrate type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15593">#15593</a></th>
<td>QuantifiedConstraints: trouble with type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15625">#15625</a></th>
<td>GHC panic, with QuantifiedConstraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15635">#15635</a></th>
<td>Implication introduction for quantified constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15636">#15636</a></th>
<td>Implication constraint priority breaks default class implementations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15943">#15943</a></th>
<td>&quot;ASSERT failed&quot; with quantified constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15974">#15974</a></th>
<td>QuantifiedConstraints: Spurious error involving superclass constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16123">#16123</a></th>
<td>QuantifiedConstraints fails to deduce trivial constraint</td></tr></table>



