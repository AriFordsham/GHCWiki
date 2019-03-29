# Deriving instances


All about the `deriving` keyword.

# Status


Use **Keyword** = `deriving` (or `deriving-perf` for performance-related tickets) to ensure that a ticket ends up on these lists.



Open Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1544">#1544</a></th>
<td>Derived Read instances for recursive datatypes with infix constructors are too inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3205">#3205</a></th>
<td>Generalized isomorphic deriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5041">#5041</a></th>
<td>Incorrect Read deriving for MagicHash constructors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5642">#5642</a></th>
<td>Deriving Generic of a big type takes a long time and lots of space</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7258">#7258</a></th>
<td>Compiling DynFlags is jolly slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8731">#8731</a></th>
<td>long compilation time for module with large data type and partial record selectors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8827">#8827</a></th>
<td>Inferring Safe mode with GeneralizedNewtypeDeriving is wrong</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9112">#9112</a></th>
<td>support for deriving Vector/MVector instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9450">#9450</a></th>
<td>GHC instantiates Data instances before checking hs-boot files</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9522">#9522</a></th>
<td>SPECIALISE pragmas for derived instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9557">#9557</a></th>
<td>Deriving instances is slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9669">#9669</a></th>
<td>Long compile time/high memory usage for modules with many deriving clauses</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9790">#9790</a></th>
<td>Produce coercion rules for derived Functor instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10980">#10980</a></th>
<td>Deriving Read instance from datatype with N fields leads to N^2 code size growth</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11008">#11008</a></th>
<td>Difficulties around inferring exotic contexts</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12457">#12457</a></th>
<td>Deriving should be (more closely) integrated with other metaprogramming methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12639">#12639</a></th>
<td>Inconsistent treatment of FlexibleInstances and MPTCs with standard vs. flexible deriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12860">#12860</a></th>
<td>GeneralizedNewtypeDeriving + MultiParamTypeClasses sends typechecker into an infinite loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13154">#13154</a></th>
<td>Standalone-derived anyclass instances aren&apos;t as permissive as empty instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13280">#13280</a></th>
<td>Consider deriving more Foldable methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13327">#13327</a></th>
<td>Figure out how to make sense of Data instances for poly-kinded datatypes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13403">#13403</a></th>
<td>Derive instances (Applicative, Monad, ...) for structures lifted over functors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13465">#13465</a></th>
<td>Foldable deriving treatment of tuples is too surprising</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13731">#13731</a></th>
<td>DeriveFunctor and friends don&apos;t understand type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13748">#13748</a></th>
<td>Variables pretty-printed from -ddump-deriv are not scoped properly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13957">#13957</a></th>
<td>Allow deriving multiparameter type classes with representationally equal arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14030">#14030</a></th>
<td>Implement the &quot;Derive Lift instances for data types in template-haskell&quot; proposal</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14331">#14331</a></th>
<td>Overzealous free-floating kind check causes deriving clause to be rejected</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14332">#14332</a></th>
<td>Deriving clauses can have forall types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15151">#15151</a></th>
<td>Better Interaction Between Specialization and GND</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15376">#15376</a></th>
<td>GHC determine illegal kind for standalone deriving with Deriving via</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15434">#15434</a></th>
<td>DerivingVia (and perhaps even GND) works badly with DeriveGeneric</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15650">#15650</a></th>
<td>Add (or document if already exist) ability to derive custom typeclasses via source plugins</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15712">#15712</a></th>
<td>GHC panic with -XDerivingVia</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15831">#15831</a></th>
<td>DerivingVia allows bogus implicit quantification in `via` type</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15839">#15839</a></th>
<td>DerivingStrategies defaulting warning has no associated enable/suppress flag</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15868">#15868</a></th>
<td>Standard deriving should be less conservative when `UndecidableInstances` is enabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15969">#15969</a></th>
<td>Generic1 deriving should use more coercions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16181">#16181</a></th>
<td>ghc panic when using DerivingVia</td></tr></table>




Closed Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1356">#1356</a></th>
<td>&quot;derive instance&quot; panics ghc-6.7.20070404</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1830">#1830</a></th>
<td>Automatic derivation of Lift</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2721">#2721</a></th>
<td>Newtype deriving doesn&apos;t work with type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2733">#2733</a></th>
<td>Newtype deriving over phantom types broke between GHC 6.6.1 and GHC 6.8.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2734">#2734</a></th>
<td>Newtype deriving over phantom types broke between GHC 6.6.1 and GHC 6.8.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3368">#3368</a></th>
<td>Deriving Foldable doesn&apos;t work</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3422">#3422</a></th>
<td>No match in record selector Var.tcTyVarDetails</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4019">#4019</a></th>
<td>deriving Ord can produce incorrect and inefficient instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4028">#4028</a></th>
<td>Derived Data instance requires Data instances for unused type parameters</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4235">#4235</a></th>
<td>deriving Enum fails for data instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4309">#4309</a></th>
<td>Painfully large errors with silly GADT instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4528">#4528</a></th>
<td>stand-alone deriving sometimes fails for GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4529">#4529</a></th>
<td>Deriving Data does not work for attached code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4530">#4530</a></th>
<td>Deriving Data for existentially quantified types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4815">#4815</a></th>
<td>Instance constraints should be used when deriving on associated data types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4896">#4896</a></th>
<td>Deriving Data does not work for attached code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5007">#5007</a></th>
<td>&quot;deriving&quot; seems to ignore class context for a type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7199">#7199</a></th>
<td>Standalone deriving Show at GHCi prompt causes divergence when printing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7401">#7401</a></th>
<td>Can&apos;t derive instance for Eq when datatype has no constructor, while it is trivial do do so.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7436">#7436</a></th>
<td>Derived Foldable and Traversable instances become extremely inefficient due to eta-expansion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7742">#7742</a></th>
<td>StandaloneDeriving on Read fails for GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8128">#8128</a></th>
<td>Standalone deriving fails for GADTs due to inaccessible code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8165">#8165</a></th>
<td>Use GeneralizedNewtypeDeriving to automatically create associated type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8263">#8263</a></th>
<td>allow duplicate deriving / standalone deriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8631">#8631</a></th>
<td>Need ImpredicativeTypes for GeneralizedNewtypeDeriving?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8740">#8740</a></th>
<td>Deriving instance conditionally compiles</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9444">#9444</a></th>
<td>-ddump-deriv doesn&apos;t dump failed newtype-deriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9630">#9630</a></th>
<td>compile-time performance regression (probably due to Generics)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9850">#9850</a></th>
<td>Template Haskell does not seem to support StandaloneDeriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10361">#10361</a></th>
<td>DeriveAnyClass does not fill in associated type defaults</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10447">#10447</a></th>
<td>DeriveFoldable rejects instances with constraints in last argument of data type</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10524">#10524</a></th>
<td>PolyKinds doesn&apos;t interact well with DeriveFunctor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10561">#10561</a></th>
<td>&quot;deriving (Functor)&quot; on a polykinded type produces ill-kinded instance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10577">#10577</a></th>
<td>Use empty cases where appropriate when deriving instances for empty types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10598">#10598</a></th>
<td>DeriveAnyClass and GND don&apos;t work well together</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10607">#10607</a></th>
<td>Auto derive from top to bottom</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10835">#10835</a></th>
<td>Regression in standalone Data deriving for phantom types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10858">#10858</a></th>
<td>Smaller generated Ord instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10859">#10859</a></th>
<td>Generated Eq instance associates &amp;&amp; wrongly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10938">#10938</a></th>
<td>DeriveAnyClass + deriving Bifunctor causes GHC panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11174">#11174</a></th>
<td>Traversable can&apos;t be derived for datatypes with unboxed arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11396">#11396</a></th>
<td>deriving Ix with custom ifThenElse causes &quot;Bad call to tagToEnum#&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11509">#11509</a></th>
<td>Incorrect error message in StandaloneDeriving: &quot;The data constructors of &lt;typeclass&gt; are not all in scope&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11837">#11837</a></th>
<td>GHC fails to unify kinds when deriving polykinded typeclass instance for polykinded newtype</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12047">#12047</a></th>
<td>Users Guide: GeneralizedNewtypeDeriving derives “instance Num Int =&gt; Num Dollars”</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12080">#12080</a></th>
<td>RebindableSyntax breaks deriving Ord</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12234">#12234</a></th>
<td>&apos;deriving Eq&apos; on recursive datatype makes ghc eat a lot of CPU and RAM</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12399">#12399</a></th>
<td>DeriveFunctor fail</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12438">#12438</a></th>
<td>DeriveDataTypeable - deriving instance Data (Mu (Const ()))</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12439">#12439</a></th>
<td>DeriveDataTypeable - deriving Data for several type constructor applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12616">#12616</a></th>
<td>type synonyms confuse generalized newtype deriving role checking</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12688">#12688</a></th>
<td>derived Show instances aren&apos;t protected from RebindableSyntax+OverloadedStrings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12768">#12768</a></th>
<td>8.0.2 derives invalid code when class method is constrained by itself</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12814">#12814</a></th>
<td>Should GND infer an instance context when deriving method-free classes?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13056">#13056</a></th>
<td>Deriving Foldable causes GHC to take a long time (GHC 8.0 ONLY)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13117">#13117</a></th>
<td>Derived functor instance for void types handles errors badly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13175">#13175</a></th>
<td>Documenting what can be derived &apos;out of the box&apos; by GHC&apos;s &quot;deriving&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13218">#13218</a></th>
<td>&lt;$ is bad in derived functor instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13263">#13263</a></th>
<td>cant derive functor on function newtype with unboxed tuple result</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13272">#13272</a></th>
<td>DeriveAnyClass regression involving a rigid type variable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13314">#13314</a></th>
<td>StandaloneDeriving and DeriveAnyClass don&apos;t work together</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13324">#13324</a></th>
<td>Allow PartialTypeSignatures in the instance context of a standalone deriving declaration</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13328">#13328</a></th>
<td>Foldable, Functor, and Traversable deriving handle phantom types badly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13759">#13759</a></th>
<td>Strange error message for deriving Data</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14070">#14070</a></th>
<td>Allow ‘unsafe’ deriving strategy, deriving code with ‘unsafeCoerce’</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14094">#14094</a></th>
<td>DeriveAnyClass doesn&apos;t warn about unimplemented type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14339">#14339</a></th>
<td>GHC 8.2.1 regression when combining GND with TypeError (solveDerivEqns: probable loop)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14357">#14357</a></th>
<td>Document deriving strategies fully</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14364">#14364</a></th>
<td>Reduce repetition in derived Read instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14365">#14365</a></th>
<td>Panic with (bogus?) deriving in hs-boot: newTyConEtadArity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14462">#14462</a></th>
<td>deriving on associated data types fails to find constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14578">#14578</a></th>
<td>Incorrect parenthesization of types in -ddump-deriv</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14579">#14579</a></th>
<td>GeneralizedNewtypeDeriving produces ambiguously-kinded code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14661">#14661</a></th>
<td>Cannot derive (newtype I a b = I (F a -&gt; F b) deriving newtype Category) for type family F</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14682">#14682</a></th>
<td>Atrocious parenthesization in -ddump-deriv output</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14692">#14692</a></th>
<td>Deriving Show with -XEmptyDataDeriving cases on the wrong argument</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14728">#14728</a></th>
<td>Is (GeneralizedNewtypeDeriving + associated type classes) completely bogus?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14748">#14748</a></th>
<td>Infer context for Data instance of (data Foo f = Foo (f Bool) (f Int))</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14916">#14916</a></th>
<td>Missing checks when deriving special classes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14918">#14918</a></th>
<td>GHC 8.4.1 regression: derived Read instances with field names containing # no longer parse</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14932">#14932</a></th>
<td>DeriveAnyClass produces unjustifiably untouchable unification variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14933">#14933</a></th>
<td>DeriveAnyClass can cause &quot;No skolem info&quot; GHC panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15012">#15012</a></th>
<td>&quot;Iface type variable out of scope&quot; when compiling with -c</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15073">#15073</a></th>
<td>Unable to newtype derive `Prim` via DerivingStrategies</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15178">#15178</a></th>
<td>Implement DerivingVia</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15398">#15398</a></th>
<td>GADT deriving Ord generates inaccessible code in a pattern with constructor.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15637">#15637</a></th>
<td>Ambiguous type variables in GeneralisedNewtypeDeriving</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15798">#15798</a></th>
<td>Flag to warn when deriving strategy is not explicitly specified</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16179">#16179</a></th>
<td>Mention DerivingStrategies in the warning when DeriveAnyClass and GeneralizedNewtypeDeriving are both enabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16194">#16194</a></th>
<td>deriving, wrong code: newtype T cat a = MkT ((forall xx. cat xx xx) -&gt; a) deriving stock Functor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16323">#16323</a></th>
<td>Cannot deduce X error with X provided in TypeFamilies</td></tr></table>



