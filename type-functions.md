# Type Functions, Type Families, and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type families (aka type functions) and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.


See the Haskell Wiki for [ user-level documentation](http://haskell.org/haskellwiki/GHC/Indexed_types).

## Status


Detailed information about implemented and unimplemented features as well as bugs and plans for further improvements is at [implementation status](type-functions-status).  The following provides a summary:


Implemented features:

- All basic functionality of open data type families, open type synonym families, and equality constraints has been implemented.
- Type checking is fully integrated with GADTs.
- Type family instances can have ordered groups of equations. See [NewAxioms](new-axioms).


Missing features:

- Superclass equalities.
- Data family instances in GADT form.
- Re-implement functional dependencies using explicit equalities.


Speculative ideas:

- [Total type families.](type-functions/total-families)
- Closed synonym families.
- [Class families.](type-functions/class-families)
- Our type-indexed data types are open.  However, we currently don't allow case expressions mixing constructors from different indexes.  We could do that if we had a story for open function definitions outside of classes.  Class instances of entire data families (including `deriving` clauses at family declarations to derive for all instances) requires the same sort of capabilities as case expressions mixing data constructors from different indexes.  This is, as they require to build a dictionary that applies to all family instances (as opposed to a distinct dictionary per instance, which is what we have now).

## Tickets



Use Keyword = `TypeFamilies` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/4259">#4259</a></th>
<td>Relax restrictions on type family instance overlap</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5224">#5224</a></th>
<td>Improve consistency checking for family instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7102">#7102</a></th>
<td>Type family instance overlap accepted in ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7808">#7808</a></th>
<td>data families and TH names do not mix well (e.g. cannot use TH deriving)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8095">#8095</a></th>
<td>TypeFamilies painfully slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8109">#8109</a></th>
<td>Type family patterns should support as-patterns.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8177">#8177</a></th>
<td>Roles for type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8423">#8423</a></th>
<td>Less conservative compatibility check for closed type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8441">#8441</a></th>
<td>Allow family instances in an hs-boot file</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9269">#9269</a></th>
<td>Type families returning quantified types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9376">#9376</a></th>
<td>More informative error messages when closed type families fail to simplify</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9394">#9394</a></th>
<td>Show data/type family instances with ghci&apos;s :info command</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9429">#9429</a></th>
<td>Alternative to type family Any</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9562">#9562</a></th>
<td>Type families + hs-boot files = unsafeCoerce</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9587">#9587</a></th>
<td>Type checking with type functions introduces many type variables, which remain ambiguous. The code no longer type checks.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9607">#9607</a></th>
<td>Programs that require AllowAmbiguousTypes in 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9667">#9667</a></th>
<td>Type inference is weaker for GADT than analogous Data Family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9780">#9780</a></th>
<td>dep_orphs in Dependencies redundantly records type family orphans</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9898">#9898</a></th>
<td>Wanted: higher-order type-level programming</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9918">#9918</a></th>
<td>GHC chooses an instance between two overlapping, but cannot resolve a clause within the similar closed type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10116">#10116</a></th>
<td>Closed type families: Warn if it doesn&apos;t handle all cases</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10141">#10141</a></th>
<td>CUSK mysteries</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10204">#10204</a></th>
<td>Odd interaction between rank-2 types and type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10327">#10327</a></th>
<td>Devise workaround for how infinite types prevent closed type family reduction</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10482">#10482</a></th>
<td>Not enough unboxing happens on data-family function argument</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10789">#10789</a></th>
<td>Notify user when a kind mismatch holds up a type family reduction</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10808">#10808</a></th>
<td>Odd interaction between record update and type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10832">#10832</a></th>
<td>Generalize injective type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10833">#10833</a></th>
<td>Use injective type families (decomposition) when dealing with givens</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10996">#10996</a></th>
<td>family is treated as keyword in types even without TypeFamilies enabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11070">#11070</a></th>
<td>Type-level arithmetic of sized-types has weaker inference power than in 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11084">#11084</a></th>
<td>Some type families don&apos;t reduce with :kind!</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11113">#11113</a></th>
<td>Type family If is too strict</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11207">#11207</a></th>
<td>GHC cannot infer injectivity on type family operating on GHC.TypeLits&apos; Nat, but can for equivalent type family operating on user-defined Nat kind</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11243">#11243</a></th>
<td>Flag to not expand type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11310">#11310</a></th>
<td>Surprising accepted constructor for GADT instance of data family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11424">#11424</a></th>
<td>&quot;Occurs check&quot; not considered when reducing closed type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11511">#11511</a></th>
<td>Type family producing infinite type accepted as injective</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11534">#11534</a></th>
<td>Allow class associated types to reference functional dependencies</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12551">#12551</a></th>
<td>Make type indices take local constraints into account in type instance declaration</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12564">#12564</a></th>
<td>Type family in type pattern kind</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13192">#13192</a></th>
<td>Ambiguity Caused By PolyKind and Not Helpful Error Messages</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13251">#13251</a></th>
<td>Must perform family consistency check on non-imported identifiers</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13386">#13386</a></th>
<td>Poor compiler performance with type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13571">#13571</a></th>
<td>Injective type family syntax accepted without TypeFamilyDependencies</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13971">#13971</a></th>
<td>Misleading &quot;Kind mis-match on LHS of default declaration&quot; error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14040">#14040</a></th>
<td>Typed holes regression in GHC 8.0.2: No skolem info: z_a1sY[sk:2]</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14111">#14111</a></th>
<td>strange error when using data families with levity polymorphism and unboxed sums and data families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14279">#14279</a></th>
<td>Type families interfere with specialisation rewrite rules</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14319">#14319</a></th>
<td>Stuck type families can lead to lousy error messages</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14420">#14420</a></th>
<td>Data families should not instantiate to non-Type kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14645">#14645</a></th>
<td>Allow type family in data family return kind</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15546">#15546</a></th>
<td>Display coaxiom branch incompatibilities in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15557">#15557</a></th>
<td>Reduce type families in equations&apos; RHS when testing equation compatibility</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15561">#15561</a></th>
<td>TypeInType: Type error conditioned on ordering of GADT and type family definitions</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15621">#15621</a></th>
<td>Error message involving type families points to wrong location</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15683">#15683</a></th>
<td>coerce fails for Coercible type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15869">#15869</a></th>
<td>Discrepancy between seemingly equivalent type synonym and type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15905">#15905</a></th>
<td>Data familes should end in Type</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16070">#16070</a></th>
<td>Better inferred signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16081">#16081</a></th>
<td>Clean up family instance consistency checking</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/2721">#2721</a></th>
<td>Newtype deriving doesn&apos;t work with type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/3990">#3990</a></th>
<td>UNPACK doesn&apos;t unbox data families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5633">#5633</a></th>
<td>TypeFamilies don&apos;t seem to play with LIberalTypeSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/6018">#6018</a></th>
<td>Injective type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/6035">#6035</a></th>
<td>Kind-indexed type family failure with polymorphic kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/6044">#6044</a></th>
<td>Regression error: Kind variables don&apos;t work inside of kind constructors in type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7005">#7005</a></th>
<td>GHC 7.4.2 crashes with a panic when using type families and data kinds together</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7073">#7073</a></th>
<td>Kind variable problem when declaring associated type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7156">#7156</a></th>
<td>&quot;Pattern match on GADT&quot; error for non-GADT</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7176">#7176</a></th>
<td>Failure to let kind variable remain uninstantiated when not needed</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7288">#7288</a></th>
<td>type inference fails with where clause (RankNTypes, TypeFamilies)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7477">#7477</a></th>
<td>reifyInstances can&apos;t deal with polykinded type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7560">#7560</a></th>
<td>Panic in conflictInstErr when branched type family instances conflict</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/7585">#7585</a></th>
<td>Core lint failure when optimizing coercions in branched axioms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8165">#8165</a></th>
<td>Use GeneralizedNewtypeDeriving to automatically create associated type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8913">#8913</a></th>
<td>either bug or confusing error message mixing PolyKinds and TypeFamilies</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9582">#9582</a></th>
<td>Associated Type Synonyms do not unfold in InstanceSigs</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9747">#9747</a></th>
<td>Odd failure to deduce a constraint</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11062">#11062</a></th>
<td>Type families + hs-boot files = panic (type family consistency check too early)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11282">#11282</a></th>
<td>Error warns about non-injectivity of injective type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11348">#11348</a></th>
<td>Local open type families instances ignored during type checking</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11357">#11357</a></th>
<td>Regression when deriving Generic1 on poly-kinded data family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11375">#11375</a></th>
<td>Type aliases twice as slow to compile as closed type families.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11381">#11381</a></th>
<td>Put injective type families in a separate language extension</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11400">#11400</a></th>
<td>* is not an indexed type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11407">#11407</a></th>
<td>-XTypeInType uses up all memory when used in data family instance</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11708">#11708</a></th>
<td>Typechecker hangs when checking type families with -ddump-tc-trace turned on</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12057">#12057</a></th>
<td>TypeFamilyDependencies on   Data.Type.Bool&apos;s `Not`</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12089">#12089</a></th>
<td>:kind command allows unsaturated type family,</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12104">#12104</a></th>
<td>Type families, `TypeError`, and `-fdefer-type-errors` cause &quot;opt_univ fell into a hole&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12119">#12119</a></th>
<td>Can&apos;t create injective type family equation with TypeError as the RHS</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12199">#12199</a></th>
<td>GHC is oblivious to injectivity when solving an equality constraint</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12239">#12239</a></th>
<td>Dependent type family does not reduce</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12638">#12638</a></th>
<td>GHC panic when resolving Show instance</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13025">#13025</a></th>
<td>Type family reduction irregularity (change from 7.10.3 to 8.0.1)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13398">#13398</a></th>
<td>Associated type family instance validity checking is too conservative</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13420">#13420</a></th>
<td>Bizarre pretty-printing of closed type families in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13774">#13774</a></th>
<td>Singletons code fails to typecheck when type signature involving type family is added</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13790">#13790</a></th>
<td>GHC doesn&apos;t reduce type family in kind signature unless its arm is twisted</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13809">#13809</a></th>
<td>TH-reified type family and data family instances have a paucity of kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13901">#13901</a></th>
<td>Lift the &quot;Illegal polymorphic type&quot; restriction on type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13913">#13913</a></th>
<td>Can&apos;t apply higher-ranked kind in type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13915">#13915</a></th>
<td>GHC 8.2 regression: &quot;Can&apos;t find interface-file declaration&quot; for promoted data family instance</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13962">#13962</a></th>
<td>GHCi allows unsaturated type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13972">#13972</a></th>
<td>GHC 8.2 error message around indexes for associated type instances is baffling</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13985">#13985</a></th>
<td>GHC 8.0 regression: ‘k’ is not in scope during type checking, but it passed the renamer</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14000">#14000</a></th>
<td>Out of scope errors with type families do not mention scope</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14042">#14042</a></th>
<td>Datatypes cannot use a type family in their return kind</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14045">#14045</a></th>
<td>Data family instances must list all patterns of family, despite documentation&apos;s claims to the contrary</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14131">#14131</a></th>
<td>Difference between newtype and newtype instance</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14179">#14179</a></th>
<td>&quot;Conflicting family instance&quot; error pretty prints data family instances poorly</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14203">#14203</a></th>
<td>GHC-inferred type signature doesn&apos;t actually typecheck</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14333">#14333</a></th>
<td>GHC doesn&apos;t use the fact that Coercible is symmetric</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14366">#14366</a></th>
<td>Type family equation refuses to unify wildcard type patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14440">#14440</a></th>
<td>Duplicate type family instances are permitted</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14441">#14441</a></th>
<td>GHC HEAD regression involving type families in kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14462">#14462</a></th>
<td>deriving on associated data types fails to find constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14547">#14547</a></th>
<td>Wrong warning by -Wincomplete-patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14661">#14661</a></th>
<td>Cannot derive (newtype I a b = I (F a -&gt; F b) deriving newtype Category) for type family F</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14728">#14728</a></th>
<td>Is (GeneralizedNewtypeDeriving + associated type classes) completely bogus?</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14938">#14938</a></th>
<td>Pattern matching on GADT does not refine type family parameters</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14991">#14991</a></th>
<td>GHC HEAD regression involving TYPEs in type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15057">#15057</a></th>
<td>Lint types created by newFamInst</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15142">#15142</a></th>
<td>GHC HEAD regression: tcTyVarDetails</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15144">#15144</a></th>
<td>Type inference regression between GHC 8.0.2 and 8.2.2</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15318">#15318</a></th>
<td>Core Lint error involving newtype family instances with wrappers</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15341">#15341</a></th>
<td>:info prints kinds in closed type family equations without enabling -fprint-explicit-kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15380">#15380</a></th>
<td>Infinite typechecker loop in GHC 8.6</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15428">#15428</a></th>
<td>Oversaturated type family application panicks GHC (piResultTys2)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15515">#15515</a></th>
<td>Bogus &quot;No instance&quot; error when type families appear in kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15568">#15568</a></th>
<td>Kind variables in type family aren&apos;t quantified in toposorted order</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15591">#15591</a></th>
<td>Inconsistent kind variable binder visibility between associated and non-associated type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15592">#15592</a></th>
<td>Type families without CUSKs cannot be given visible kind variable binders</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15691">#15691</a></th>
<td>Marking Pred(S n) = n as injective</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15704">#15704</a></th>
<td>Different saturations of the same polymorphic-kinded type constructor aren&apos;t seen as apart types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15740">#15740</a></th>
<td>Type family with higher-rank result is too accepting</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15796">#15796</a></th>
<td>Core Lint error with invalid newtype declaration</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15827">#15827</a></th>
<td>Explicit foralls in type family equations are pretty-printed inconsistently (and strangely, at times)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15828">#15828</a></th>
<td>Type family equation foralls allow strange re-quantification of class-bound type variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15852">#15852</a></th>
<td>Bad axiom produced for polykinded data family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15986">#15986</a></th>
<td>Poor error message source location reporting with unsaturated type family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16002">#16002</a></th>
<td>Type family equation with wrong name is silently accepted (GHC 8.6+ only)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16008">#16008</a></th>
<td>GHC HEAD type family regression involving invisible arguments</td></tr></table>



## Terminology



**Data-type family**: a data type declared with a `data family` declaration.



**Type-synonym family**, or **type function**: a type synonym declared with a `type family` declaration.



**Type family**: a data-type family or type-synonym family.



**Parametric type constructors**: the type constructor of a vanilla Haskell type.



**Family type constructor** or **Family `TyCon`**: the type constructor for a type family.



**Instance `TyCon`**: the `TyCon` arising from a `data/newtype/type instance` declaration.  Sometimes called the **representation `TyCon`**.  The instance `TyCon` is invisible to the programmer; it is only used internally inside GHC.  



**Associated type**: A type family that is declared in a type class.



**Kind signature**: Declaration of the name, kind, and arity of an indexed type constructor.  The *arity* is the number of type indexes - *not* the overall number of parameters - of an indexed type constructor.



**Definitions vs. declarations**: We sometimes call the kind signature of an indexed constructor its *declaration* and the subsequent population of the type family by type equations or indexed data/newtype declarations the constructor's *definition*.


Note: we previously used the term "indexed type", but have now switched to using "type family".  Please change any  uses of the former into the latter as you come across them.

## How It Works


The details of the implementation are split over a couple of subpages, due to the amount of the material:

- [syntax and representation,](type-functions-syntax)
- [renaming,](type-functions-renaming)
- [type checking,](type-functions-type-checking)
- [desugaring,](type-functions-core) and
- [interfaces.](type-functions-iface)


Furthermore, we have

- [details on the normalisation and solving of type equalities](type-functions-solving) and
- [integrating class and equality constraint solving.](type-functions/integrated-solver)

## References

- [ Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. In Proceedings of ICFP 2008 : The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 51-62, 2008.
- [ Associated Types with Class.](http://www.cse.unsw.edu.au/~chak/papers/CKPM05.html) Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, and Simon Marlow. In Proceedings of The 32nd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL'05), ACM Press, pages 1-13, 2005.
- [ Associated Type Synonyms.](http://www.cse.unsw.edu.au/~chak/papers/CKP05.html) Manuel M. T. Chakravarty, Gabriele Keller, and Simon Peyton Jones. In Proceedings of The Tenth ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 241-253, 2005.
- [ Towards Open Type Functions for Haskell.](http://www.cse.unsw.edu.au/~chak/papers/SSPC07.html) Tom Schrijvers, Martin Sulzmann, Simon Peyton-Jones, and Manuel M. T. Chakravarty. Presented at IFL 2007.
- [ Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. ICFP 2008: The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, 2008.
- Old and outdated wiki material on [type checking with indexed synonyms.](type-functions-syn-tc)
