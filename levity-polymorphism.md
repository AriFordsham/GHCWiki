# Levity polymorphism


This is a status page for levity polymorphism, as described in the [Levity Polymorphism paper](https://www.microsoft.com/en-us/research/publication/levity-polymorphism/).


See also [UnliftedDataTypes](unlifted-data-types)

## Tickets



Use Keyword = `LevityPolymorphism` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11715">#11715</a></th>
<td>Constraint vs *</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12708">#12708</a></th>
<td>RFC: Representation polymorphic Num</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12980">#12980</a></th>
<td>Unlifted class method rejected</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12989">#12989</a></th>
<td>($) can have a more general type</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13105">#13105</a></th>
<td>Allow type families in RuntimeReps</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13233">#13233</a></th>
<td>typePrimRep panic while compiling GHC with profiling</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13592">#13592</a></th>
<td>Newtype type class with compiler generated instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13595">#13595</a></th>
<td>Should ‘coerce’ be levity polymorphic?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14185">#14185</a></th>
<td>Non-local bug reporting around levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14196">#14196</a></th>
<td>Replace ArrayArray# with either UnliftedArray# or Array#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14765">#14765</a></th>
<td>Levity polymorphism panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14917">#14917</a></th>
<td>Allow levity polymorphism in binding position</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15532">#15532</a></th>
<td>Relaxing Levity-Polymorphic Binder Check for Lifted vs Unlifted pointers</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15883">#15883</a></th>
<td>GHC panic: newtype F rep = F (forall (a :: TYPE rep). a)</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11431">#11431</a></th>
<td>GHC instantiates levity-polymorphic type variables with foralls</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11465">#11465</a></th>
<td>Eliminate check_lifted check in TcValidity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11471">#11471</a></th>
<td>Kind polymorphism and unboxed types: bad things are happening</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11473">#11473</a></th>
<td>Levity polymorphism checks are inadequate</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11714">#11714</a></th>
<td>Kind of (-&gt;) type constructor is overly constrained</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11724">#11724</a></th>
<td>Must check for representation polymorphism in datatype declarations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11736">#11736</a></th>
<td>Allow unsaturated uses of unlifted types in Core</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11786">#11786</a></th>
<td>Need -fno-print-explicit-runtime-reps to work on IfaceType, else RuntimeRep leaks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12668">#12668</a></th>
<td>Program that fails Core Lint terribly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12670">#12670</a></th>
<td>Representation polymorphism validity check is too strict</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12709">#12709</a></th>
<td>GHC panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12718">#12718</a></th>
<td>Segmentation fault, runtime representation polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12809">#12809</a></th>
<td>TYPE &apos;UnboxedTupleRep is still a lie</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12901">#12901</a></th>
<td>Levity polymorphic expressions mustn&apos;t be floated out</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12905">#12905</a></th>
<td>Core lint failure with pattern synonym and levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12911">#12911</a></th>
<td>Levity polymorphism check eliminates non-levity-polymorphic data constructor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12973">#12973</a></th>
<td>No levity-polymorphic arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12987">#12987</a></th>
<td>Core lint error with levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13202">#13202</a></th>
<td>Levity polymorphism panic in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13244">#13244</a></th>
<td>Error Dealing with Unboxed Types and Type Families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13275">#13275</a></th>
<td>ghci ignores -fprint-explicit-runtime-reps</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13343">#13343</a></th>
<td>Levity polymorphism-related GHC panic: expectJust zonkTcTyVarToVar</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13435">#13435</a></th>
<td>Segfaults on levity-polymorphic type class</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13458">#13458</a></th>
<td>Panic with unsafeCoerce and -dcore-lint</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13509">#13509</a></th>
<td>Perplexing type error with unboxed tuples</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13601">#13601</a></th>
<td>GHC errors but hangs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13603">#13603</a></th>
<td>Can&apos;t resolve levity polymorphic superclass</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13929">#13929</a></th>
<td>GHC panic with levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13955">#13955</a></th>
<td>Backpack does not handle unlifted types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13963">#13963</a></th>
<td>Runtime representation confusingly displayed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14555">#14555</a></th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14561">#14561</a></th>
<td>Panic on levity polymorphic very unsafe coerce</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14563">#14563</a></th>
<td>GHC Panic with TypeInType / levity polymorphism</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15180">#15180</a></th>
<td>Make Control.Exception.throw levity polymorphic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15181">#15181</a></th>
<td>Levity Polymorphic type signatures in GHC.Prim</td></tr></table>



