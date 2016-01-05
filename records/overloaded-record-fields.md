# [OverloadedRecordFields](records/overloaded-record-fields)


The `OverloadedRecordFields` extension for GHC will allow multiple record datatypes to share the same field names, and make it possible for type information to disambiguate them. It consists of three separate of components, of which two will be in GHC 8.0. For more information, see:

- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign)

  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) (in GHC 8.0)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels) (in GHC 8.0)
  - Part 3: [Magic type classes](records/overloaded-record-fields/magic-classes) (will not make GHC 8.0)
- [ Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Discussion

*Lennart*: I've implemented 2&3 in the Mu compiler, and I'll add some comments about it.

*Lennart*: The MagicClasses proposal is fundamentally broken, because it breaks abstraction.


If we have a data type `R` with a field `foo` of type `T`, then we generate `instance HasField "foo" R T`.
Since instances are silently exported and imported it means that this instance is now available in every module that somehow depends on the defining module.  This means that the `foo` field of `R` is now accessible everywhere.  There is no way to limit the scope of `foo` anymore.  This is really terrible.  Any record proposal that no longer allows abstract data types to be defined is broken.

*Adam*: I don't think so. We will only solve a `HasField` constraint automatically in modules that have the field in scope, and thus we can retain the same abstraction rules as normal Haskell. This is just like `Coercible`. We don't really generate and export instances, rather there is special behaviour in the constraint solver. See [discussion on representation hiding](records/overloaded-record-fields/magic-classes#representation-hiding) (now updated to the latest design).

## Issues


Open Tickets:

<table><tr><th>[\#4479](https://gitlab.haskell.org//ghc/ghc/issues/4479)</th>
<td>Implement TDNR</td></tr>
<tr><th>[\#11228](https://gitlab.haskell.org//ghc/ghc/issues/11228)</th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th>[\#11343](https://gitlab.haskell.org//ghc/ghc/issues/11343)</th>
<td>Unable to infer type when using DuplicateRecordFields</td></tr>
<tr><th>[\#11352](https://gitlab.haskell.org//ghc/ghc/issues/11352)</th>
<td>Allow applying type to label</td></tr>
<tr><th>[\#11671](https://gitlab.haskell.org//ghc/ghc/issues/11671)</th>
<td>Allow labels starting with uppercase with OverloadedLabels</td></tr>
<tr><th>[\#13352](https://gitlab.haskell.org//ghc/ghc/issues/13352)</th>
<td>Strange requirement for re-exported duplicate record fields</td></tr>
<tr><th>[\#13438](https://gitlab.haskell.org//ghc/ghc/issues/13438)</th>
<td>ghci :browse does not work with DuplicateRecordFields</td></tr>
<tr><th>[\#14848](https://gitlab.haskell.org//ghc/ghc/issues/14848)</th>
<td>-XDuplicateRecordFields breaks record expression splices</td></tr>
<tr><th>[\#14892](https://gitlab.haskell.org//ghc/ghc/issues/14892)</th>
<td>Field imposters with DuplicateRecordFields and NamedFieldPuns.</td></tr>
<tr><th>[\#15277](https://gitlab.haskell.org//ghc/ghc/issues/15277)</th>
<td>Move field name resolution to the type-checker</td></tr>
<tr><th>[\#16232](https://gitlab.haskell.org//ghc/ghc/issues/16232)</th>
<td>Add setField to HasField</td></tr></table>


Closed Tickets:

<table><tr><th>[\#11103](https://gitlab.haskell.org//ghc/ghc/issues/11103)</th>
<td>DuplicateRecordFields + TemplateHaskell</td></tr>
<tr><th>[\#11167](https://gitlab.haskell.org//ghc/ghc/issues/11167)</th>
<td>Fixity of field-deconstructors incorrect</td></tr>
<tr><th>[\#11173](https://gitlab.haskell.org//ghc/ghc/issues/11173)</th>
<td>Infix declarations for record fields with DuplicateRecordFields are broken</td></tr>
<tr><th>[\#11227](https://gitlab.haskell.org//ghc/ghc/issues/11227)</th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th>[\#11328](https://gitlab.haskell.org//ghc/ghc/issues/11328)</th>
<td>Auto complete in ghci shows $sel:function:Type for DuplicateRecordFields fields</td></tr>
<tr><th>[\#11662](https://gitlab.haskell.org//ghc/ghc/issues/11662)</th>
<td>Regression using NamedFieldPuns with qualified field names</td></tr>
<tr><th>[\#12097](https://gitlab.haskell.org//ghc/ghc/issues/12097)</th>
<td>DuplicateRecordFields appears not to work in GHCi</td></tr>
<tr><th>[\#12243](https://gitlab.haskell.org//ghc/ghc/issues/12243)</th>
<td>RebindableSyntax and OverloadedLabels</td></tr>
<tr><th>[\#12459](https://gitlab.haskell.org//ghc/ghc/issues/12459)</th>
<td>UnboxedTuple makes overloaded labels fail to parse</td></tr>
<tr><th>[\#12609](https://gitlab.haskell.org//ghc/ghc/issues/12609)</th>
<td>unused-top-binds wrongly warns about underscore-prefixed field names when DuplicateRecordFields enabled</td></tr>
<tr><th>[\#13132](https://gitlab.haskell.org//ghc/ghc/issues/13132)</th>
<td>Compilation fails with a panic: get_op runContT</td></tr>
<tr><th>[\#13847](https://gitlab.haskell.org//ghc/ghc/issues/13847)</th>
<td>record construction accepts local unqualified name instead of qualified imported name</td></tr>
<tr><th>[\#13947](https://gitlab.haskell.org//ghc/ghc/issues/13947)</th>
<td>GHC 8.2 gives misleading error message for out-of-scope infix type constructor</td></tr>
<tr><th>[\#14487](https://gitlab.haskell.org//ghc/ghc/issues/14487)</th>
<td>Can't Hide Field When DuplicateRecordFields Is Enabled</td></tr>
<tr><th>[\#14783](https://gitlab.haskell.org//ghc/ghc/issues/14783)</th>
<td>Initializing record with similarly named field from a different record results in warning rather than error</td></tr>
<tr><th>[\#14953](https://gitlab.haskell.org//ghc/ghc/issues/14953)</th>
<td>Panic when exporting duplicate record fields from separate modules</td></tr>
<tr><th>[\#15149](https://gitlab.haskell.org//ghc/ghc/issues/15149)</th>
<td>Identical distinct type family fields miscompiled</td></tr></table>


Add **ORF** to the ticket's keywords to include it in these lists.

## Code

- [ Phab:D761](https://phabricator.haskell.org/D761), [ Phab:D1391](https://phabricator.haskell.org/D1391), [ Phab:D1486](https://phabricator.haskell.org/D1486), [ Phab:D1586](https://phabricator.haskell.org/D1586), [ Phab:D1600](https://phabricator.haskell.org/D1600): `DuplicateRecordFields` extension
- [ Phab:D1331](https://phabricator.haskell.org/D1331), [ Phab:D1623](https://phabricator.haskell.org/D1623): `OverloadedLabels` extension
- [ Phab:D1687](https://phabricator.haskell.org/D1687): magic classes
- [ Prototype implementation of the magic typeclasses](https://github.com/adamgundry/records-prototype)

## History


The extension was implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Original design of the extension](records/overloaded-record-fields/design)
- [Discussion of the problem and possible solutions](records)
- [ Google Summer of Code project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/4766932662222848)