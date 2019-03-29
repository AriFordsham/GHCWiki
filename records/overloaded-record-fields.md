# [OverloadedRecordFields](records/overloaded-record-fields)


The Overloaded Record Fields family of extensions for GHC allow multiple record datatypes to share the same field names, and make it possible for type information to disambiguate selectors. In the design as implemented, there is no single `OverloadedRecordFields` extension, but there are extensions for `DuplicateRecordFields` and `OverloadedLabels`.


For more information, see:

- GHC proposals (most up to date):

  - [Adding setField to HasField](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-record-set-field.rst) (2018, not yet implemented)
  - [Adding HasField class, changes to OverloadedLabels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst) (2016, implemented in GHC 8.2 without `IsLabel x (r -> a)` instance)
- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign) (2015)

  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) (in GHC 8.0)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels) (in GHC 8.0)
  - Part 3: [Magic type classes](records/overloaded-record-fields/magic-classes) (partly in GHC 8.2)
  - [Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)
- [Original design](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Design) (2013)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Issues


Add **ORF** to the ticket's keywords to include it in these lists.



Open Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4479">#4479</a></th>
<td>Implement TDNR</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11228">#11228</a></th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11343">#11343</a></th>
<td>Unable to infer type when using DuplicateRecordFields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11352">#11352</a></th>
<td>Allow applying type to label</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11671">#11671</a></th>
<td>Allow labels starting with uppercase with OverloadedLabels</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13352">#13352</a></th>
<td>Strange requirement for re-exported duplicate record fields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13438">#13438</a></th>
<td>ghci :browse does not work with DuplicateRecordFields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14848">#14848</a></th>
<td>-XDuplicateRecordFields breaks record expression splices</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14892">#14892</a></th>
<td>Field imposters with DuplicateRecordFields and NamedFieldPuns.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15277">#15277</a></th>
<td>Move field name resolution to the type-checker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16232">#16232</a></th>
<td>Add setField to HasField</td></tr></table>




Closed Tickets:

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11103">#11103</a></th>
<td>DuplicateRecordFields + TemplateHaskell</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11167">#11167</a></th>
<td>Fixity of field-deconstructors incorrect</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11173">#11173</a></th>
<td>Infix declarations for record fields with DuplicateRecordFields are broken</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11227">#11227</a></th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11328">#11328</a></th>
<td>Auto complete in ghci shows $sel:function:Type for DuplicateRecordFields fields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11662">#11662</a></th>
<td>Regression using NamedFieldPuns with qualified field names</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12097">#12097</a></th>
<td>DuplicateRecordFields appears not to work in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12243">#12243</a></th>
<td>RebindableSyntax and OverloadedLabels</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12459">#12459</a></th>
<td>UnboxedTuple makes overloaded labels fail to parse</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12609">#12609</a></th>
<td>unused-top-binds wrongly warns about underscore-prefixed field names when DuplicateRecordFields enabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13132">#13132</a></th>
<td>Compilation fails with a panic: get_op runContT</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13847">#13847</a></th>
<td>record construction accepts local unqualified name instead of qualified imported name</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13947">#13947</a></th>
<td>GHC 8.2 gives misleading error message for out-of-scope infix type constructor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14487">#14487</a></th>
<td>Can&apos;t Hide Field When DuplicateRecordFields Is Enabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14783">#14783</a></th>
<td>Initializing record with similarly named field from a different record results in warning rather than error</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14953">#14953</a></th>
<td>Panic when exporting duplicate record fields from separate modules</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15149">#15149</a></th>
<td>Identical distinct type family fields miscompiled</td></tr></table>



## Code


- [Phab:D761](https://phabricator.haskell.org/D761), [ Phab:D1391](https://phabricator.haskell.org/D1391), [ Phab:D1486](https://phabricator.haskell.org/D1486), [ Phab:D1586](https://phabricator.haskell.org/D1586), [ Phab:D1600](https://phabricator.haskell.org/D1600): `DuplicateRecordFields` extension
- [Phab:D1331](https://phabricator.haskell.org/D1331), [ Phab:D1623](https://phabricator.haskell.org/D1623): `OverloadedLabels` extension
- [Phab:D1687](https://phabricator.haskell.org/D1687), [ Phab:D2708](https://phabricator.haskell.org/D2708): magic classes
- [Prototype implementation of the magic typeclasses](https://github.com/adamgundry/records-prototype)

## History


The extension was implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Original design of the extension](records/overloaded-record-fields/design)
- [Discussion of the problem and possible solutions](records)
- [Google Summer of Code project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/4766932662222848)
