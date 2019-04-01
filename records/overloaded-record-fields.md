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
- [Original design](https://gitlab.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Design) (2013)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Issues

See the ~OverloadedRecordFields label.

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
