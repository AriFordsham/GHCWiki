# [OverloadedRecordFields](records/overloaded-record-fields)


The `OverloadedRecordFields` extension for GHC allows multiple record datatypes to share the same field names, and uses type information to disambiguate them. For more information, see:

- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign)

  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels)
- [Notes on the implementation](records/overloaded-record-fields/implementation)
- [ Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Code

- [ Phab:D761](https://phabricator.haskell.org/D761) (merged): `DuplicateRecordFields` extension
- [ Phab:D1331](https://phabricator.haskell.org/D1331) (being worked on): `OverloadedLabels` extension
- A [ prototype implementation](https://github.com/adamgundry/records-prototype) of an early design is also available.

## History


The extension was implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Original design of the extension](records/overloaded-record-fields/design)
- [Discussion of the problem and possible solutions](records)
- [ Google Summer of Code project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/4766932662222848)