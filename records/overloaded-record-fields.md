# [OverloadedRecordFields](records/overloaded-record-fields)


The `OverloadedRecordFields` extension for GHC allows multiple record datatypes to share the same field names, and uses type information to disambiguate them. For more information, see:

- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign)

  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) (in GHC 8.0)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels) (aimed for GHC 8.0)
  - Part 3: [Magic type classes](records/overloaded-record-fields/magic-classes) (will probably not make GHC 8.0)
- [ Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

*Lennart*: I've implemented 2&3 in the Mu compiler, and I'll add some comments about it.

*Lennart*: The MagicClasses proposal is fundamentally broken, because it breaks abstraction.


If we have a data type `R` with a field `foo` of type `T`, then we generate `instance HasField "foo" R T`.
Since instances are silently exported and imported it means that this instance is now available in every module that somehow depends on the defining module.  This means that the `foo` field of `R` is now accessible everywhere.  There is no way to limit the scope of `foo` anymore.  This is really terrible.  Any record proposal that no longer allows abstract data types to be defined is broken.

## Code

- [ Phab:D761](https://phabricator.haskell.org/D761) (merged) and [ Phab:D1391](https://phabricator.haskell.org/D1391) (being reviewed): `DuplicateRecordFields` extension
- [ Phab:D1331](https://phabricator.haskell.org/D1331) (being reviewed): `OverloadedLabels` extension
- [ Prototype implementation of the magic typeclasses](https://github.com/adamgundry/records-prototype)

## History


The extension was implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Original design of the extension](records/overloaded-record-fields/design)
- [Discussion of the problem and possible solutions](records)
- [ Google Summer of Code project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/4766932662222848)