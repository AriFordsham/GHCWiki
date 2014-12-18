# [OverloadedRecordFields](records/overloaded-record-fields)


The `OverloadedRecordFields` extension for GHC allows multiple record datatypes to share the same field names, and uses type information to disambiguate them. For more information, see:

- [Design of the extension](records/overloaded-record-fields/design) (for power users)
- [Notes on the implementation](records/overloaded-record-fields/implementation) (for GHC hackers)


Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Code


The latest code is on the `wip/orf-new` branch of the `ghc` and `haddock` development repositories.  The forks of [ ghc](https://github.com/adamgundry/ghc), [ packages-base](https://github.com/adamgundry/packages-base) and [ haddock](https://github.com/adamgundry/haddock) have been superseded.


A [ prototype implementation](https://github.com/adamgundry/records-prototype) of the design is also available.


The design has mostly stabilised, but there is still some work to do on the implementation. See [notes for GHC hackers on the current status of the code](records/overloaded-record-fields/implementation#current-status).

## History


The extension was implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Discussion of the problem and possible solutions](records)
- [ Google Summer of Code project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/4766932662222848)