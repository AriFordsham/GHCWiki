# Anonymous Records with lenses


This page is to discuss adding support for Nikita Volkov's record design to GHC.


Links

- The record package on Hackage: [ http://hackage.haskell.org/package/record](http://hackage.haskell.org/package/record)
- Homepage and tutorial: [ http://nikita-volkov.github.io/record/](http://nikita-volkov.github.io/record/)
- Reddit discussion: [ http://www.reddit.com/r/haskell/comments/2svayz/i_think_ive_nailed_it_ive_solved_the_records/](http://www.reddit.com/r/haskell/comments/2svayz/i_think_ive_nailed_it_ive_solved_the_records/)

## Drawbacks


The most important drawbacks of this design relative to other designs are:

- Lack of support for type-changing update
- Lack of support for strict and unpacked fields
- Lack of support for polymorphic (Rank-N) fields
- Fixed limit on the number of fields (24 in the current implementation)


Also, it is questionable whether any record extension should bake in a particular lens type.