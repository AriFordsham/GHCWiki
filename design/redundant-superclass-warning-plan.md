
This plan is spun out of #11370


The immediate plan (for 8.0) is

1. Remove warn-redundant-constraints from the default constraint set **and** the -Wall constraint set.
1. Implement #11429 and make unrecognized -W flags a warning rather than compile error.


Having done so, we are in a position to further move on [https://gitlab.haskell.org/trac/ghc/wiki/Design/Warnings](https://gitlab.haskell.org/trac/ghc/wiki/Design/Warnings) in a future release. In the course of that we can settle on the proper flagset to include this warning in.
