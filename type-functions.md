# Type Functions, Type Families, and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type families (aka type functions) and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.


See the Haskell Wiki for [user-level documentation](http://haskell.org/haskellwiki/GHC/Indexed_types).

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

See the ~TypeFamilies label.



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

- [Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. In Proceedings of ICFP 2008 : The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 51-62, 2008.
- [Associated Types with Class.](http://www.cse.unsw.edu.au/~chak/papers/CKPM05.html) Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, and Simon Marlow. In Proceedings of The 32nd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL'05), ACM Press, pages 1-13, 2005.
- [Associated Type Synonyms.](http://www.cse.unsw.edu.au/~chak/papers/CKP05.html) Manuel M. T. Chakravarty, Gabriele Keller, and Simon Peyton Jones. In Proceedings of The Tenth ACM SIGPLAN International Conference on Functional Programming, ACM Press, pages 241-253, 2005.
- [Towards Open Type Functions for Haskell.](http://www.cse.unsw.edu.au/~chak/papers/SSPC07.html) Tom Schrijvers, Martin Sulzmann, Simon Peyton-Jones, and Manuel M. T. Chakravarty. Presented at IFL 2007.
- [Type Checking with Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, and Martin Sulzmann. ICFP 2008: The 13th ACM SIGPLAN International Conference on Functional Programming, ACM Press, 2008.
- Old and outdated wiki material on [type checking with indexed synonyms.](type-functions-syn-tc)
