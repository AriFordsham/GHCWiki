# Type Functions and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type functions and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.

## Aims


New features:

- Open type-indexed data types and type functions
- Associated data types and type synonyms, which are type-indexed data types and type functions associated with a class - i.e., associated types are syntactic sugar for type-indexed types and type functions.


Revised features

- We may want to re-implement functional dependencies using associated type synonyms.


We keep track of the current [implementation status](type-functions-status).

## Specification and Restrictions


Refinement of the specification in the *Beyond Associated Types* paper.  (I'll actually link this paper here once it is a bit more coherent.)  Some [examples are on an extra page](type-functions-examples).

- Kind signatures of indexed data type families have the form

  ```wiki
  data family T a1 .. an :: <kind>
  ```

  and introduce a data type whose first `n` argument are indexes, with `n` \>= 1.  The `<kind>` can specify additional parametric parameters.   Index variables can have a kind annotation.  Indexed newtypes have the same form, except for the initial keyword.  **Is it still necessary to know the number of type indexes (now that we don't require saturated applications for indexed data types)?  We can now also admit the omission of the kind with \* being the default.**
- Kind signatures of type functions have the form

  ```wiki
  type family [iso] T a1 .. an :: <kind>
  ```

  and introduce `n`-ary type functions, which may be of higher-kind, with `n` \>= 1.  Again, the type variables can have kind signatures.  The modifier `iso` is optional and requires the type function to be injective.  (In principle, we could make the `<kind>` optional, with `*` being the default, but we don't do that for uniformity with signatures of indexed types - the form `data T a1 .. an` is already used for empty data types.  **Not true anymore.**)
- Applications of type functions need to supply all indexes after unfolding of all ordinary type synonyms.  (This is the same saturation requirement that we walready have on ordinary type synonyms.)
- Instances of indexed data types/newtypes and equations of type functions have the keyword `instance` after the first keyword.  They otherwise have the same form as ordinary data types/newtypes and type synonyms, respectively, but can have non-variable type indexes in index positions.  Type indexes can include applications of indexed data types and newtypes, but no type functions.
- Instances of indexed types are only valid if a kind signature for the type constructor is in scope.  The kind of an indexed type is solely determined from the kind signature.  Instances must conform to this kind; in particular, they must have the same number of type indexes.
- All type indexes of an associated indexed type or type function need to be class parameters.
- Instances of indexed data and new types may not overlap (as such instances correspond to indeterminate type functions).  Type equations may only overlap if the equations coincide at critical pairs.  (Rational: We cannot be more lazy about checking overlap, as we otherwise cannot guarantee that we generate an F<sub>C</sub> program that fulfils the formal consistency criterion.)


Restrictions:

- We currently don't allow associated GADTs. I cannot see any fundamental problem in supporting them, but I want to keep it simple for the moment. (When allowing this, a constructor signature in an associated GADT can of course only refine the instantiation of the type arguments specific to the instance in which the constructor is defined.)

## Terminology

**Parametric type constructors**: Type constructors in vanilla Haskell.

**Indexed type constructors**: Type constructors that are defined via one or more type declarations that have non-variable parameters.  We often call them sloppily just *indexed types*.  We informally call constructors that are not indexed *vanilla* constructors.

**Kind signature**: Declaration of the name, kind, and arity of an indexed type constructor.  The *arity* is the number of type indexes - *not* the overall number of parameters - of an indexed type constructor.

**Type function**: An indexed type synonym.

**Indexed data type**: An indexed type constructor declared with `data` or `newtype`.

**Associated type**: An indexed type that is declared in a type class.

**Type family**: Indexed types can be regarded as families of types; especially in the case of indexed data types, we call each declaration at a particular type index as *member* or *element* of that family.

**Definitions vs. declarations**: We sometimes call the kind signature of an indexed constructor its *declaration* and the subsequent population of the type family by type equations or indexed data/newtype declarations the constructor's *definition*.

## How It Works


The details of the implementation are split over a couple of subpages, due to the amount of the material:

- [syntax and representation,](type-functions-syntax)
- [renaming,](type-functions-renaming)
- [type checking,](type-functions-type-checking) and
- [desugaring.](type-functions-core)

## Possible Extensions

- Our type-indexed data types are open.  However, we currently don't allow case expressions mixing constructors from different indexes.  We could do that if we had a story for open function definitions outside of classes.
