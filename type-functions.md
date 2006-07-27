# Type Functions and Associated Types in GHC - The Master Plan


This page serves as a collection of notes concerning the implementation of type functions and associated types, especially about the implications for type checking, interface files, and F<sub>C</sub> intermediate code generation.

## Aims


New features:

- Toplevel type function definitions.
- Associated data types and type synonyms in classes, where the latter are eseentially type function definitions spread across the instances of the associated class.  Associated types are essentially syntactic sugar for general type functions.


Revised features

- We may want to re-implement functional dependencies using associated type synonyms.

## Specification and Restrictions


Refinement of the specification in the *Beyond Associated Types* paper.  (I'll actually link this paper here once it is a bit more coherent.)

- Kind signatures can only occur on the type variables that are in excess of the class parameters in an associated type declaration (in a type class declaration). Rationale: The binding position for the class parameters is the class head. That's where the signatures should be.
- Associated data type definitions (in instances) can have kind signatures at type variables occuring in the head. These signatures must coincide with those of the instance head (for the class parameters) and those of the associated data type declarations (for the excess parameters). Rationale: In contrast to class declarations, we don't regard the instance head as binding variables in the body.
- The declaration of an associated data type in a class cannot have a context. Rationale: We don't want a context constraining class parameters for the same reason that we don't want that on function signatures. A context on additional arguments to the data declaration would be feasible, but doesn't seem worth the trouble.  **This is a pre-F<sub>C</sub> restriction that needs to be removed from the currect code base, before being taken off the wiki.**
- The declaration of an associated data type in a class can have a deriving clause. The meaning is that all instances of that type inherit all these derivings (or do we merely want to force them to state - at least - these derivings). Rationale: If I want equality on an associated type, we need to guarantee that all its variants come with an equality.


Restrictions:

- We currently don't allow associated GADTs. I cannot see any fundamental problem in supporting them, but I want to keep it simple for the moment. (When allowing this, a constructor signature in an associated GADT can of course only refine the instantiation of the type arguments specific to the instance in which the constructor is defined.)
- We currently don't have toplevel data definitions with type patterns.  They would essentially be open GADTs, which we probably can type check with the existing GADT machinery and translate much as we translate associated data types in classes.  Again, I want to avoid doing too much in the first sweep.
