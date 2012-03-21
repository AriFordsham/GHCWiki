# Type Level Naturals


This page collects information on how to work with type-level natural numbers, as implemented in the Haskell compiler GHC (ticket [\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385)).

## User's Guide

- [Type-Level Naturals Basics](type-nats/basics)
- [Type-Level Computation](type-nats/operations)
- [Typed examinations of TNat (inductive definitions)](type-nats/inductive-definitions)

## Notes on Design

- [Alternative Design For Singletons](type-nats/alternative-singletons)
- [Avoiding Partial Type Functions](type-nats/avoiding-partial-type-functions)

## Notes on the Implementation

- [Implementation of GHC.TypeLits](type-nats/implementation)
- [ Axioms for type-level type operators](http://github.com/yav/tc-solver/blob/master/docs/axioms.md)

## External links

- The implementation resides on branch 'type-nats' of the GHC repo.  The following GHC-related related repos also have a type-nats branch:

  - libraries/base
  - libraries/template-haskell
  - utils/haddock

## XXX: Cleanup

- [Natural Numbers: From Values to Types](type-nats/naturals)
- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)
- [Examples](type-nats/examples)