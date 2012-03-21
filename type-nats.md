# Type Level Naturals


This page collects information on how to work with type-level natural numbers, as implemented in the Haskell compiler GHC (ticket [\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385)).

## User's Guide

- [Type-Level Naturals Basics](type-nats/basics)
- [Natural Numbers: From Values to Types](type-nats/naturals)
- [Type-Level Operations](type-nats/operations)
- [Examples](type-nats/examples)

## Notes on Design

- [Alternative Design For Singletons](type-nats/alternative-singletons)
- [Avoiding Partial Type Functions](type-nats/avoiding-partial-type-functions)
- Naturals or Integers?
- [Inductive definitions](type-nats/inductive-definitions)

## Notes on the Implementation

- [Implementation of GHC.TypeNats](type-nats/implementation)
- [Axioms for Natural Number Operators](type-nats/axioms)
- GHC Interaction Rules ([Notational Conventions](type-nats/rule-notation))

  - [Top-Level Interactions](type-nats/interact1)
  - [Simple Inert Interactions](type-nats/interact2)
  - [Solving (\<=) Predicates](type-nats/leq)
  - XXX: Write the new rules
- Translation to FC
- Precedences for infix predicates such as \~ and \<=

## External links

- The implementation resides on branch 'type-nats' of the GHC repo.  The following GHC-related related repos also have a type-nats branch:

  - libraries/base
  - libraries/template-haskell
  - utils/haddock

- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)