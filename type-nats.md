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

- The implementation resides in several repositories:

  - (git) Changes to GHC are on branch type-nats in: [ git://code.galois.com/type-naturals/ghc.git](git://code.galois.com/type-naturals/ghc.git)
  - (darcs) Changes to the base library are at [ http://code.galois.com/darcs/type-naturals/09-Jan-2011/base/](http://code.galois.com/darcs/type-naturals/09-Jan-2011/base/)
  - (darcs) Changes to the template-haskell library are at [ http://code.galois.com/darcs/type-naturals/09-Jan-2011/template-haskell/](http://code.galois.com/darcs/type-naturals/09-Jan-2011/template-haskell/)

- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)