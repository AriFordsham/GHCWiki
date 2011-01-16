# Type Level Naturals


This page collects information on how to work with type-level natural numbers, as implemented in the Haskell compiler GHC (ticket [\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385)).

## User's Guide

- [Type-Level Naturals Basics](type-nats/basics)
- [Natural Numbers: From Values to Types](type-nats/naturals)
- [Type-Level Operations](type-nats/operations)

## Implementation and Design Issues

- [Implementation of GHC.TypeNats](type-nats/implementation)
- [Alternative Design For Singletons](type-nats/alternative-singletons)
- [Axioms for Natural Number Operators](type-nats/axioms)
- GHC Interaction Rules ([Notational Conventions](type-nats/rule-notation))

  - [Top-Level Interactions](type-nats/interact1)
  - [Simple Inert Interactions](type-nats/interact2)
  - [Solving (\<=) Predicates](type-nats/leq)

## External links

- [ Source repos](http://code.galois.com/darcs/type-naturals/)
- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)