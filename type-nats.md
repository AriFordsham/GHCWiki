# Type Level Naturals


This page collects information on how to work with type-level natural numbers, as implemented in the Haskell compiler GHC (ticket [\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385)).

## User's Guide

- [Type-Level Naturals Basics](type-nats/basics)
- [Simple Examples of Using GHC.TypeNats](type-nats/basic-examples)
- [Design Notes about Nat vs. TypeNat](type-nats/implicit-explicit)

## Implementation and Design Issues

- [Alternative Design For Singletons](type-ntas/alternative-singletins)

- [Implementation of {{{GHC.TypeNats}}}](type-nats/implementation)

- [Axioms for Natural Number Operators](type-nats/axioms)
- GHC Interaction Rules ([Notational Conventions](type-nats/rule-notation))

  - [Top-Level Interactions](type-nats/interact1)
  - [Simple Inert Interactions](type-nats/interact2)
  - [Solving (\<=) Predicates](type-nats/leq)

## External links

- [ Source repos](http://code.galois.com/darcs/type-naturals/)
- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)