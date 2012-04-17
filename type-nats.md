# Type Level Literals


This page collects information on how to work with type-level literals, as implemented in the Haskell compiler GHC (ticket [\#4385](https://gitlab.haskell.org//ghc/ghc/issues/4385)).

## User's Guide

- [Type-Level Literal Basics](type-nats/basics)
- [Type-Level Computation](type-nats/operations)
- [Typed examinations of TNat (inductive definitions)](type-nats/inductive-definitions)

## Notes on Design

- [Alternative Design For Singletons](type-nats/alternative-singletons)
- [Avoiding Partial Type Functions](type-nats/avoiding-partial-type-functions)
- [Singletons and Existentials](type-nats/singletons-and-existentials)

## Notes on the Implementation

- [Implementation of GHC.TypeLits](type-nats/implementation)
- [ Axioms for type-level type operators](http://github.com/yav/tc-solver/blob/master/docs/axioms.md)

## Source Code

- [ type-nats branch of GHC](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=ghc.git;a=shortlog;h=refs/heads/type-nats)
- [ type-nats branch of the base library](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=packages/base.git;a=shortlog;h=refs/heads/type-nats)
- [ type nats branch of template-haskell](http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=packages/template-haskell.git;a=shortlog;h=refs/heads/type-nats)
- Also, there is a type-nats branch for 'haddock'.

## XXX: Cleanup

- [Natural Numbers: From Values to Types](type-nats/naturals)
- More advanced example: [ https://github.com/yav/memory-arrays/tree/master](https://github.com/yav/memory-arrays/tree/master)
- [Examples](type-nats/examples)