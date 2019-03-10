# Shorter import syntax


This page describes a proposed variant of Haskell's `import` declarations.


Trac ticket is [\#10478](https://gitlab.haskell.org//ghc/ghc/issues/10478).


Link to discussions on

- Haskell cafe discussion thread
- Analysis of public code to characterize how common the confusing case would be months ago.

## Specification

```wiki
 import modid maybeImpspec maybeAs
```


Semantics are the same as existing imports unless both an `impspec` (i.e.
a parenthesised list of identifiers optionally prefixed with the `hiding`
keyword) and an `as` (e.g. `as M`) are present. In such a case, all  identifiers exported by `modid` are accessible behind the prefix given by  the `as`.

## Examples


...
