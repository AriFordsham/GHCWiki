# Why we should split off usage analysis from strictness/termination analysis

- [\#16284](https://gitlab.haskell.org/ghc/ghc/issues/16284)
- the cases where usage analysis takes long (i.e. needs many iterations to stabilise) seldom overlap with the cases where strictness analysis takes long, but we need to recompute already stable analysis results nonetheless
- lazy_fv
- Strictness is better off with LetDown
- Separation of concerns
