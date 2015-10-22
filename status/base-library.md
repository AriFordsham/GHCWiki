# Roadmap/Plans for GHC's `base` library


This page aims to provide a concise summarized view of major planned changes for upcoming GHC releases related to the `base` library (including related GHC facilities such warnings).

## GHC 8.0 (`base-4.9`)

- Add `Data.Semigroup` and `Data.List.NonEmpty` modules

  ([\#10365](https://gitlab.haskell.org//ghc/ghc/issues/10365), i.e. Phase1 of [Proposal/SemigroupMonoid](proposal/semigroup-monoid))
- Add `-Wcompat` warnings about missing Semigroup instances.
- Add `Control.Monad.Fail` and a `MonadFail` language pragma. (Phase 1 of [ https://wiki.haskell.org/MonadFail_Proposal](https://wiki.haskell.org/MonadFail_Proposal))
- Add `-Wcompat` warnings about missing `MonadFail` instances.
- Add `expm1`, `log1p`, `log1pexp`, `log1mexp` to `Floating` (with defaults).

## GHC 8.2

- Nothing planned.

## GHC 8.4

- Turn the `Semigroup` and `MonadFail` warnings into warnings that are on by default.
- Warn about the pending removal of the default definitions for `log1p`, etc.

## GHC 8.6

- Turn on `MonadFail` by default.
- Move `Semigroup` into `Prelude`.
- Remove the default definitions of  `log1p`, etc.

## GHC 8.8

- Nothing planned.