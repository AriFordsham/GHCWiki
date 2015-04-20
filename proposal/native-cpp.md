# Native `{-# LANGUAGE CPP #-}`

## Problem Statement


Currently, GHC relies on the system-installed [ C-preprocessor](http://en.wikipedia.org/wiki/C_preprocessor) (lateron referred to as system-`cpp`) accompanying the C compiler for implementing `{-# LANGUAGE CPP #-}`. However, this has several drawbacks:

- Fragile semantics, as the "traditional mode" GHC relies on is not well-specified, and therefore implementations disagree in some cases

  - Consider all the Clang-issues GHC experienced when Apple switched from the GCC toolchain to the Clang toolchain
  - Packages using `-XCPP` only tested with one system-`cpp` variant may not work with another system-`cpp` which either means more testing-cost and/or support-costs

- As system-`cpp` is designed to handle mostly C-code, it conflicts with Haskell's tokenization/syntax, for instance:

  - Haskell-multi-line string literals can't be used anymore with `-XCPP`
  - Haddock comments get mangled as system-`cpp` isn't aware of Haskell comments
  - system-`cpp` may complain about "unterminated" `'`s even though in Haskell they are not always used for quoting character literals (e.g. `data Foo = Foo' ()`)
  - Some valid Haskell operators such as `/*`, `*/` or `//` are misinterpreted by system-`cpp`

- Lack of ability to extend/evolve `-XCPP` as we have no control over system-`cpp`

## Possible Course of Actions

### Plan 0: No change (i.e. keep using relying on system-`cpp`)


Nothing is gained, but since the issue remains unsolved, we may risk to become pressed for time the circumstances force us to act (e.g. if GCC's or Clang's `cpp` change in an incompatible way for GHC)

### Plan 1: Use custom fixed `cpp` implementation bundled with GHC


One candidate would be `tradcpp` (see [ http://www.freshports.org/devel/tradcpp/](http://www.freshports.org/devel/tradcpp/))

- Probably not easy to extend/evolve to be more Haskell-syntax-aware

### Plan 2: Embed Malcom's `cpphs` into GHC

- `cpphs` is licensed as "LGPL w/ static linking exception" (similar to e.g. ZeroMG, [ http://zeromq.org/area:licensing](http://zeromq.org/area:licensing)); this would result in GHC's total licence agreement getting extended (TODO show concrete change)

- `cpphs` has been widely used, hence it's proven code

- It's already more Haskell-aware than system-`cpp`

- `cpphs` is actively maintained

### Plan 3: Write native BSD-licenced Haskell implementation from scratch

- Requires menpower and time

- Additional long-term maintenance effort for GHC-HQ
