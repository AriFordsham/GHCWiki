# Native `{-# LANGUAGE CPP #-}`


The status of this proposal is tracked by #14553.

## Problem Statement

Currently, GHC relies on the system-installed [C-preprocessor](http://en.wikipedia.org/wiki/C_preprocessor) (lateron referred to as system-`cpp`) accompanying the C compiler for implementing `{-# LANGUAGE CPP #-}`. However, this has several drawbacks:


- We already have a couple of tickets filed w/ the ~cpp label

- Fragile semantics, as the "traditional mode" in `cpp` GHC relies on is not well-specified, and therefore implementations disagree in subtle but annoying ways

  - Consider all the Clang-issues GHC experienced when Apple switched from the GCC toolchain to the Clang toolchain
  - Packages using `-XCPP` only tested with one system-`cpp` variant may not work with another system-`cpp` which either means more testing-cost and/or support-costs
  - Clang cpp does not support stringize and string concatenation in traditional mode (see ticket #12516)

- As system-`cpp` is designed to handle mostly C-code, it conflicts with Haskell's tokenization/syntax, specifically:

  - Haskell-multi-line string literals can't be used anymore with `-XCPP` (c.f. [SO Question](http://stackoverflow.com/questions/2549167/cpp-extension-and-multiline-literals-in-haskell) and/or #10230)
  - Haddock comments get mangled as system-`cpp` isn't aware of Haskell comments
  - system-`cpp` may get confused about "unterminated" `'`s even though in Haskell they are not always used for quoting character literals. For example, Haskell allows variable names like `x'` or even `x'y`.  Another practical example from the [int-cast](http://hackage.haskell.org/package/int-cast) package, in the following code

    ```
    #if defined(WORD_SIZE_IN_BITS)
    type instance IntBaseType Int    = 'FixedIntTag  WORD_SIZE_IN_BITS
    type instance IntBaseType Word   = 'FixedWordTag WORD_SIZE_IN_BITS
    #else
    # error Cannot determine bit-size of 'Int'/'Word' type
    #endif
    ```

    GNU `cpp` fails to macro-expand `WORD_SIZE_IN_BITS` due to the unterminated `'`-quote
  - Valid Haskell operators such as `/*`, `*/` or `//` are misinterpreted by system-`cpp` as comment-starters
  - Unix She-bang (`#!/usr/bin/env runghc`) Haskell scripts can't be used with `-XCPP` (c.f. [SO Q](http://stackoverflow.com/questions/8177950/how-can-i-load-a-runhaskell-script-without-a-hs-extension-with-ghci))
  - One case involving a comment containing `C:\\...` had an unexpected side-effect: [https://github.com/haskell/cabal/pull/3810/commits/7a8062b9219c6353c18e31188cdbd38249578ab0](https://github.com/haskell/cabal/pull/3810/commits/7a8062b9219c6353c18e31188cdbd38249578ab0)

- Lack of ability to extend/evolve `-XCPP` as we have no control over system-`cpp`

## Possible Course of Actions


### Plan 0: No change (i.e. keep using relying on system-`cpp`)



Nothing is gained, but since the issue remains unsolved, we may risk to become pressed for time (and/or cause GHC release delays) if the circumstances change suddenly and force us to act (e.g. if GCC's or Clang's `cpp` change in an incompatible way for GHC).


### Plan 1: Use custom fixed `cpp` implementation bundled with GHC


- One candidate would be the C-implemented `tradcpp` (see [http://www.freshports.org/devel/tradcpp/](http://www.freshports.org/devel/tradcpp/))

- Clang's `cpp` could be another candidate (as suggested [here](http://permalink.gmane.org/gmane.comp.lang.haskell.cafe/116403)). Needs more investigation

- Probably not as easy to extend/evolve to be more Haskell-syntax-aware

### Plan 2: Embed Malcom's [hackage:cpphs](http://hackage.haskell.org/package/cpphs) into GHC



**Advantages**


- `cpphs` has been widely used, hence it's proven code
- It's already more Haskell-aware than system-`cpp`
- `cpphs` is actively maintained
- no more `fork(2)/exec(2)` ([avoids substantial overhead on Windows](http://permalink.gmane.org/gmane.comp.lang.haskell.ghc.devel/8869))


**Disadvantages**


- `cpphs` is licensed as "LGPLv2 w/ static linking exception" (see below)

  - GHC's total licence agreement getting extended (TODO show concrete change)
  - The `ghc` package would be tainted by this license augmentation. (But no more tainted than it is already, by the LGPL'd GMP library (Gnu multi-precision arithmetic).)

### Plan 3: Write native BSD-licenced Haskell implementation from scratch



**Advantages**


- no more `fork(2)/exec(2)` ([avoids substantial overhead on Windows](http://permalink.gmane.org/gmane.comp.lang.haskell.ghc.devel/8869))
- Tailored to GHC's needs


**Disadvantages**


- Requires manpower and time
- Additional long-term maintenance effort for GHC-HQ

### Plan 3a: Embed [hackage:hpp](http://hackage.haskell.org/package/hpp) into GHC



Since this wiki page was first written, [hackage:hpp](http://hackage.haskell.org/package/hpp) has been written, which is BSD3 licensed.


### Plan 4: Bundle `cpphs`-based executable with GHC



This is a variant of plan 2 where `cpphs` code remains in a separate executable.



**Advantages**


- `cpphs` has been widely used, hence it's proven code
- It's already more Haskell-aware than system-`cpp`
- `cpphs` is actively maintained
- ~~no more `fork(2)/exec(2)`~~


**Disadvantages**


- `cpphs` is licensed as "LGPLv2 w/ static linking exception" (see below)

  - GHC's total licence agreement getting extended (TODO show concrete change)
  - ~~The `ghc` package would be tainted by this license augmentation~~

## `cpphs`'s licence in more detail


- The main intent behind `cpphs`'s current licensing is to have modifications/improvements of redistributed `cpphs` binaries made publicly available to recipients of the binaries (so that they can be e.g. merged upstream if useful). This is a concern the BSD3 licence doesn't address.

- The library portion of the `cpphs` is dual-licensed (see [http://code.haskell.org/cpphs/COPYRIGHT](http://code.haskell.org/cpphs/COPYRIGHT)):

  - [LGPL v2.1](https://www.gnu.org/licenses/lgpl-2.1.html) with static linking exception

    >
    >
    > As a relaxation of clause 6 of the LGPL, the copyright holders of this library give permission to use, copy, link, modify, and distribute, binary-only object-code versions of an executable linked with the original unmodified Library, without requiring the supply of any mechanism to modify or replace the Library and relink (clauses 6a, 6b, 6c, 6d, 6e), provided that all the other terms of clause 6 are complied with.
    >
    >

  - for binary distributions only: [http://code.haskell.org/cpphs/LICENCE-commercial](http://code.haskell.org/cpphs/LICENCE-commercial) (doesn't seem useful for GHC)

- As a practical consequence of the *LGPL with static-linking-exception* (LGPL+SLE), **if no modifications are made to the `cpphs`-parts** (i.e. the LGPL+SLE covered modules) of the GHC code-base, **then there is no requirement to ship (or make available) any source code** together with the binaries, even if other parts of the GHC code-base were modified.

- LGPL w/ static linking exception is sometimes used: cf. ZeroMQ [http://zeromq.org/area:licensing](http://zeromq.org/area:licensing)

- [http://programmers.stackexchange.com/questions/179084/is-there-a-modified-lgpl-license-that-allows-static-linking](http://programmers.stackexchange.com/questions/179084/is-there-a-modified-lgpl-license-that-allows-static-linking)

- [wikipedia:GPL_linking_exception](http://en.wikipedia.org/wiki/GPL_linking_exception)

### `ghc` package's current license



The `ghc` package which can be linked into programs currently depends on the packages
`array`, `base`, `binary`, `bin-package-db`, `bytestring`, `containers`, `deepseq`, `directory`, `filepath`, `ghc-prim`, `hoopl`, `hpc`, `integer-gmp`, `pretty`, `process`, `rts`, `template-haskell`, `time`, `transformers`, and `unix` whose collated `LICENSE` have been pasted as [http://lpaste.net/131294](http://lpaste.net/131294)


