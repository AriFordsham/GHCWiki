# Redesigned GHC Warnings


TLDR: Borrow some ideas from GCC/Clang's warning-related CLI for GHC.

## Current Situation (GHC 8.0)


GHC currently uses a somewhat unsatisfying warning CLI:

```wiki
-W      (enable normal warnings)
-w      (disable all warnings)
-Wall   (enable almost all warnings)
-Werror (make warnings fatal)
-Wwarn  (make warnings non-fatal)

-Wduplicate-exports
-Widentities
-Wmissing-signatures
-Wunused-binds
...
```

## Proposed Change

TODO needs more elaboration & motivation


By reusing the GCC CLI convention for warning-flags we can make GHC's CLI a bit more intuitive to people used to GCC (& Clang's) CLI.

- ([\#11218](https://gitlab.haskell.org//ghc/ghc/issues/11218) - implemented in 8.0) Keep the current `-f(no-)warn-$WARNTYPE` flags as hidden flag aliases for newly introduced -W(no-)$WARNTYPE\` flags more in line with GCC's conventions, e.g.

  - `-Worphans` instead of `fwarn-orphans`
  - `-Wno-missing-methods` instead of `-fno-warn-missing-methods`

- ([\#11219](https://gitlab.haskell.org//ghc/ghc/issues/11219)) Introduce variant of `-Werror` (c.f. GCC's `-Werror=*`) which allows to specify the individual warnings to be promoted to errors, e.g.

  - `-Wall -Werror=orphans` would only promote `-Worphans` warnings into errors
  - `-Wall -Werror -Wno-error=missing-methods` would promote all warnings *except*`-Wmissing-methods` into errors

- Introduce some warning sets, e.g.

  - ([\#11000](https://gitlab.haskell.org//ghc/ghc/issues/11000)) `-Wcompat` could refer to all warnings about future compatility GHC *currently* knows about (like e.g. `-Wcompat-amp`, `-Wcompat-mfp`, `-Wcompat-mrp`)
  - Have `ghc` provide a way to dump the current warning-sets (in a format that's parseable by humans and machines)
  - Define set `-Wstandard` (modulo bikeshed, maybe `-Wdefault`?) to denote the set of warnings on by default, together with its negation `-Wno-standard`
  - Define set `-Weverything` (c.f. clang's [ -Weverything](http://clang.llvm.org/docs/UsersManual.html#diagnostics-enable-everything) as precedent) to comprise really \*all\* warnings (together with its negation `-Wno-everything` for symmetry, which is a synonym for `-w`)
  - Define set `-Wextra` (modulo bikeshed, maybe `-Wnormal`?) as synonym for `-W`, together with its negation `-Wno-extra`

- ([\#10752](https://gitlab.haskell.org//ghc/ghc/issues/10752)) When emitting warnings/errors, show which warning flag was responsible,
  e.g.

  ```wiki
  foo.hs:1:1: Warning:  [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  ```

  making it easier to silence specific warnings via e.g. `-Wno-missing-signatures`