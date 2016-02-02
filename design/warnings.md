# Redesigned GHC Warnings


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


By reusing the GCC CLI convention for warning-flags, we can make GHC's CLI a bit more intuitive to people used to GCC (& Clang's) CLI.

## Changes to be implemented, and timing

**Already implemented in GHC 8.0:**

- ([\#11218](https://gitlab.haskell.org//ghc/ghc/issues/11218)) Keep the current `-f(no-)warn-$WARNTYPE` flags as hidden flag aliases for newly introduced -W(no-)$WARNTYPE\` flags more in line with GCC's conventions, e.g.

  - `-Worphans` instead of `fwarn-orphans`
  - `-Wno-missing-methods` instead of `-fno-warn-missing-methods`

  This is already done in GHC 8.0.

- ([\#11429](https://gitlab.haskell.org//ghc/ghc/issues/11429)) Make unrecognised `-W` flags a warning (`-Wunrecognised-warning-flags`) rather than an error. 

- ([\#11370](https://gitlab.haskell.org//ghc/ghc/issues/11370)) Remove `warn-redundant-constraints` from the default constraint set and the `-Wall` constraint set

- ([\#11451](https://gitlab.haskell.org//ghc/ghc/issues/11451)) Split off `-Wunused-foralls` and `-Wunused-type-patterns` from `-Wunused-matches`. Make `-Wall` imply `-Wunused-foralls` and `-Wunused-type-patterns`, but *not* imply `-Wunused-type-patterns`

**Proposed for GHC 8.0:**

- ([ phab:D1850](https://phabricator.haskell.org/D1850)) Introduce some new warning sets, e.g.

  - Define set `-Wstandard` (modulo bikeshed, maybe `-Wdefault`?) to denote the set of warnings on by default, together with its negation `-Wno-standard`
  - Define set `-Weverything` (c.f. clang's [ -Weverything](http://clang.llvm.org/docs/UsersManual.html#diagnostics-enable-everything) as precedent) to comprise really \*all\* warnings (together with its negation `-Wno-everything` for symmetry, which is a synonym for `-w`)
  - Define set `-Wextra` (modulo bikeshed, maybe `-Wnormal`?) as synonym for `-W`, together with its negation `-Wno-extra`
  - ([\#11000](https://gitlab.haskell.org//ghc/ghc/issues/11000)) Define set `-Wcompat` to denote all warnings about future compatility GHC *currently* knows about (like e.g. `-Wcompat-amp`, `-Wcompat-mfp`, `-Wcompat-mrp`)  In addition, have `ghc` provide a way to dump the current warning-sets (in a format that's parseable by humans and machines)

**Anytime someone is motivated**

- ([\#11219](https://gitlab.haskell.org//ghc/ghc/issues/11219)) Introduce variant of `-Werror` (c.f. GCC's `-Werror=*`) which allows to specify the individual warnings to be promoted to errors, e.g.

  - `-Wall -Werror=orphans` would only promote `-Worphans` warnings into errors
  - `-Wall -Werror -Wno-error=missing-methods` would promote all warnings *except*`-Wmissing-methods` into errors

- ([\#10752](https://gitlab.haskell.org//ghc/ghc/issues/10752)) When emitting warnings/errors, show which warning flag was responsible,
  e.g.

  ```wiki
  foo.hs:1:1: Warning:  [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  ```

  making it easier to silence specific warnings via e.g. `-Wno-missing-signatures`

- Introduce `-Wpedantic` (this is subsumed by `-Weverything` already?), which turns on warnings that `-Wall` doesn't turn on (e.g., `-Wredundant-constraints` and `-Wunused-type-patterns`)

## Intended usage of warnings


Here is the proposed warning framework:

- Intended meanings

  - `-Wdefault`: the ones you get by default. This indicate things that are almost certainly wrong.
  - `-Wall`: indicate suspicious code
  - `-Wcompat`: indicate code that in future releases may trigger errors or `-Wdefault` warnings.  Subset of `-Wall`.

- With no flags, GHC implements the "default warnings". You can get these by saying `-Wdefault` (yet to be implemented).

- When GHC implements a new warning it is put into the `-Weverything` set.  In the next release, that warning may move into the `-Wall` set or the `-Wdefault` set.  So libraray authors may want to future-proof their libraries by compiling with `-Weverything`.

- A flag should only move into `-Wall` or `-Wdefault` if there is a reasonable way for a library author to change their source code to avoid the warning. Experimental, unpredicatable, or compiler-writer-guidance warnings should not be in `-Wall`.

- Libraries on Hackage should not be distributed with `-Werror`, so that any warnings do not abort compilation.  Authors may want to use `-Wall` to help encourage other contributors to improve the library to avoid the warning; but `-Wdefault` is more typical usage.

### To `-Wcompat` ⊂ `-Wall`, or not? ([\#11494](https://gitlab.haskell.org//ghc/ghc/issues/11494))


With GHC 8.0 we have implemented a new warning-group `-Wcompat` (see
[\#11000](https://gitlab.haskell.org//ghc/ghc/issues/11000)) to comprise warnings that will be enabled by default in the
future, but remain off in normal compilations for the time
being. This allows library authors eager to make their code future
compatible to adapt to new features before they even generate
warnings, and even later probably turn into actual compile errors.


However, there is one important design choice left regarding the default:

1. Opt-in style  (`-Wall` does **not include**`-Wcompat`):

  - Users who desire warnings about upcoming changes: `-Wall -Wcompat`
  - Users who dislike such warnings: `-Wall`

1. Opt-out style (`-Wall`**includes**`-Wcompat`):

  - Users who desire warnings about upcoming changes: `-Wall`
  - Users who dislike such warnings: `-Wall -Wno-compat`


Arguments **for B. opt-out**/against opt-in style:

- If we don't enable `-Wcompat` by default, discoverability
  suffers. Most users know mostly about `-Wall` but not about
  `-Wcompat`, and even if `-Wcompat` becomes better known, maybe they
  won't bother (or simply forget) to turn on `-Wcompat`.

>
> The most effective way to reach everybody is by enabling `-Wcompat`
> by default and have them opt-out if they don't like it.

- This warnings are just noisy and don't cause actual problems until
  `-Werror` is enabled (which doesn't cause problems for Hackage since
  Hackage rejects packages with `-Werror` anyway).

>
> If somebody enables `-Wall`, they're already asking for some noise.
> Especially since `-Wall`'s effect changes with each GHC release.


Arguments against opt-out/**for A. opt-in** style:

- Opting out while supporting pre GHC8 needs to actively add boilerplate
  in cabal files to silence GHC:

  ```wiki
    ghc-options: -Wall
    if impl(ghc >` 8)
       ghc-options: -Wno-compat
  ```

- Having `-Wcompat` separate from `-Wall` allows us to include
  more noisy warnings to `-Wcompat` that would be questionable in `-Wall`

- `-Wcompat` warnings aren't necessarily actionable if backwards
  compatibility is desired; if they were, they'd be in `-Wall`. The
  point of `-Wcompat` was to give notice to folks who wanted them as soon
  as possible, even if they were things they couldn't do, yet moving
  them into `-Wall` means that this whole thing becomes a big mess of
  active maintenance

- If `-Wcompat` was on by default, less or maybe no users at all would
  bother about the 3-release policy at all, as it would add another
  hoop to jump through, i.e.  requires to actively opt out via
  `-Wno-compat`.
