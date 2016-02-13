# `-Wcompat`


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

#### Arguments **for B. opt-out**/against opt-in style

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

#### Arguments against opt-out/**for A. opt-in** style

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
