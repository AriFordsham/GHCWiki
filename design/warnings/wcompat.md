# `-Wcompat` warnings


With GHC 8.0 we have implemented a new warning-group `-Wcompat` (see
[\#11000](https://gitlab.haskell.org//ghc/ghc/issues/11000)) which includes warnings that will be enabled by default in the
future, but remain off in normal compilations for the time
being. This allows library authors eager to make their code future
compatible to adapt to new features before they even generate
warnings, and even later turn into actual compile errors.


Of course, in order for this new group to be effective in bringing coming changes to light, people must to use it. One way to encourage this would be to add `-Wcompat` to the widely used `-Wall` group.


This page describes some arguments for and against including `-Wcompat` in `-Wall`. We refer to these options as,

1. Opt-in style  (`-Wall` does **not include**`-Wcompat`):

  - Users who desire warnings about upcoming changes would use `-Wall -Wcompat`
  - Users who dislike such warnings could use `-Wall`

1. Opt-out style (`-Wall`**includes**`-Wcompat`):

  - Users who desire warnings about upcoming changes could use `-Wall`
  - Users who dislike such warnings would use `-Wall -Wno-compat`

## Arguments **for (B) opt-out** / against opt-in style

<table><tr><th>Improved discoverability of `-Wcompat`</th>
<td>
Most users know mostly about `-Wall` but not about
`-Wcompat`, and even if `-Wcompat` becomes better known, maybe they
won't bother (or simply forget) to turn on `-Wcompat`.
</td></tr></table>

>
> The most effective way to reach everybody is by enabling `-Wcompat`
> by default and have them opt-out if they don't like it.

<table><tr><th>The additional warnings won't cause build issues unless `-Werror` is enabled</th>
<td>
This means that the change won't cause problems for Hackage since Hackage rejects packages with `-Werror` anyway).
</td></tr></table>

>
> In requesting `-Wall` the user is requesting an inclusive set of warnings that may not be stable over compiler version.

<table><tr><th>The user may expect that `-Wall` enables all warnings known to GHC</th>
<td>
Adding more ad-hoc exceptions than already exist further breaks this expectation.
</td></tr></table>

## Arguments **for (A) opt-in** / against opt-out style

<table><tr><th>Users of `-Wall` will need Cabal file boilerplate to disable `-Wcompat`</th>
<td>
Packages that insist on `-Wall` cleanliness while supporting pre-GHC-8.0 need to add boilerplate
in their cabal files to silence compatibility warnings:

```wiki
  ghc-options: -Wall
  if impl(ghc >` 8)
     ghc-options: -Wno-compat
```

However, as [ previously](https://mail.haskell.org/pipermail/ghc-devs/2016-January/010955.html|stated),
`-Wall` is intended for use during development. Therefore, we discourage the use of `-Wall`
in released projects.
</td></tr></table>

<table><tr><th>Inclusion in `-Wall` raises the bar for including new warnings in `-Wcompat`</th>
<td>
Having `-Wcompat` separate from `-Wall` allows us to include
more verbose warnings to `-Wcompat` that would be questionable in `-Wall`</td></tr></table>

<table><tr><th>`-Wcompat` warnings aren't necessarily actionable</th>
<td>`-Wcompat` warnings aren't necessarily actionable if backwards
compatibility is desired; if they were, they'd be in `-Wall`. The
point of `-Wcompat` was to give notice to folks who wanted them as soon
as possible, even if they were things they couldn't do, yet moving
them into `-Wall` means that this whole thing becomes a big mess of
active maintenance
</td></tr></table>