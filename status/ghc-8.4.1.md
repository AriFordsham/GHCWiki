# GHC plans for 8.4.1


This page is our road-map for what will be in 8.4.  


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Release in February 2018. Cut release branch in November 2017.

## Libraries Status


See [Libraries](status/ghc-8.4.1/libraries) and [Migration/8.4](/migration/8.4).

## Release highlights (planned)


Below are the major highlights of 8.4.

- Improved support for **cross-compilation** (Moritz Angermann)

### Build system and miscellaneous changes

- Improved Windows support, including support for split sections and long file paths (Tamar Christina)
- Support for building stating libraries for elf and mach-o (`-staticlib`)

## Landed in `master` branch

- Improved code generation for join points
- Many, many bug-fixes

### Library changes

- Phase 2 of the [Semigroup-Monoid Proposal](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid) (Herbert Riedel)

### Build system and miscellaneous changes

- iserv can be used over the network via iserv-proxy
- llvm backend uses LLVM5
- **New Shake-based build system, `hadrian`, will be merged.**  (Andrey Mokhov)
- **Remove dependency on Hoopl package.**  (Michal Terepeta, [Phab:D3616](https://phabricator.haskell.org/D3616))

## Tickets

See the %8.4.1 milestone.
  

