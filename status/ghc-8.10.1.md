# GHC plans for 8.10.1


This page is our road-map for what will be in GHC %8.10.1.


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates

 - October  18 2019:  start of one week freeze in preparation for branching
 - October  25 2019:  ghc-8.10 branch cut
 - November 24 2019:  8.10.1-alpha1
 - December 11 2019:  8.10.1-alpha2
 - December 6  2019:  8.10.1-alpha3
 - January  2  2019:  8.10.1-rc1
 - January  12 2020:  Final 8.10.1 release


## Libraries Status


See Libraries? and [Migration/8.10](/migration/8.10).

## Release highlights (planned)


Below are the major highlights of the 8.10 series.

### Compiler

- An improved code layout algorithm (!616) (@AndreasK)
- Standalone kind signatures (!1438) (@int-index)
- Fix exponential typechecking time for large rational numbers (#15646)
- Ship stage1 libraries with HIE files (!1337)
- Tagging of large constructor families (#14373, !1742)
- Don't emit custom TypeErrors that appear as given constraints (!1730)
- Internal module hierarchy reorganization (#13009)
- Fix obscure but awful bug affecting compacting GC (#17088, !1747; depends upon !1742)
- Unlifted newtypes (#15219, !364)

### Runtime system

 - A new low-latency garbage collector (!972) (@bgamari and @osa1)
 - Implement interfaces needed by [ghc-debug](https://github.com/bgamari/ghc-debug) (!1435)

### Build system and miscellaneous changes

- [Reinstallable lib:ghc](https://mail.haskell.org/pipermail/ghc-devs/2017-July/014424.html)
- The Hadrian build system will hopefully become the default

## Landed in `master` branch


### Library changes


### Build system and miscellaneous changes


## Tickets

See the %8.10.1 milestone.

See also [GHC 8.10 tracking ticket](https://gitlab.haskell.org/ghc/ghc/issues/17214).


