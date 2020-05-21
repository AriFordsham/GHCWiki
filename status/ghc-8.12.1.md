# GHC plans for 8.12.1

This page is our road-map for what will be in GHC %8.12.1.

If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates

* 16 June 2020: Beginning of feature freeze
* Late June 2020: Cut branch
* July - August 2020: Alpha releases
* 1 September 2020: Beta release
* 25 September 2020: Final 8.12.1 release


## Libraries Status

See Libraries? and [Migration/8.12](/migration/8.12).

## Release highlights (planned)

Below are the major highlights of the 8.12 series.

### Compiler

* [ ] `QualifiedDo` (@matt, [Proposal #216](https://github.com/ghc-proposals/ghc-proposals/pull/216))
* [ ] Signalling NaNs (@carter, #16519)
* [ ] NCG register allocation improvements (@AndreasK)
* [ ] Various documentation improvements (@Kleidukos)
* [x] Simplified subsumption (@simonpj)
* [x] Explicit specificity in type variable binders (@gertjan423, #16393)
* [ ] `keepAlive#` (#17760, @bgamari)
* [ ] Improvements in code generation for `runRW#` (#15127, @bgamari)
* [x] Refactoring of GHC module hierarchy
* [ ] New Darwin packaging (#17418, #17854, @trac-conradwt)
* [ ] New Windows packaging? (#18104)

### Runtime system

 - [ ] Further improvements to the low-latency garbage collector (@bgamari)
 - [ ] Implement interfaces needed by [ghc-debug](https://github.com/bgamari/ghc-debug) (!1435)

### Build system and miscellaneous changes

- The Hadrian build system will hopefully become the default

## Tickets

See the %8.12.1 milestone.

See also [GHC 8.12 tracking ticket](https://gitlab.haskell.org/ghc/ghc/issues/18216).


