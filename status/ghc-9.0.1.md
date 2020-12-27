# GHC plans for 9.0.1

This page is our road-map for what will be in GHC %9.0.1.

If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates

* 16 June 2020: Beginning of feature freeze
* Late June 2020: Cut branch
* Mid september 2020: Alpha releases
* ~24 September 2020: Beta release
* Week of 5th October 2020: Final release candidate


## Migration details

See [Migration/9.0](/migration/9.0).

## Release highlights (planned)

Below are the major highlights of the 9.0 series.

### Compiler

* [ ] Linear Types (@aspiwack, !852, #15981) 
* [ ] `QualifiedDo` (@matt, [Proposal #216](https://github.com/ghc-proposals/ghc-proposals/pull/216))
* [ ] Signalling NaNs (@carter, #16519)
* [ ] NCG register allocation improvements (@AndreasK)
* [ ] Various documentation improvements (@Kleidukos)
* [x] Simplified subsumption (@simonpj)
* [x] Explicit specificity in type variable binders (@gertjan423, #16393)
* [ ] `keepAlive#` (#17760, @bgamari)
* [ ] Improvements in code generation for `runRW#` (#15127, @bgamari)
* [x] Refactoring of GHC module hierarchy (#13009, @hsyl20)
* [ ] New Darwin packaging (#17418, #17854, @trac-conradwt)
* [ ] New Windows packaging? (#18104)
* [x] WinIO IO manager for Windows (@Phyx, @AndreasK)
* [ ] Reduce `FastString` memory consumption (!1675, @dxld)
* [ ] Improved GC behavior due to `ModIface` in compact region (@mpickering)
* [x] Make `Q (TExp a)` a newtype (@mpickering, !3358, [Proposal 195](https://github.com/ghc-proposals/ghc-proposals/pull/216))
* [ ] AArch64 linker fixes (@angerman)
* [x] unified ghc-bignum package (!2231, @hsyl20)

### Runtime system

 - [ ] Further improvements to the low-latency garbage collector (@bgamari)
 - [ ] Implement interfaces needed by [ghc-debug](https://github.com/bgamari/ghc-debug) (!1435)

### Build system and miscellaneous changes

- The Hadrian build system will hopefully become the default (https://gitlab.haskell.org/ghc/ghc/-/milestones/360)
- The GHCi prompt does not display the loaded modules by default anymore (#18702)
- Data.Foldable methods `maximum{,By}`, `minimum{,By}`, `product` and `sum` are now strict by default, as well as the methods used in the List instance (#17867, #4675)


## Tickets

See the %9.0.1 milestone.

See also [GHC 9.0 tracking ticket](https://gitlab.haskell.org/ghc/ghc/issues/18216).
