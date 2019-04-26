# GHC plans for 8.6.1

This page is our road-map for what will be in 8.6.  


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Cut release branch in June 2018. Release in August 2018.

## Libraries Status


See Libraries? and [Migration/8.6](/migration/8.6).

## Release highlights (planned)


Below are the major highlights of 8.6.

### Build system and miscellaneous changes

- Improved Windows support, including support for split sections, and long file paths support (no MAX_PATH restriction) (Tamar Christina)

- Better handling of LOAD COMMAND SIZE Limits on macOS via `-dead_strip_dylibs` (Moritz Angermann)

## Landed in `master` branch

- Deriving via ([proposal](https://github.com/Icelandjack/ghc-proposals/blob/239cfc8ef532db95f15ea392e073061f04273d8e/proposals/0000-deriving-via.rst), Ryan GL Scott)

- An early version of the GHCi `:doc` command

- [QuantifiedConstraints](quantified-constraints)

- The core functionality of the `ghc-heap-view` package has been merged into GHC, allowing introspection into the structure of GHC's heap. (Patrick Dougherty, [Phab:D3055](https://phabricator.haskell.org/D3055))

- Many improvements to exhausiveness checking (#14546)

- Valid hole fits (#14969, #14990, #10946)

- Improvements in code generation, include a new SRT representation that results in more compact generated code ([blog post](http://simonmar.github.io/posts/2018-06-22-New-SRTs.html))

- Further improvements to DWARF unwinding support

- More sophisticated constant folding (#9136)

- BlockArguments extension (#10843, [proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0010-block-arguments.rst))

- NumericUnderscores extension (#14473, [proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0009-numeric-underscores.rst))

- StarIsType extension ([proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0020-no-type-in-type.rst), Vladislav Zavialov)

- The next phase of the `MonadFail` proposal, enabling the `MonadFailDesugaring` extension by default

### Library changes


### Build system and miscellaneous changes


## Tickets

See the %8.6.1 milestone.
  
