# GHC plans for 8.8.1


This page is our road-map for what will be in 8.8.


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates

- 18 November 2018: Cut release branch
- 25 November 2018: Release alpha1
- 16 December 2018: Release alpha2
- 6 January 2019: Release alpha3
- 27 January 2019: Release alpha4
- 17 February 2019: Release beta1
- 15 March 2019: Final release

## Libraries Status


See Libraries? and [Migration/8.8](/migration/8.8).

## Release highlights (planned)


Below are the major highlights of 8.8.

### Compiler

- A safer and more efficient `with#` combinator to control object lifetime (#14375)
- Improved compilation time for type-family-heavy programs (#8095, [Phab:D4766](https://phabricator.haskell.org/D4766))
- More efficient code generation for nested closures (#14461)
- Next iteration of [Trees That Grow](implementing-trees-that-grow) (tickets/patches for this?)
- Continued focus on performance:

  - Some possible tickets: #15418, #15455, #14980, #14013, #15488, #15519, #14062, #14035, #15176, #15304
  - New codelayout algorithm for the NCG: #15124
  - Optimize based on limited static analysis: #14672
- A late lambda lifting optimisation on STG (#9476)
- More locations where users can write `forall`: [https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0007-instance-foralls.rst](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0007-instance-foralls.rst)

### Build system and miscellaneous changes

- [Reinstallable lib:ghc](https://mail.haskell.org/pipermail/ghc-devs/2017-July/014424.html)
- The Hadrian build system will hopefully become the default

## Landed in `master` branch


### Library changes


### Build system and miscellaneous changes


## Tickets

See the %8.8.1 milestone.
  
  



