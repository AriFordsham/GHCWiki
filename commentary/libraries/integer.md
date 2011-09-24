# GHC Commentary: [Libraries/Integer](commentary/libraries/integer)

TODO

## Selecting an Integer implementation


You can select which implementation of Integer is used by defining `INTEGER_LIBRARY` in `mk/build.mk`. The code is in `libraries/$(INTEGER_LIBRARY)`.


The default value is `integer-gmp`, which uses the [ GNU Multiple Precision Arithmetic Library (GMP)](http://gmplib.org/) to define the Integer type and its operations.


The other implementation currently available is `integer-simple`, which uses a simple (but slow, for larger Integers) pure Haskell implementation.

## The Integer interface

TODO

## How Integer is handled inside GHC

TODO