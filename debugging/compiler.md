# Debugging the compiler

## Basic strategies


At compile time (see also the [relevant User Manual section](http://www.haskell.org/ghc/docs/latest/html/users_guide/options-debugging.html)):

- Use `-v3` or `-v4` to get an idea about what GHC is doing when the problem occurs.

- Add `-dcore-lint` the GHC command line when compiling each Haskell module.  This makes GHC type-check the intermediate program after every optimisation pass, which often nails a fault.

- Add `-ddump-simpl` to see the optimised Core output.  There are a number of other `-ddump-x` flags; see the user manual.

- The flag `-dppr-debug` makes the `-ddump-x` flags print much more verbose output.  Use this if you are getting desperate!
