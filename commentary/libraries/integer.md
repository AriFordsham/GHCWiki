# GHC Commentary: [Libraries/Integer](commentary/libraries/integer)


GHC is set up to allow different implementations of the `Integer` type to be chosen at build time.

## Selecting an Integer implementation


You can select which implementation of Integer is used by defining `INTEGER_LIBRARY` in `mk/build.mk`. This tells the build system to build the library in `libraries/$(INTEGER_LIBRARY)`, and the `cIntegerLibrary` and `cIntegerLibraryType` values in `Config.hs` are defined accordingly.


The default value is `integer-gmp`, which uses the [ GNU Multiple Precision Arithmetic Library (GMP)](http://gmplib.org/) to define the Integer type and its operations.


The other implementation currently available is `integer-simple`, which uses a simple (but slow, for larger Integers) pure Haskell implementation.

## The Integer interface


All Integer implementations should export the same set of types and functions from `GHC.Integer`. These exports are used by the `base` package However, all of these types and functions must actually be defined in `GHC.Integer.Type`, so that GHC knows where to find them.

## How Integer is handled inside GHC


Integers are represented using the `HsInteger` constructor of `HsLit` for the early phases of compilation (e.g. type checking), but for later stages, once we use the `Core` representation, they are converted to the `LitInteger` constructor of the `Literal` type by `mkIntegerExpr`. While `Integer`s aren't "machine literals" like the other `Literal` constructors, it is more convenient when writing rules to pretend that they are literals rather than having to understand their real core representation. We also carry around a `Type`, representing the `Integer` type, in the constructor, as we need access to it in a few functions (e.g. `literalType`).


All of the types and functions in the `Integer` interface have built-in names, e.g. `plusIntegerName`, defined in [compiler/prelude/PrelNames.lhs](/trac/ghc/browser/ghc/compiler/prelude/PrelNames.lhs) and included in `basicKnownKeyNames`. This allows us to match on all of the functions in `builtinIntegerRules` in [compiler/prelude/PrelRules.lhs](/trac/ghc/browser/ghc/compiler/prelude/PrelRules.lhs), so we can constant-fold Integer expressions.


We keep the `LitInteger` representation as late as possible; in particular, it's important that this representation is used in unfoldings in interface files, so that constant folding can happen on expressions that get inlined. We only convert it to a proper core representation of Integer in [compiler/coreSyn/CorePrep.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CorePrep.lhs), which looks up the Id for `mkInteger` and uses it to build an expression like `mkInteger True [123, 456]` (where the `Bool` represents the sign, and the list of `Int`s are 31 bit chunks of the absolute value from lowest to highest).


However, there is a special case for `Integer`s that are within the range of `Int` when the `integer-gmp` implementation is being used; in that case, we use the `S#` constructor (via `integerGmpSDataCon` in [compiler/prelude/TysWiredIn.lhs](/trac/ghc/browser/ghc/compiler/prelude/TysWiredIn.lhs)) to break the abstraction and directly create the datastructure.


Most of the functions in the Integer implementation are marked `NOINLINE`. This is because inlining them is generally not beneficial (any constant folding is already handled by the built-in rules), and in fact can be harmful: In the GMP representation, each argument can be one of two constructors (`S#` and `J#`), which leads to 2 branches. When you have a number of `Integer` arithmetic operations, you can get an exponential code explosion if they all get inlined.
