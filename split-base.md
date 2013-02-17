## Splitting base


In a [ thread on glasglow-haskell-users](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023764.html) in February some ideas about splitting base in smaller components were floating around. This wiki page tries to assemble ideas on how to re-group the modules.


The following is a list of all modules in Base, with a suggested re-grouping. Whether this makes sense WRT interdependencies has not yet been verified:

### Non-Obvious interdependencies


This is a list of interdependencies between seemingly unrelated parts that need to be taken into consideration:

- class Monad mentions `String`, hence pulling Char
- class Monad mentions `error` and `Data.Int` requires `throw DivideByZero`, hence pulling in exceptions
- Exceptions pull in `Typeable`
- `Typeable` pulls in `GHC.Fingerprint`
- GHC.Fingerprint pulls in `Foreign` and `IO` (but could be replaced by a pure implementation)
- The Monad instance of `IO` calls `failIO`, which creates an `IOException`, which has fields for handles and devices, and hence pulls in some `Foreign` stuff and some file-related `IO`, preventing the creation of a clean base-io package.

### Other issues

- Some names of base are hardcoded in GHC and hence cannot be moved to a different package name without changes in GHC. This includes:

  - The `Num` constraint on polymorphic literals. Can be avoided by writing `fromIntegral 0` instead of `0`.
  - Similar, the `[x..y]` syntax generates a `base:GHC.Enum.Enum` constraint, `RebindableSyntax` does not help (GHC bug?)
  - `StablePtr`, as used in `GHC.Stable`
  - `Typeable`, `Show` when used in `deriving`. Can probably be avoided by hand-writing instances. `Read` can probably move completely out.
  - `error` has its type wired in GHC when in package base; This is used in a hack in [ GHC/Err.hs-boot](https://github.com/ghc/packages-base/blob/master/GHC/Err.lhs-boot). Work-around: Import `GHC.Types` in `GHC/Err.lhs-boot`
  - The `Monad` constraint on do-notation expects the definition to live in base. `RebindableSyntax` helps, but requires to define a local `ifThenElse` function.
- The ST Monad can (and should) be provided independently of IO, but currently functions like `unsafeIOToST` are provided in the `Control.Monad.ST` namespace.

### First attempt


Joachim has started a first attempt to pull stuff out of the bottom of base; these chunks often contain more than initially intended:

- [ base-pure](https://github.com/nomeata/packages-base/tree/base-pure) Basic stuff without `IO`, `Foreign` or floating point arithmetic. Requires reimplementing `GHC.Fingerprint` without using FFI (or at least without using FFI types and without `IO`).
- [ base-io](https://github.com/nomeata/packages-base/tree/base-io) (uses base-pure). The `IO` and `ST` monads. Unfortunately pulls in `Handle`-related stuff via `IOException`.
