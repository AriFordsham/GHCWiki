
One possiblity is to replace GMP with an arbitrary-precision integer arithmetic library written entirely in Haskell. There is at least one available:

- [ HIntegerByInt](http://www.haskell.org/pipermail/libraries/2007-August/007909.html) by Isaac Dupree


The downside is speed, of course.  The upside is that (a) you don't need GMP and (b) it's relatively easy to do.


Becuase of the speed issue we can't abandon GMP, so you'd have to give a new flag to GHC, say `-integer-package=my-integer-2.0`.  The intention would be that the compiler would then use the `Integer` library in package `my-integer-2.0` instead of GMP.


Here are some notes on what you'd need to do to achieve this.
You'll have to change three bits:

- The compiler
- The runtime system
- The libraries

## The compiler


GHC knows relatively little about the `Integer` type. Look in `prelude\PrelNames` and search for `Integer`.  These `Names` "know" which module the functions are defined in; look at the defn of `plusIntegerName` for example.  The flag needs to change this defining module, which is a little awkward since `plusIntegerName` is a top-level defn.


This would have to change.  At the places where `plusIntegerName` is looked up, you'd need to check the flag, and look up something different.  Not hard though.


Integer *literals* are built by the desugarer in `DsUtils.mkIntegerExpr`.  All it assumes is that there are functions:

```wiki
  smallIntegerDataCon :: Int -> Integer
  plusInteger, timesInteger :: Integer -> Integer -> Integer
```


It uses these functions to construct big integer literals using arithmetic. Despite the name, it's not important that `smallIntegerDataCon` is bound to a data constructor. All the library needs to provide is a way to convert an `Int` to an `Integer`

## The libraries


Currently `Integer` is defined in the module `GHC.Num`.  It would not be hard to pull out the `Integer` stuff, so that we had `GHC.Integer` (exporting well-defined interface) that `GHC.Num` imported.  `GHC.Num` is where class `Num` is defined, and `Num` uses `Integer` in the type of the class method `fromInteger`; that's why `GHC.Num` has to import `Integer` and not the other way round.


The tricky bit is that you'll need to build the libraries twice; once with `GHC.Num` importing `GHC.Integer`, and once with `GHC.Num` importing your new `Integer` library instead.  You'd need to ship GHC with both sets of libraries; the `-integer-package` flag would select which set of libraries you use when compiling and linking.  We already have this library-version issue with profiling; but you've just doubled the number of library builds!

## The runtime system


You'd need to build two versions of the runtime system too, one omitting all the GMP goop.
