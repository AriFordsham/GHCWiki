# Primop changes in GHC 7.8


In GHC 7.8 we modified primops that returned a `Bool`. This includes comparisons between unboxed values (like `==#` or `ltFloat#`) as well as `sameMutableArray#`, `sameMutableByteArray#`, `sameMutableArrayArray#`, `sameMutVar#`, `sameMVar#` and `sameTVar#`. These changes are not backwards compatible and if you use any of these primops then you need to adjust your code. Adjustments are simple and should take no longer than a couple of minutes.

- If your code uses primops for comparing unboxed `Char#`, `Int#`, `Word#`, `Float#`, `Double#` or `Addr#` you need to import `GHC.PrimWrappers` module into your code. Note that `GHC.PrimWrappers` is re-exported by `GHC.Exts`, so if you already import `GHC.Exts` then you don't need to change anything.

- If your code uses `sameMutableArray#`, `sameMutableByteArray#`, `sameMutableArrayArray#`, `sameMutVar#`, `sameMVar#` or `sameTVar#`, you need to import `GHC.PrimWrappers` and remove `#` from the names of these functions. In other words, functions that you should be using now are `sameMutableArray`, `sameMutableByteArray`, `sameMutableArrayArray`, `sameMutVar`, `sameMVar` and `sameTVar`, respectively.


That's all you need to do - your code should now be working as previously. 


If your interested in technical details behind this change, see [ this page](http://ghc.haskell.org/trac/ghc/wiki/PrimBool).
