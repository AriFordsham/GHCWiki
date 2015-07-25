## Why?


To make arithmetic safer: [ http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103](http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103)

## What's already there?

`Data.SafeInt` uses these primops, which only work on `Int`s: `addIntC#`, `subIntC#`, `mulIntMayOflo#`.
Also, `maxInt#` and `minInt#` exist in `libraries/base/GHC/Base.hs`, but not `maxWord#` or `maxInt64#`, etc.

## How do I add a new primop?


See the guide at [ https://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps\#AddinganewPrimOp](https://ghc.haskell.org/trac/ghc/wiki/Commentary/PrimOps#AddinganewPrimOp)


I need to add *inline* primops since `addIntC#` & co. are all inline.
(Just use the same attributes, but make sure to read on each of them.)
So, I only need to touch these files:

- `compiler/prelude/primops.txt.pp`
- `compiler/codeGen/StgCmmPrim.hs`


There's also a tutorial on adding an *out-of-line* primop, but some
bits of it may be useful (e.g., building GHC after making changes):
[ https://ghc.haskell.org/trac/ghc/wiki/AddingNewPrimitiveOperations](https://ghc.haskell.org/trac/ghc/wiki/AddingNewPrimitiveOperations)

## What do I need to know about GHC types?


The "The word size story" section in `primops.txt.pp` provides a good overview.

## Where are the tests for the `Int` primops?

`grep -rniI --exclude=*.html addIntC testsuite`

## How do I expose my new primops as ordinary functions?


Just define `Num` instances in the safeint package as it's done for `SafeInt`.
(There could be an alternative class for things returning an `Either`, but first I'll just add a version that fails at runtime.)
