# Implementing primitive Bool\#


This page gathers the notes about implementing primitive logical operations and thus resolving ticket [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135).

## The problem


Consider following fragment of code:

```wiki
 case (x <# 0#) || (x >=# width) || (y <# 0#) || (y >=# height) of
  True  -> E1
  False -> E2
```


This kind of code is common in image processing (and array programming in general) where one needs to check whether the `(x,y)` coordinates are within the image. Primitive comparison operators `<#` and `>=#` have type `Int# -> Int# -> Bool`. Logical OR operator `(||)` is defined as:

```wiki
(||)       :: Bool -> Bool -> Bool
True  || _ =  True
False || x =  x
```


in GHC.Classes (ghc-prim library) which is equivalent of:

```wiki
(||) x y = case x of
            True  -> True
            False -> y
```


During the compilation process (assuming the optimizations are turned on) the definition of `(||)` gets inlined and then case-of-case transform is performed successively. This results in following Core (cleaned up for clarity):

```wiki
case <# x 0 of _ {
  False ->
    case >=# x width of _ {
      False ->
        case <# y 0 of _ {
          False ->
            case >=# y height of _ {
              False -> E2
              True  -> E1
            };
          True -> E1
        };
      True -> E1
    };
  True -> E1
};
```


and in following assembler code:

```wiki
.Lc1rf:
        testq %r14,%r14
        jl .Lc1rk
        cmpq %rdi,%r14
        jge .Lc1rp
        testq %rsi,%rsi
        jl .Lc1ru
        cmpq %r8,%rsi
        jge .Lc1rz
        movl $Main_g2_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rk:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rp:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1ru:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rz:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
```


There are five possible branches to take, although four of them have the same result. This is caused by code duplication introduced by case-of-case transform (see [ this blog post](http://ics.p.lodz.pl/~stolarek/blog/2013/01/taking-magic-out-of-ghc-or-tracing-compilation-by-transformation/) for a step by step derivation). According to Ben Lippmeier, who submitted the original bug report, mis-predicted branches are bad in object code because they stall the pipeline.

## Workarounds


It is possible to work around the issue of code duplication by using GHC primops `tagToEnum#` and `dataToTag#`. These allow to distinguish between `True` and `False` by means of accessing the tag of a data type constructor. This means that `dataToTag#` can convert `True` to `1#` and `False` to `0#`, while `tagToEnum#` does the opposite (see paper [ Faster Laziness Using Dynamic Pointer Tagging](http://research.microsoft.com/en-us/um/people/simonpj/papers/ptr-tag/index.htm) for more details): 

```wiki
ghci> import GHC.Exts
ghci> import GHC.Prim
ghci> :set -XMagicHash
ghci> I# (dataToTag# True)
1
ghci> I# (dataToTag# False)
0
ghci> (tagToEnum# 0#) :: Bool
False
ghci> (tagToEnum# 1#) :: Bool
True
ghci>
```


Having the possibility of converting `Bool` to an unboxed `Int#` allows us to compute results of logical expression by means of logical bitwise operations. The result can be converted back to a `Bool` so this is transparent on the Haskell source level, except for the fact that defined logical binary operators will be strict in both their arguments. 

**NOTE: Validity of this solution is based on assumption that `True` will always have a tag of `1#`, while `False` will have a tag of `0#`. Changing this invariant in the future would make these primitive logical operators invalid.**

### First workaround


First workaround assumes converting each result of comparison into an unboxed `Int` and replacing `||` with `+#`:

```wiki
 case (dataToTag# (x <# 0#)) +# (dataToTag# (x >=# width)) +#
      (dataToTag# (y <# 0#)) +# (dataToTag# (y >=# height)) of
  0# -> E2 -- note that branch order is reversed
  _  -> E1
```


This compiles to:

```wiki
case +#
       (+#
          (+# (dataToTag# (<# x 0)) (dataToTag# (>=# x width)))
          (dataToTag# (<# y 0)))
       (dataToTag# (>=# y height))
of _ {
  __DEFAULT -> E1;
  0 -> E2
}
```


Similarly we can convert logical && into multiplication.

### Second workaround


The above workaround is a bit clumsy: `dataToTag#`s make the code verbose and it may not be very obvious what the code is doing. Hence the second workaround, that defines an alternative logical `or` operator:

```wiki
(||#) :: Bool -> Bool -> Bool
(||#) x y = let xW = int2Word# (dataToTag# x)
                yW = int2Word# (dataToTag# y)
                zI = word2Int# (yW `or#` xW)
            in tagToEnum# zI
```


This operator is defined in terms of primops `dataToTag#`, `tagToEnum#` and a bitwise or primop `or#`. Since the last one operates only on `Word`s we need to use `int2Word#` and `word2Int#` for conversion between these data types. Luckily, GHC does a good job of removing unnecessary conversions between data types. This means that:

```wiki
 case (x <# 0#) ||# (x >=# width) ||# (y <# 0#) ||# (y >=# height) of
  True  -> E1
  False -> E2
```


compiles to:

```wiki
case tagToEnum#
       (word2Int#
          (or#
             (int2Word# (dataToTag# (>=# y height)))
             (or#
                (int2Word# (dataToTag# (<# y 0)))
                (or#
                   (int2Word# (dataToTag# (>=# x width)))
                   (int2Word# (dataToTag# (<# x 0)))))))
of _ {
  False -> E2;
  True  -> E1
}
```


Primitive logical operators `&&#` and `not#` can be defined in a similar matter.

**NOTE: Neither of this two workarounds produces good object code. The reason is that comparison operators return a `Bool` as a thunk that needs to be evaluated. The real solution requires that no thunk is created.**

## Solutions


A good beginning would be to implementing second of the above workarounds as a primop. Then we need to create primops that return unboxed values instead of a thunk. The big question is should an unboxed version of Bool be introduced into the language?

### First approach


Treat `Bool` as a boxed version of primitive `Bool#`. `True` would be equivalent of `B# True#`, `False` of `B# False#`:

```wiki
data Bool = B# True# | B# False#

-- B# :: Bool# -> Bool
```


Not sure if this can be considered equivalent to what the Haskell Report says about Bool. We need to ensure that `Bool#` is populated only by `True#` and `False#` and that these two are translated to `1#` and `0#` in the Core. It should be **impossible** to write such a function at Haskell level:

```wiki
g :: Bool -> Int -> Int
g (B# b) (I# i) = I# (b + i)
```


This approach might require one additional case expression to inspect the value of `Bool` at the Core level. For example:

```wiki
f :: Int -> Int -> Int
f x y = if x > y
        then x
        else y
```


would compile to:

```wiki
case x of _ { I# xP ->
 case y of _ { I# yP ->
   case ># xP yP of _ {
     B# bP -> case bP of _ { 1# -> e1; 0# -> e2 }
   }
 }
}
```


This would complicate Core a bit but it should be possible to compile such Core to exactly the same result as with normal `Bool`. This code assumes that `>#` has type `Int# -> Int# -> Bool`, but to truly avoid branching in the Core we need `.># :: Int# -> Int# -> Bool#` so that we get a primitive value that doesn't need to be inspected using case expression but can be directly used by primitive logical operators.

### Second approach


Second approach assumes creating type `Bool#` that is independent of type `Bool`. Boxing and unboxing would have to be done explicitly via additional functions:

```wiki
data Bool = True | False -- no changes here

bBox :: Bool# -> Bool
bBox 1# = True
bBox 0# = False

bUnbox :: Bool -> Bool#
bUnbox True  = 1#
bUnbox False = 0#
```

`Bool#` could not be implemented as an ADT because it is unlifted and unboxed, while ADT value constructors need to be boxed and lifted (see comments in [compiler/types/TyCon.lhs](/trac/ghc/browser/ghc/compiler/types/TyCon.lhs)). There would need to be some magical way of ensuring that `Bool#` is populated only by `#0` and `1#` and that these values cannot be mixed with unboxed integers. Perhaps this could be done by preventing programmer from explicitly creating values of that type (can this be done?) and allow her only to use values returned from functions.


Another problem with this approach is that it would introduce primitive logical operations `||#` and `&&#` with type `Int# -> Int# -> Int#` - it is questionable whether anyone would want such operations available to the programmer. I think it is desirable to have primitive logical operators of type `Bool# -> Bool# -> Bool#`.

## Proposed patch (13/03/2013)


The prototype patch posted on the trac implements six new prototype comparison primops:

```wiki
.>#  :: Int# -> Int# -> Int#
.<#  :: Int# -> Int# -> Int#
.>=# :: Int# -> Int# -> Int#
.<=# :: Int# -> Int# -> Int#
.==# :: Int# -> Int# -> Int#
./=# :: Int# -> Int# -> Int#
```


Each of these new primops takes two `Int#`s that are to be compared. The result is also an `Int#`: `0#` if the relation between the operands does not hold and `1#` when it does hold. For example `5# .># 3#` returns `1#` and `3# .># 3#` returns `0#`. With the new primops we can rewrite the original expression that motivated the problem:

```wiki
case (x <# 0#) || (x >=# width) || (y <# 0#) || (y >=# height) of
  True  -> E1
  False -> E2
```


as

```wiki
case (x .<# 0#) `orI#` (x .>=# width) `orI#` (y .<# 0#) `orI#` (y .>=# height) of
  True  -> E1
  False -> E2
```


(Note: `orI#` is a bitwise OR operation on operands of type `Int#`. It was introduced together with `andI#`, `notI#` and `xor#` in [\#7689](https://gitlab.haskell.org//ghc/ghc/issues/7689)). Using the LLVM backend this compiles to:

```wiki
# BB#0:                                 # %c1nK
	movq	%rsi, %rax
	orq	%r14, %rax
	shrq	$63, %rax
	cmpq	%rdi, %r14
	setge	%cl
	movzbl	%cl, %ecx
	orq	%rax, %rcx
	cmpq	%r8, %rsi
	setge	%al
	movzbl	%al, %eax
	orq	%rcx, %rax
	jne	.LBB2_1
# BB#3:                                 # %c1ol
	movq	(%rbp), %rax
	movl	$r1m6_closure+1, %ebx
	jmpq	*%rax  # TAILCALL
.LBB2_1:                                # %c1nK
	cmpq	$1, %rax
	jne	.LBB2_2
# BB#4:                                 # %c1ov
	movq	(%rbp), %rax
	movl	$r1m7_closure+1, %ebx
	jmpq	*%rax  # TAILCALL
.LBB2_2:                                # %c1ob
	movq	r1m5_closure(%rip), %rax
	movl	$r1m5_closure, %ebx
	jmpq	*%rax  # TAILCALL
```


The assembly does not contain comparisons and jumps in the scrutinee of the case expression, but still does jumps for selecting an appropriate branch of the case expression.


An alternative design decision is to make the new primops return a `Word#`. I decided to use `Int#` because `Int` is used more often than `Word`. If new primops returned a `Word#` the user would have to use `int2Word#`/`word2Int#` primops to do conversions if she ever wished to box the result.


Comparisons for `Word#`, `Float#` and `Double#` will be implemented once we make sure that the prototype implementation is correct.


Some concerns:

- should the primops return an `Int#` or `Word#` as their result?
- what names should the new primops have? I planned to use names with a dot preceeding the operator for `Int#` and `Double#` comparisons (e.g. `./=#`, `.>##`) and names with "St" suffix for `Word#` and `Float#`, e.g. `gtWordSt#`, `gtFloatSt#` (`St` stands for 'strict' because the result can be used with the strict bitwise logical operators).
- how to remove the old `Compare` primops (ones of type `T -> T -> Bool`)?
- once we have the new primops do we really care about the unboxed `Bool#`?

### Benchmarks for the proposed patch


Below is a benchmark for the proof-of-concept filter function that demonstrates performance gains possible with the new primops:

```wiki
{-# LANGUAGE BangPatterns, MagicHash #-}
module Main (
             main
            ) where

import Control.Monad.ST                  (runST)
import Criterion.Config                  (Config, cfgPerformGC,
                                          defaultConfig, ljust)
import Criterion.Main
import Data.Vector.Unboxed.Mutable       (unsafeNew, unsafeSlice, unsafeWrite)
import Data.Vector.Unboxed               as U (Vector, filter, foldM',
                                               fromList, length, unsafeFreeze)
import GHC.Exts                          (Int (I#), (.>=#))
import System.Random                     (RandomGen, mkStdGen, randoms)
import Prelude                    hiding (filter, length)


filterN :: U.Vector Int -> U.Vector Int
filterN vec = runST $ do
  let !size = length vec
  fVec <- unsafeNew size
  let put i x = do
        let !(I# v) = x
            inc     = I# (v .>=# 0#)
        unsafeWrite fVec i x
        return $ i + inc
  fSize <- foldM' put 0 vec
  unsafeFreeze $ unsafeSlice 0 fSize fVec


main :: IO ()
main = return (mkStdGen 1232134332) >>=
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let dataSize   = 10 ^ (7 :: Int)
        inputList  = take dataSize . randoms $ gen :: [Int]
        inputVec   = fromList inputList
        isPositive = (> 0)
    in [
       bgroup "Filter"
         [
           bench "New"    $ whnf (filterN)            inputVec
         , bench "Vector" $ whnf (filter  isPositive) inputVec
         ]
      ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }

```


Compile and run with:

```wiki
ghc -O2 -fllvm -optlo-O3 Main.hs
./Main -o report.html
```


Benchmarking shows that `filterN` function is 60% faster than the `filter` function based on stream fusion (tested for unboxed vectors containing 10 thousand and 10 million elements).
