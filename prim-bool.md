# Implementing primitive Bool\#


This page gathers the notes about implementing new primitive logical operations and thus resolving ticket [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135).

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


There are five possible branches to take, although four of them have the same result. This is caused by code duplication introduced by case-of-case transform (see [ this blog post](http://lambda.jstolarek.com/2013/01/taking-magic-out-of-ghc-or-tracing-compilation-by-transformation/) for a step by step derivation). According to Ben Lippmeier, who submitted the original bug report, mis-predicted branches are bad in object code because they stall the pipeline.

## Solution


The idea behind the solution is to modify comparison primops to return unboxed unlifted `Int#` instead of `Bool` (which is lifted and thus is returned as a thunk that needs to be evaluated). This will be implemented in the following way:

- existing comparison primops will have their return type changed to `Int#`. Also, their names will be changed. Operators will have `$` added before `#`, others will have `I` added before the `#` (this is a mnemonic denoting that this primop returns and `Int#`). Examples:

```wiki
>=$#      :: Int#    -> Int#    -> Int#
/=$##     :: Double# -> Double# -> Int#
gtCharI#  :: Char#   -> Char#   -> Int#
eqWordI#  :: Word#   -> Word#   -> Int#
ltFloatI# :: Float#  -> Float#  -> Int#
leAddrI#  :: Addr#   -> Addr#   -> Int#
```

- a new module `GHC.PrimWrappers` will be added to ghc-prim library. This module will contain wrappers for comparison primops. These wrappers will have names identical to removed primops and will return a `Bool`. Examples:

```wiki
gtChar# :: Char# -> Char# -> Bool
gtChar# a b = tagToEnum# (a `gtCharI#` b)

(>=#) :: Int# -> Int# -> Bool
(>=#) a b = tagToEnum# (a >=$# b)

eqWord# :: Word# -> Word# -> Bool
eqWord# a b = tagToEnum# (a `eqWordI#` b)

(/=##) :: Double# -> Double# -> Bool
(/=##) a b = tagToEnum# (a /=$## b)

ltFloat# :: Float# -> Float# -> Bool
ltFloat# a b = tagToEnum# (a `ltFloatI#` b)

leAddr# :: Addr# -> Addr# -> Bool
leAddr# a b = tagToEnum# (a `leAddrI#` b)
```


Thanks to these wrappers the change will be almost backwards compatible. The only thing primop users will need to change in their existing code to make it work again is adding import of !GHC.PrimWrappers module.

- The following boot libraries require modification in order to work with the new primops: base, ghc-prim and integer-gmp. The only required modifications are imports of the !GHC.PrimWrappers module in modules that use the primops.

## Proof of concept


The prototype patch posted on the trac on 13th of March implemented six new prototype comparison primops:

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
  movq  %rsi, %rax
  orq %r14, %rax
  shrq  $63, %rax
  cmpq  %rdi, %r14
  setge %cl
  movzbl  %cl, %ecx
  orq %rax, %rcx
  cmpq  %r8, %rsi
  setge %al
  movzbl  %al, %eax
  orq %rcx, %rax
  jne .LBB2_1
# BB#3:                                 # %c1ol
  movq  (%rbp), %rax
  movl  $r1m6_closure+1, %ebx
  jmpq  *%rax  # TAILCALL
.LBB2_1:                                # %c1nK
  cmpq  $1, %rax
  jne .LBB2_2
# BB#4:                                 # %c1ov
  movq  (%rbp), %rax
  movl  $r1m7_closure+1, %ebx
  jmpq  *%rax  # TAILCALL
.LBB2_2:                                # %c1ob
  movq  r1m5_closure(%rip), %rax
  movl  $r1m5_closure, %ebx
  jmpq  *%rax  # TAILCALL
```


The assembly does not contain comparisons and jumps in the scrutinee of the case expression, but still does jumps for selecting an appropriate branch of the case expression.

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
