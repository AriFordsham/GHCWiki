# Implementing new primitive comparisons to allow branchless algorithms


This page presents motivation and technical details behind implementing new primitive comparison operators (this was originaly reported as Trac ticket #6135). See [this page](http://ghc.haskell.org/trac/ghc/wiki/NewPrimopsInGHC7.8) for instructions how to adjust your already existing code to work with new primops.


See also
 

- #9661
- #13397

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


in GHC.Classes (ghc-prim library). This definition is equivalent of:

```wiki
(||) x y = case x of
            True  -> True
            False -> y
```


During compilation definition of `(||)` gets inlined (assuming the optimizations are turned on) and then case-of-case transform is performed successively. This results in following Core (cleaned up for clarity):

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


and in following assembly code:

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


There are five possible branches to take, although four of them have the same result. This is caused by code duplication introduced by case-of-case transform (see [this blog post](http://lambda.jstolarek.com/2013/01/taking-magic-out-of-ghc-or-tracing-compilation-by-transformation/) for a step by step derivation). According to Ben Lippmeier, who submitted the original bug report, mis-predicted branches are bad in object code because they stall the pipeline.


Note: this example was produced with GHC 7.6.3. At the moment of merging new primops into HEAD, there was no code duplication at the assembly level when using old primops. However, avoiding code duplication is not the main problem new primops are meant to solve. That problem are conditional branches. 

## Solution


This problem was solved by modifying comparison primops to return unboxed unlifted `Int#` instead of `Bool`, which is lifted and thus must be inspected with a `case` expression. Having `Int#` returned as a result of logical comparison allows to use branchless bitwise logical operators instead of branching logical operators defined by Haskell.

## Eliminating branches using new primops


With the new primops we can rewrite the original expression that motivated the problem:

```wiki
case (x <# 0#) || (x >=# width) || (y <# 0#) || (y >=# height) of
  True  -> E1
  False -> E2
```


as

```wiki
case (x <$# 0#) `orI#` (x >=$# width) `orI#` (y <$# 0#) `orI#` (y >=$# height) of
  True  -> E1
  False -> E2
```


Let's analyze how that code gets compiled by the LLVM backend. For purposes of this analysis I will convert the above case expression to the following function:
 

```wiki
f :: Int# -> Int# -> Int# -> Int# -> String
f x y width height =
    case (x <$# 0#) `orI#` (x >=$# width) `orI#` (y <$# 0#) `orI#` (y >=$# height) of
      1# -> "one"
      0# -> "zero"
```


By dumping intermediate Cmm representation we can determine how variables are mapped to CPU registers. Arguments are passed to the function using a stack:

```wiki
Main.f_slow() //  [R1]
         { info_tbl: []
           stack_info: arg_space: 0 updfr_space: Nothing
         }
     {offset
       cWY:
           R5 = I64[Sp + 24];
           R4 = I64[Sp + 16];
           R3 = I64[Sp + 8];
           R2 = I64[Sp];
           R1 = R1;
           Sp = Sp + 32;
           call Main.f_info(R5, R4, R3, R2, R1) args: 8, res: 0, upd: 8;
     }
 }
```

`R2` contains the first argument `x`, `R3` contains `y`, `R4` contains `width` and `R5` contains `height`. We can verify that by looking at the body of `Main.f_info`:

```wiki
_sV9::I64 = R3;
_sV3::I64 = R2;
_sWx::I64 = %MO_S_Lt_W64(_sV3::I64, 0) | %MO_S_Ge_W64(_sV3::I64, R4) |
            %MO_S_Lt_W64(_sV9::I64, 0) | %MO_S_Ge_W64(_sV9::I64, R5);
```


Mappings between Cmm's `R(2/3/4/5)` registers and machine registers are defined in `includes/stg/MachRegs.h`:

```wiki
#define REG_R2 r14
#define REG_R3 rsi
#define REG_R4 rdi
#define REG_R5 r8
```


Knowing that we can dump the assembly generated by LLVM backend:

```wiki
Main_f_info:
# BB#0:
   movq    %rsi, %rax
   orq     %r14, %rax
   shrq    $63, %rax
   cmpq    %rdi, %r14
   setge   %cl
   movzbl  %cl, %ecx
   orq     %rax, %rcx
   cmpq    %r8, %rsi
   setge   %al
   movzbl  %al, %eax
   orq     %rcx, %rax
   jne     .LBB4_1
# BB#3:
   movq    Main_f2_closure(%rip), %rax
   movl    $Main_f2_closure, %ebx
   jmpq    *%rax  # TAILCALL
.LBB4_1:
   cmpq    $1, %rax
   jne     .LBB4_2
# BB#4:
   movq    Main_f1_closure(%rip), %rax
   movl    $Main_f1_closure, %ebx
   jmpq    *%rax  # TAILCALL
.LBB4_2:
   movq    Main_f3_closure(%rip), %rax
   movl    $Main_f3_closure, %ebx
   jmpq    *%rax  # TAILCALL
```


Let's analyze line by line the part responsible for evaluating the scrutinee:

```wiki
  movq   %rsi, %rax   # load y (stored in %rsi) into %rax register
  orq    %r14, %rax   # perform bitwise OR between y (now in %rax) and x (in %r14)
                      # and store result in %rax
  shrq   $63, %rax    # shift %rax 63 bits to the right. If both x and y were
                      # positive numbers, then their sign bits (MSB) were set to
                      # 0 and so %rax is now 0. If at least one of them was
                      # negative then its sign bit must have been 1 and so %rax
                      # is now 1
  cmpq   %rdi, %r14   # compare width (in %rdi) with x (in %r14) and set the flags
                      # in the flag register according to the result.
  setge  %cl          # if, based on flags set by cmpq, x was greater or equal to 
                      # width then we set %cl to 1. Otherwise it is set to 0.
  movzbl %cl, %ecx    # zero the bits of %ecx register except the lowest 8 bits
                      # containg the result of previous operation
  orq    %rax, %rcx   # perform logical OR on results of the previous test and
                      # store the result in %rcx. At this point if %rcx is 1
                      # then either x was negative, or y was negative or
                      # x was greater or equal to width
  cmpq   %r8, %rsi    # now we are checking whether y is greter or equal to
                      # height. This is the same as previosly for x and width.
  setge  %al          # This time we set LSB of %al to 1 if y >= height
  movzbl %al, %eax    # as previously, clear bits of %eax except lowest 8 bits
  orq    %rcx, %rax   # perform logical OR which combines result of previous
                      # three comparisons with the last one    
  jne    .LBB2_1      # if ZF is not set it this means that either %rcx or %rax
                      # was not zero, which means that at least one condition
                      # in the scrutinee was true
```


The assembly does not contain comparisons and branches in the scrutinee of the case expression, but still uses jumps to select an appropriate branch of the case expression. 

## Implementation details


Below is a summary of implementation details and decisions:

- The new comparison primops return a value of type `Int#`: `1#` represents `True` and `0#` represents `False`. The `Int#` type was chosen because in Haskell it is more common to use signed `Int` type insetad of unsigned `Word`. By using `Int#` the users can easily convert unboxed result into a boxed value, without need to use `word2Int#` and `int2word#` primops.

- Unlike C, `2#` or `-3#` don't represent a Boolean value.  More concretely, you can use `tagToEnum#` to convert one of these `Int#` values to a `Bool`, but `tagToEnum#` does no error checking, so it would be Very Very Bad to call it on `2#`. Our plan is to provide safe `isTrue#` and `isFalse#` functions, which will check whether its `Int#` parameter is a valid representation of `True` (i.e. it is `1#`) or `False` (i.e. it is `0#`). This is not possible at the moment due to deficiency in the code generator (see #8326), but we do provide `isTrue#` function for you to use (defined in `GHC.Types`, re-exported by `GHC.Base` and `GHC.Exts`). Currently it is an alias to `tagToEnum#` and is therefore unsafe, but once we solve #8326 we will turn it into a safe function.

- As a small side-task, four new logical bitwise primops have been implemented: `andI#`, `orI#`, `xorI#` and `negI#` (#7689). These operate on values of type `Int#`. Earlier we only had bitwise logical primops operating on values of type `Word#`.

- Functions for comparing values of `Integer` type are not primops from technical point of view, because they are implemented in Haskell (in case of integer-gmp also with FFI), but they pretend to be ones. There are six primops for comparing `Integer` values:

  ```wiki
  eqInteger#  :: Integer -> Integer -> Int#
  neqInteger# :: Integer -> Integer -> Int#
  leInteger#  :: Integer -> Integer -> Int#
  ltInteger#  :: Integer -> Integer -> Int#
  gtInteger#  :: Integer -> Integer -> Int#
  geInteger#  :: Integer -> Integer -> Int#
  ```


Each of these functions has a wrapper that calls `isTrue#` and returns a `Bool`. These wrappers are: `eqInteger`, `neqInteger`, `leInteger`, `ltInteger`, `gtInteger` and `geInteger`.

- Other libraries that were modified to work with the new primops are: array, base, dph, ghc-prim, primitive and template-haskell.

- GHC received an internal module [compiler/utils/ExtsCompat46](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/utils/ExtsCompat46) that allows to bootstrap with GHC versions that have old primops (i.e. GHC 7.6 and GHC 7.4). This module is meant to be temporary - see #8330.

### Benchmarks


Below is a benchmark for the proof-of-concept branchless filter function that demonstrates performance gains possible with the new primops:

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
import GHC.Exts                          (Int (I#), (>=$#))
import System.Random                     (RandomGen, mkStdGen, randoms)
import Prelude                    hiding (filter, length)


filterN :: U.Vector Int -> U.Vector Int
filterN vec = runST $ do
  let !size = length vec
  fVec <- unsafeNew size
  let put i x = do
        let !(I# v) = x
            inc     = I# (v >=$# 0#)
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


Benchmarking shows that `filterN` function is about 55-65% faster than the `filter` function based on stream fusion (tested for unboxed vectors containing 10 thousand and 10 million elements). Below is an example benchmarking report from criterion:



![](prim-bool/prim-bool-criterion.png)


