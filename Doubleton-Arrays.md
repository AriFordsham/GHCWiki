It is occasionally useful to create small arrays, initialize them, and then immediately freeze them. This happens, for example, in the [internals of `unordered-containers`](https://github.com/tibbe/unordered-containers/blob/d0b78afc028ef307a3cdfe1dd95feaf4ec47f195/Data/HashMap/Array.hs#L266-L271):

    pair :: a -> a -> Array a
    pair x y = run $ do
        ary <- new 2 x
        write ary 1 y
        return ary

For the rest of this page, the focus will be doubleton arrays although the arguments and ideas here are equally applicable to tripleton, quadrupleton, etc. arrays as well. The minimal self-contained example of the best possible option for creating doubleton arrays available to GHC users today is:

```
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RankNTypes #-}

{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module DoubletonArray
  ( makeDoubleton
  ) where

import GHC.Exts
import GHC.ST

data SmallArray a = SmallArray (SmallArray# a)
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)

makeDoubleton :: a -> a -> SmallArray# a
makeDoubleton x y = runSmallArrayST $ do
  dst <- newSmallArray 2 x
  writeSmallArray dst 1 y
  unsafeFreezeSmallArray dst

writeSmallArray :: SmallMutableArray s a -> Int -> a -> ST s ()
writeSmallArray (SmallMutableArray sma#) (I# i#) x =
  ST $ \s -> (# writeSmallArray# sma# i# x s, () #)

unsafeFreezeSmallArray :: SmallMutableArray s a -> ST s (SmallArray a)
unsafeFreezeSmallArray (SmallMutableArray sma#) =
  ST $ \s -> case unsafeFreezeSmallArray# sma# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)

newSmallArray
  :: Int -- ^ size
  -> a   -- ^ initial contents
  -> ST s (SmallMutableArray s a)
newSmallArray (I# i#) x = ST $ \s ->
  case newSmallArray# i# x s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)

runSmallArrayST :: (forall s. ST s (SmallArray a)) -> SmallArray# a
runSmallArrayST f = runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, SmallArray r #) -> r }})
```

Note that this includes its own implementation of `runST` (`runSmallArrayST`) to work around #15127. With GHC 8.10 and `ddump-cmm`, the relevant section of the cmm is:

```
==================== Output Cmm ====================
2020-05-15 11:26:23.441750195 UTC

[DoubletonArray.makeDoubleton_entry() { //  [R3, R2]
         { info_tbls: [(cR4,
                        label: DoubletonArray.makeDoubleton_info
                        rep: HeapRep static { Fun {arity: 2 fun_type: ArgSpec 15} }
                        srt: Nothing)]
           stack_info: arg_space: 8 updfr_space: Just 8
         }
     {offset
       cR4: // global
           Hp = Hp + 32;
           if (Hp > HpLim) (likely: False) goto cR8; else goto cR7;
       cR8: // global
           HpAlloc = 32;
           R3 = R3;
           R2 = R2;
           R1 = DoubletonArray.makeDoubleton_closure;
           call (stg_gc_fun)(R3, R2, R1) args: 8, res: 0, upd: 8;
       cR7: // global
           I64[Hp - 24] = stg_SMALL_MUT_ARR_PTRS_DIRTY_info; // exhibit E
           I64[Hp - 16] = 2;
           _cQZ::I64 = Hp - 24;
           P64[_cQZ::I64 + 16] = R2; // exhibit A
           P64[_cQZ::I64 + 24] = R2; // exhibit B
           _sQN::P64 = R3;
           _sQQ::P64 = _cQZ::I64;
           if (I64[nonmoving_write_barrier_enabled] == 0) (likely: True) goto cR1; else goto cR3; // exhibit D
       cR3: // global
           call "ccall" arg hints:  [PtrHint,
                                     PtrHint]  result hints:  [] updateRemembSetPushClosure_(BaseReg, P64[_cQZ::I64 + 24]);
           goto cR1;
       cR1: // global
           call MO_WriteBarrier(); // exhibit H
           P64[_sQQ::P64 + 24] = _sQN::P64; // exhibit C
           I64[_sQQ::P64] = stg_SMALL_MUT_ARR_PTRS_DIRTY_info; // exhibit F
           I64[_sQQ::P64] = stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info; // exhibit G
           R1 = _sQQ::P64;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
     }
 },
 section ""data" . DoubletonArray.makeDoubleton_closure" {
     DoubletonArray.makeDoubleton_closure:
         const DoubletonArray.makeDoubleton_info;
 }]
```

Some comments named exhibit A, B, C, etc. have been added so that attention can be draw to specific lines. First, let's focus on the good. Notice that GHC unrolls `memset` when lowering `newSmallArray#` from STG to Cmm (A and B). Now the bad. In Cmm, the array is fully initialized (A and B) and then overwritten later (C). The header is written times (E,F,G). There is some stuff with write barriers that does not need to happen at all (D,H) since the array is immediately frozen.

## Solutions

### Doubleton Primop

Introduce a primop `doubletonSmallArray# :: a -> a -> SmallArray# a`.

### Array Literals

Introduce new syntax for array literals. This is more general and would work for doubletons, tripletons, quadrupletons, etc.

