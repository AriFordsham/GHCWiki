## Why?


To make arithmetic safer: [ http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103](http://article.gmane.org/gmane.comp.lang.haskell.ghc.devel/9103)

## What's already there?

- `Data.SafeInt` uses these primops, which only work on `Int`s: `addIntC#`, `subIntC#`, `mulIntMayOflo#`.
- `maxInt#` and `minInt#` exist in `libraries/base/GHC/Base.hs`, but not `maxWord#` or `maxInt64#`, etc.
- In `libraries/integer-gmp/src/GHC/Integer/Type.hs`, there's `subWordC# :: Word# -> Word# -> (# Word#, Int# #)`
  defined as a helper, which should be replaced by a proper primop.

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

## Where is C-- generated?


For `addIntC#`, it's done in the already mentioned
`compiler/codeGen/StgCmmPrim.hs` file:

```wiki
callishPrimOpSupported :: DynFlags -> PrimOp -> Either CallishMachOp GenericOp
callishPrimOpSupported dflags op
  = case op of
      ...
      IntAddCOp      | (ncg && x86ish)
                         || llvm      -> Left (MO_AddIntC    (wordWidth dflags))
                     | otherwise      -> Right genericIntAddCOp
```

```wiki
-- XXX: Modules required for pretty-printing.
import BlockId
import UniqSupply
import System.IO.Unsafe


emitPrimOp dflags results op args
   = case callishPrimOpSupported dflags op of
          Left op   -> -- emit $ mkUnsafeCall (PrimTarget op) results args
                       -- XXX: Always print C-- when compiling.
                       emit $
                         pprTrace
                           "emitPrimOp:"
                           (ppr $
                              labelAGraph
                                (mkBlockId $ uniqFromSupply $ unsafePerformIO $ mkSplitUniqSupply 'a')
                                ( mkUnsafeCall (PrimTarget op) results args
                                , GlobalScope ) )
                           (mkUnsafeCall (PrimTarget op) results args)
          Right gen -> gen results args
```


Test module:

```wiki
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Test where

import GHC.Prim

f x = addIntC# 2# x
```


Output:

```wiki
$ ghc/inplace/bin/ghc-stage2 --make Test.hs
[1 of 1] Compiling Test             ( Test.hs, Test.o )
emitPrimOp:
  {offset
    avf:
        (_cvc::I64, _cvd::I64) = call MO_AddIntC W64(2, _sv7::I64);
        goto avf;
  }
```


Another option is to use `-ddump-cmm`, which doesn't require touching
the compiler, but it generates more code.

## Where is asm generated?


For x86, it's `compiler/nativeGen/X86/CodeGen.hs`:

```wiki
genCCall _ is32Bit target dest_regs args = do
    ...
    (PrimTarget (MO_AddIntC width), [res_r, res_c]) ->
        addSubIntC platform ADD_CC (Just . ADD_CC) width res_r res_c args
    ...
  where
        addSubIntC platform instr mrevinstr width res_r res_c [arg_x, arg_y]
            = do let format = intFormat width
                 rCode <- anyReg =<< trivialCode width (instr format)
                                       (mrevinstr format) arg_x arg_y
                 reg_tmp <- getNewRegNat II8
                 let reg_c = getRegisterReg platform True (CmmLocal res_c)
                     reg_r = getRegisterReg platform True (CmmLocal res_r)
                     code = rCode reg_r `snocOL`
                            SETCC OFLO (OpReg reg_tmp) `snocOL`
                            MOVZxL II8 (OpReg reg_tmp) (OpReg reg_c)
                 return code
        addSubIntC _ _ _ _ _ _ _
            = panic "genCCall: Wrong number of arguments/results for addSubIntC"
```


Testing on the same file (some things are omitted):

```wiki
$ ghc --make -ddump-asm Test.hs
[1 of 1] Compiling Test             ( Test.hs, Test.o )

==================== Asm code ====================
.text
	.align 8
	.quad	4294967300
	.quad	0
	.quad	15
.globl Test.f_info
.type Test.f_info, @object
Test.f_info:
_cuF:
_cuH:
	addq $2,%r14
	seto %al
	movzbl %al,%eax
	movq %r14,%rbx
	movq %rax,%r14
	jmp *(%rbp)
	.size Test.f_info, .-Test.f_info
```

## What do I need to know about GHC types?


The "The word size story" section in `primops.txt.pp` provides a good overview.

## Where are the tests for the `Int` primops?

`grep -rniI --exclude=*.html addIntC testsuite`

## How do I expose my new primops as ordinary functions?


Just define `Num` instances in the safeint package as it's done for `SafeInt`.
(There could be an alternative class for things returning an `Either`, but first I'll just add a version that fails at runtime.)
