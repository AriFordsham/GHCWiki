# NOTE: Historical page


This page is here for historical reasons.  Most of the issues described here are now fixed (2 Aug 2012), and the new code generator produces code approximately as good as the old code generator.  Any remaining issues will be made into tickets as necessary. See [Code Generator](commentary/compiler/code-gen) page for an up-to-date description of the current code generator.

# Stupidity in the New Code Generator


Presently compiling using the new code generator results in a fairly sizable performance hit, because the new code generator produces sub-optimal (and sometimes absolutely terrible code.) There are [a lot of ideas for how to make things better](http://darcs.haskell.org/ghc/compiler/cmm/cmm-notes); the idea for this wiki page is to document all of the stupid things the new code generator is doing, to later be correlated with specific refactorings and fixes that will hopefully eliminate classes of these stupid things. The hope here is to develop a sense for what the most endemic problems with the newly generated code is.

## Cantankerous Comparisons

FIXED in newcg branch, 15/2/2012


In `cgrun065` we have

```wiki
test_sizeofArray :: [Int]
test_sizeofArray = runST $ ST $ \ s# -> go 0# [] s#
  where
    go i acc s#
        | i <# 1000# = case newArray# i 0 s# of
            (# s2#, marr# #) -> go (i +# 1#) ((I# 1#):acc) s2#
        | otherwise = (# s#, acc #)
```


Which compiles to the nice STG code

```wiki
                let-no-escape {
                    go_sew =
                        sat-only \r srt:SRT:[] [i_seo acc_ser s#1_seq]
                            case <# [i_seo 1000] of wild_seN {
                              GHC.Types.False -> (#,#) [s#1_seq acc_ser];
                              GHC.Types.True ->
                                  let { sat_seJ = NO_CCS GHC.Integer.Type.S#! [0];
                                  } in 
```


But the comparison is compiled into stupid code:

```wiki
  cg0:
      // outOfLine should follow:
      _cgm::I64 = %MO_S_Lt_W64(_seo::I64, 1000);
      // emitReturn: Sequel: Assign
      _seN::I64 = I64[GHC.Types.Bool_closure_tbl + (_cgm::I64 << 3)];
      _cgn::I64 = _seN::I64 & 7;
      if (_cgn::I64 >= 2) goto cgc; else goto cg5;
  cgc:
```


etc.


We're actually converting to a `Bool` and then doing an algebraic case!  This is a StgCmm issue, not a pipeline issue.

## Dead stack/heap checks

FIXED in newcg branch, but in an ad-hoc way (the stack allocator does it).  We probably want to do this as part of a more general optimisation pass.


See in `cgrun065`

```wiki
     cgr:
         if (0) goto cgx;
         R1 = R2;
         jump stg_ap_0_fast ();
     cgx: jump stg_gc_fun ();
```

## Instruction reordering


NEW. We should be able to reorder instructions in order to decrease register pressure. Here's an example from 3586.hs

```wiki
        _cPY::I32 = I32[Sp - 24];
        I32[R1 + 4] = _cPY::I32;
        I32[R1] = stg_IND_STATIC_info;
        I32[Sp - 8] = _cPY::I32;
        I32[Sp - 12] = stg_upd_frame_info;
```


R1 and Sp probably don't clobber each other, so we ought to use _cPY twice in quick succession. Fortunately stg_IND_STATIC_info is a constant so in this case the optimization doesn't help to much, but in other cases it might make sense. TODO Find better example

## Stack space overuse

FIXED in the newcg branch.  (stack layout algorithm redesigned)


CONFIRMED. `T1969.hs` demonstrates this:

```wiki
 Simp.c_entry()
         { update_frame: <none>
           has static closure: True type: 0
           desc: 0
           tag: 15
           ptrs: 0
           nptrs: 0
           srt: (srt_Sc2_srt,0,3)
           fun_type: 5
           arity: 1
           slow: Simp.c_slow
         }
     cbG:
         _sbe::I32 = I32[Sp + 0];
         if (Sp - 4 < SpLim) goto cbN;
         // outOfLine should follow:
         if (_sbe::I32 & 3 != 0) goto cbP;
         // emitCall: Sequel: Assign
         R1 = _sbe::I32;
         I32[Sp - 4] = block_cbA_info;
         Sp = Sp - 4;
         jump I32[_sbe::I32] ();
     cbN:
         // outOfLine here
         R1 = Simp.c_closure;
         jump stg_gc_fun ();
     cbP:
         // emitReturn: Sequel: Assign
         _sbg::I32 = _sbe::I32;
         Sp = Sp + 4;
         jump block_cbR_entry ();
 }
```


The call area for the jump in cbG is using an extra word on the stack, but in fact Sp + 0 at the entry of the function immediately becomes dead after the assignment, so we ought to be able to save some space in our layout. Simon Marlow suggests we distinguish between the return address and the old call area; however, since this can also happen for the return parameters from call areas, we need a more general scheme.


After I discussed this with SPJ, we've decided that we need to teach the stack layout how to handle partial conflicts. There is a complication here, in that if we do this naively, the interference graph will blow up (since, rather than conflicting call areas, we now have conflicting words of call areas.) Simon suggested that we bound the amount of conflicts we track: either up to 3 or conflict with everything (in which case we just place the area as far down as necessary rather than try to be clever.) I plan on doing this once I understand the current layout code...

## Double temp-use means no inlinining?


CONFIRMED. Here's a curious piece of code that fails to get inlined (from `cc004`):

```wiki
        _sG5::I32 = I32[Sp + 48];
        I32[Sp - 4] = _sG5::I32;
```


Why is that? Because the temp gets reused later on:

```wiki
    u1bF:
        Sp = Sp + 56;
        // outOfLine here
        R1 = a13_rAk_closure;
        I32[Sp - 8] = _sG5::I32;
```


In this case, we want more aggressive inlining because there are too
many temps and they're going to have to get spilled to the stack anyway.
IS THAT TRUE?  For comparison's sake, the old codegen doesn't appear to
do any rewriting, because it just reuses the call area.

## Stupid spills


CONFIRMED. If something is already in memory, why do we have to spill it again?

```wiki
module Foo where
foo a b c d e f g h
    = case baz a of
        True -> bar a b c d e f g h
        False -> 0

baz 3 = True
baz _ = False
bar a b c d e f g h = a * b + c * d + e * f * g * h
```


Well, it's because the spiller isn't clever enough:

```wiki
  cs3:
      _smw::I32 = I32[(old + 8)];
      I32[(slot<_smw::I32> + 4)] = _smw::I32;
      _smv::I32 = I32[(old + 12)];
      I32[(slot<_smv::I32> + 4)] = _smv::I32;
      _smu::I32 = I32[(old + 16)];
      I32[(slot<_smu::I32> + 4)] = _smu::I32;
      _smt::I32 = I32[(old + 20)];
      I32[(slot<_smt::I32> + 4)] = _smt::I32;
      _sms::I32 = I32[(old + 24)];
      I32[(slot<_sms::I32> + 4)] = _sms::I32;
      _smr::I32 = I32[(old + 28)];
      I32[(slot<_smr::I32> + 4)] = _smr::I32;
      _smq::I32 = I32[(old + 32)];
      I32[(slot<_smq::I32> + 4)] = _smq::I32;
      _smn::I32 = I32[(old + 36)];
      I32[(slot<_smn::I32> + 4)] = _smn::I32;
      _smm::I32 = I32[(old + 40)];
      I32[(slot<_smm::I32> + 4)] = _smm::I32;
      if (Sp - <highSp> < SpLim) goto cs9; else goto cs5;
  cs5:
      // outOfLine should follow:
      // directEntry else
      // emitCall: Sequel: Assign
      I32[(young<crz> + 8)] = _smn::I32;
      I32[(young<crz> + 12)] = _smm::I32;
      I32[(young<crz> + 4)] = crz;
      call Foo.baz_info(...) returns to Just crz (12) (4) with update frame 4;
  crz:
      _smm::I32 = I32[(slot<_smm::I32> + 4)];
      _smn::I32 = I32[(slot<_smn::I32> + 4)];
      _smq::I32 = I32[(slot<_smq::I32> + 4)];
      _smr::I32 = I32[(slot<_smr::I32> + 4)];
      _sms::I32 = I32[(slot<_sms::I32> + 4)];
      _smt::I32 = I32[(slot<_smt::I32> + 4)];
      _smu::I32 = I32[(slot<_smu::I32> + 4)];
      _smv::I32 = I32[(slot<_smv::I32> + 4)];
      _smw::I32 = I32[(slot<_smw::I32> + 4)];
      _smx::I32 = R1;
      I32[(slot<_smx::I32> + 4)] = _smx::I32;
      _csp::I32 = _smx::I32 & 3;
      if (_csp::I32 >= 2) goto csk; else goto usu;
```


Ick! The old codegen was much better...

```wiki
Foo.foo_entry()
        { [const Foo.foo_slow-Foo.foo_info;, const 9;,
       const Foo.foo_srt-Foo.foo_info;, const 589824;, const 0;,
const 196623;]
        }
    cqM:
        if ((Sp + -12) < I32[BaseReg + 84]) goto cqO;
        I32[Sp - 8] = I32[Sp + 4];
        I32[Sp - 12] = I32[Sp + 0];
        I32[Sp - 4] = smz_info;
        Sp = Sp - 12;
        jump Foo.baz_info ();
    cqO:
        R1 = Foo.foo_closure;
        Sp = Sp + 0;
        jump (I32[BaseReg - 4]) ();
}

smz_ret()
        { [const Foo.foo_srt-smz_info;, const 9;, const 65568;]
        }
    cqx:
        if (R1 & 3 >= 2) goto cqC;
        Hp = Hp + 8;
        if (Hp > I32[BaseReg + 92]) goto cqA;
        I32[Hp - 4] = sat_smA_info;
        I32[Sp + 36] = Hp - 4;
        I32[Sp + 32] = stg_ap_p_info;
        I32[Sp + 28] = I32[Sp + 4];
        Sp = Sp + 28;
        jump GHC.Num.fromInteger_info ();
    cqC:
        Sp = Sp + 4;
        jump Foo.bar_info ();
    cqB: jump (I32[BaseReg - 8]) ();
    cqA:
        I32[BaseReg + 112] = 8;
        goto cqB;
}
```


The trouble is that the spiller doesn't know that the old call area is also valid game for locations that variables can live in. So, the solution is to rewrite the spiller to know about existing incoming memory locations. Make sure that this information gets to the stack layout engine when we do partial layouts (it should automatically notice, but double check!)

## Noppy proc-points


CONFIRMED. Consider

```wiki
f :: Int -> Int -> Int -> Int
f a b c
    = let x = a + b
          y = b + c
          z = a + c
      in x `seq` case g x y z of
            True  -> x
            False -> 0

{-# NOINLINE g #-}
g :: Int -> Int -> Int -> Bool
g x y 0 = x == y
g _ _ _ = False
```

```wiki
      cnm:
          // directEntry else
          // emitCall: Sequel: Assign
          I32[(young<cmM> + 8)] = base_GHCziNum_zdfNumInt_closure;
          I32[(young<cmM> + 4)] = cmM;
          call base_GHCziNum_zp_info(...) returns to Just cmM (8) (4) with update frame 4;
      cmM:
          _cmN::I32 = R1;
          // slow_call for _cmN::I32 with pat stg_ap_pp
          R1 = _cmN::I32;
          // emitCall: Sequel: Assign
          I32[(young<cmP> + 8)] = _skO::I32;
          I32[(young<cmP> + 12)] = _skN::I32;
          I32[(young<cmP> + 4)] = cmP;
          call stg_ap_pp_fast(...) returns to Just cmP (12) (4) with update frame 4;
```


We generate an extra proc-point for ``cmM``, where in theory we ought to be able to stick the subsequent ``stg_ap_pp_fast`` onto the stack as another return point.

## Lots of temporary variables


WONTFIX. Lots of temporary variables (these can tickle other issues when the temporaries are long-lived, but otherwise would be optimized away). You can at least eliminate some of them by looking at the output of `-ddump-opt-cmm`, which utilizes some basic temporary inlining when used with the native backend `-fasm`, but this doesn't currently apply to the GCC or LLVM backends.

~~At least one major culprit for this is `allocDynClosure`, described in Note `Return a LocalReg`; this pins down the value of the `CmmExpr` to be something for one particular time, but for a vast majority of use-cases the expression is used immediately afterwards. Actually, this is mostly my patches fault, because the extra rewrite means that the inline pass is broken.~~ Fixed in latest version of the pass; we don't quite manage to inline enough but there's only one extra temporary.


Another cause of all of these temporary variables is that the new code generator immediately assigns any variables that were on the stack to temporaries immediately upon entry to a function. This is on purpose. The idea is we optimize these temporary variables away.

## Double proc points

FIXED in newcg branch.


Given a simple case expression

```wiki
f x = case x of
         I# y -> y
```


we generate \*two\* proc points, not one.

```wiki
  cbR:
      if (_sbl::I64 & 7 != 0) goto cbU; else goto cbV;
  cbU:
      _sbp::I64 = _sbl::I64;
      goto cbW;
  cbV:
      R1 = _sbl::I64;
      I64[(young<cbE> + 8)] = cbE;
      call (I64[_sbl::I64])(...) returns to Just cbE (8) (8) with update frame 8;
  cbE:
      _sbp::I64 = R1;
      goto cbW;
  cbW:
      _sbo::I64 = I64[_sbp::I64 + 7];
      _cbJ::I64 = _sbo::I64 + 1;
      // emitReturn: Sequel: Return
      R1 = _cbJ::I64;
      call (I64[(old + 8)])(...) returns to Nothing (8) (0) with update frame 8;
```


Both `cbE` and `cbW` are going to become proc points.


To avoid it we should generate code that re-uses `cbE` as the destination for the first `if`; that is, we need to load up the registers as if we were returning from the call.  This needs some refactoring in the code generator.

## Rewriting stacks

FIXED. `3586.hs` emits the following code:

```wiki
 Main.$wa_entry()
         { [const Main.$wa_slow-Main.$wa_info;, const 3591;, const 0;,
    const 458752;, const 0;, const 15;]
         }
     c17W:
         _s16B::F64 = F64[Sp + 20];
         F64[Sp - 8] = _s16B::F64;
         _s16h::I32 = I32[Sp + 16];
         _s16j::I32 = I32[Sp + 12];
         _s16y::I32 = I32[Sp + 8];
         _s16x::I32 = I32[Sp + 4];
         _s16w::I32 = I32[Sp + 0];
         if (Sp - 12 < SpLim) goto u1bR;
         Sp = Sp + 32;
// [SNIP]
         // directEntry else
         // emitCall: Sequel: Return
         F64[Sp - 12] = _s17a::F64;
         I32[Sp - 16] = _s17b::I32;
         I32[Sp - 20] = _s16j::I32;
         I32[Sp - 24] = _s16y::I32;
         I32[Sp - 28] = _s16x::I32;
         I32[Sp - 32] = _s16w::I32;
         Sp = Sp - 32;
         jump Main.$wa_info ();
     u1bR:
         Sp = Sp + 32;
         // outOfLine here
         R1 = Main.$wa_closure;
         F64[Sp - 12] = _s16B::F64;
         I32[Sp - 16] = _s16h::I32;
         I32[Sp - 20] = _s16j::I32;
         I32[Sp - 24] = _s16y::I32;
         I32[Sp - 28] = _s16x::I32;
         I32[Sp - 32] = _s16w::I32;
         Sp = Sp - 32;
         jump stg_gc_fun ();
```


We see that these temporary variables are being repeatedly rewritten to the stack, even when there are no changes.


Since these areas on the stack are all old call areas, one way to fix this is to inline all of the memory references. However, this has certain undesirable properties for other code, so we need to be a little more clever. The key thing to notice is that these accesses are only used once per control flow path, in which case sinking the loads down and then inlining them should be OK (it will increase code size but not execution time.) However, the other difficulty is that the CmmOpt inliner, as it stands, won't inline things that look like this because although the variable is only used once in different branches, the same name is used, so it can't distinguish between the temporaries with mutually exclusive live ranges. Building a more clever inliner with Hoopl is also a bit tricky, because inlining is a forward analysis/transformation, but usage counting is a backwards analysis.


This looks fixed with the patch from April 14.

## Spilling Hp/Sp

FIXED. `3586.hs` emits the following code:

```wiki
     _c1ao::I32 = Hp - 4;
     I32[Sp - 20] = _c1ao::I32;
     foreign "ccall"
       newCAF((BaseReg, PtrHint), (R1, PtrHint))[_unsafe_call_];
     _c1ao::I32 = I32[Sp - 20];
```


We see `Hp - 4` being allocated to a temp, and then consequently being spilled to the stack even though `newCAF` definitely will not change `Hp`, so we could have floated the expression down.


This seems to happen whenever there's a `newCAF` ccall.


We also seem to reload these values multiple times.

```wiki
        _c7Yt::I32 = Hp - 4;
        I32[Sp - 28] = _c7Yt::I32;
        foreign "ccall"
          newCAF((BaseReg, PtrHint), (R1, PtrHint))[_unsafe_call_];
        _c7Yt::I32 = I32[Sp - 28];
        I32[R1 + 4] = _c7Yt::I32;
        I32[R1] = stg_IND_STATIC_info;
        _c7Yt::I32 = I32[Sp - 28];  <--- totally unnecessary
        I32[Sp - 8] = _c7Yt::I32;
        I32[Sp - 12] = stg_upd_frame_info;
```

~~We need to not spill across certain foreign calls, but for which calls this is OK for is unclear.~~ Variables stay live across all unsafe foreign calls (foreign calls in the middle), except for the obvious cases (the return registers), so no spilling should happen at all. The liveness analysis is too conservative.


This is not fixed in the April 14 version of the patch... we still need to fix the liveness analysis? I thought I fixed that... that's because the transform did extra spilling for CmmUnsafeForeignCalls. Removed that code, and now it's fixed.

## Up and Down

FIXED. A frequent pattern is the stack pointer being bumped up and then back down again, for no particular reason. 

```wiki
         Sp = Sp + 4;
         Sp = Sp - 4;
         jump block_c7xh_entry ();
```


This is mentioned at the very top of `cmm-notes`. This was a bug in the stack layout code that I have fixed.

## Sp is generally stupid

FIXED. Here is an optimized C-- sample from `arr016.hs`.

```wiki
Main.D:Arbitrary_entry()
        { [const 131084;, const 0;, const 15;]
        }
    c7J5:
        _B1::I32 = I32[Sp + 4];
        _B2::I32 = I32[Sp + 0];
        if ((Sp + 0) < I32[BaseReg + 84]) goto u7Jf;
        Sp = Sp + 12;
        Hp = Hp + 12;
        if (Hp > I32[BaseReg + 92]) goto c7Jc;
        I32[Hp - 8] = Main.D:Arbitrary_con_info;
        I32[Hp - 4] = _B2::I32;
        I32[Hp + 0] = _B1::I32;
        _c7J4::I32 = Hp - 7;
        R1 = _c7J4::I32;
        Sp = Sp - 4;
        jump (I32[Sp + 0]) ();
    u7Jf:
        Sp = Sp + 12;
        goto c7Jd;
    c7Jc:
        I32[BaseReg + 112] = 12;
        goto c7Jd;
    c7Jd:
        R1 = Main.D:Arbitrary_closure;
        Sp = Sp - 12;
        jump (I32[BaseReg - 4]) ();
}
```


Compare with the old code:

```wiki
Main.D:Arbitrary_entry()
        { [const 131084;, const 0;, const 15;]
        }
    c4pX:
        Hp = Hp + 12;
        if (Hp > I32[BaseReg + 92]) goto c4pU;
        I32[Hp - 8] = Main.D:Arbitrary_con_info;
        I32[Hp - 4] = I32[Sp + 0];
        I32[Hp + 0] = I32[Sp + 4];
        R1 = Hp - 7;
        Sp = Sp + 8;
        jump (I32[Sp + 0]) ();
    c4pV:
        R1 = Main.D:Arbitrary_closure;
        jump (I32[BaseReg - 4]) ();
    c4pU:
        I32[BaseReg + 112] = 12;
        goto c4pV;
}
```


You can see the up and down behavior here, but that's been fixed, so ignore it for now. (Update the C--!) The unfixed problem is this (some of the other problems were already addressed): we do an unnecessary stack check on entry to this function. We should eliminate the stack check (and by dead code analysis, the GC call) in such cases.


This pattern essentially happens for every function, since we always assign incoming parameters to temporary variables before doing anything.

## Heap and R1 aliasing

FIXED. Values on the heap and values from R1 don't necessarily clobber
each other.  allocDynClosure seems like a pretty safe bet they
don't.  But is this true in general? ANSWER: Memory writes with
Hp are always new allocations, so they don't clobber anything.

```wiki
        _s14Y::F64 = F64[_s1uN::I32 + 8];
        _s152::F64 = F64[_s1uN::I32 + 16];
        _s156::F64 = F64[_s1uN::I32 + 24];
        _s15a::F64 = F64[_s1uN::I32 + 32];
        _s15e::F64 = F64[_s1uN::I32 + 40];
        _s15i::F64 = F64[_s1uN::I32 + 48];
        _s15m::F64 = F64[_s1uN::I32 + 56];
        _c39O::I32 = Hp - 60;
        // calling allocDynClosure
        // allocDynClosure
        I32[Hp - 60] = sat_s1uQ_info;
        F64[Hp - 52] = _s14Y::F64;
        F64[Hp - 44] = _s152::F64;
        F64[Hp - 36] = _s156::F64;
        F64[Hp - 28] = _s15a::F64;
        F64[Hp - 20] = _s15e::F64;
        F64[Hp - 12] = _s15i::F64;
        F64[Hp - 4] = _s15m::F64;
```