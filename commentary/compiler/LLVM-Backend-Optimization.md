# Planned LLVM Backend Improvements


This page gathers together a list of ideas and plans for improving the LLVM backend of the compiler. Any progress made will also be noted here.

## Removal of Proc Point Splitting for LLVM


The reason we needed proc-point splitting for the LLVM backend is detailed [here](commentary/compiler/backends/llvm/wip#get-rid-of-proc-point-splitting).

### Rationale


Ideally, we would expose an entire Cmm function to LLVM for better optimization and machine code generation, instead of breaking the functions apart just to grab a valid label.
The proc-point splitting is also a rather ugly transformation we would like to remove from the pipeline.

### GHC's Requirements


1. Must be able to allocate frames in a heap-allocated stack segment. These
   frames are allocated in a stack that is seperate from the FFI stack.
   This segment lives in the heap and stack overflow is checked-for using
   a limit test, unlike the guard-page limited C stack convention.

2. Frames must be laid out in a way that is compatible with GHC's runtime system.

3. To support tables-next-to-code (TNTC), we must be able to emit data describing
   the layout of the frame at a return-address just above the label
   corresponding to that address. When the GC walks the stack looking for
   pointers, it will expect the frame's layout information to be alongside
   the code.
   This could be changed for GHC's LLVM backend going forward by building
   a position-independent hash table keyed on return addresses
   during compilation that contains this information.
   At runtime the GC accesses table's information by computing the relative
   offset of the return address with respect to the start of the code segment
   in memory, and then using that offset as the key into the static hash table.
   Benchmarks to help understand the performance trade-off would be needed,
   as the original motivation for TNTC was in-part for performance.


### Proposed Paths Forward


See here for now: https://github.com/kavon/stack-rfc/blob/master/CURRENT.md

There was also discussion of whether we can move to using the native stack-pointer
register for the GHC stack, and swap to an FFI stack to handle such calls.

### Status

#### TODOs

##### From June 26th, 2019 Meeting

1. Investigate the feasibility of using the native stack pointer register.

   Ideally we would use call / ret instructions. This might not be possible
   even for native code gen.
   When performing an Enter to evaluate a thunk, the return
   address is not at the top of the stack, because the curried arguments
   are pushed on after the return address.
   The "Making a fast curry" paper discusses this in terms of fully saturated
   and non-saturated curried functions: https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/

   The major concern is whether we can prevent the kernel from writing to
   memory beyond the stack pointer when delivering signals / interrupts.
   On recent Linux kernels this seems to be the default behavior, but it may
   not be the case on Windows, etc.

   It's a bit tricky to get a direct answer about this, since the System V ABI
   is vague about what the kernel can do beyond the red zone, so a while back
   I created a tool to confirm that nothing is touched by the Linux kernel:
   https://github.com/kavon/redzone

   See: https://www.kernel.org/doc/Documentation/x86/kernel-stacks

   Michal mentioned that there are other reasons why GHC wants to move over
   to use native stack pointer register, beyond performance reasons.
   As a first go, the native backend in GHC could use RSP but not use call/ret,
   and then optimize it later to use call/ret.

   The benefit of using the native stack pointer is that we do not need to modify
   LLVM as much to, for example, add a subtarget for GHC that uses a different
   register.

   If we do use the native stack pointer register, the implementation of this stuff
   turns into writing a custom progloue/epilogue, as done in Manticore


2. Kavon needs to test the overhead of performing stack switching vs having
   the Manticore stack being compatible with C calls. This would help inform
   the decisions made for GHC going forward.

3. Look into feasibility of not using TNTC for return points (i.e., basic
  blocks within a function) because I believe the the only information we place
  before basic block labels is frame layout information.

  Placing this information before block labels in LLVM is quite annoying, because
  the existing implementation for cps-calls uses an assembly-code mangler
  to add `.data` directives before these blocks.

  There doesn't appear to be any nice way to do this in LLVM, so we ought to look
  into having the GC lookup this frame layout information in a hash table.

  We may not be able to totally do away TNTC because SRT information is mixed with the table information for CAF liveness (thunks), which doesn't have to do with the frame.

  Because CAFs are associated with function labels only, it should be possible to
  keep TNTC for CAFs in LLVM, since it's easy to add that information just before
  a function label, but for basic blocks I *believe* only frame layout information
  is added there, and should go into a hash table.



### Old Issues


Issues with the existing cps-call implementation from 2017:

1. The intrinsic is not compatible with the way GHC produces safe FFI calls,
   i.e., FFI calls during which the garbage collector may run, and thus
   the call must setup the GHC stack to be read by the GC properly.
   The main reason for difficulty here is that GHC expands safe FFI calls
   from STG to Cmm is a very unusual manner: the is continuation captured
   and passed to the runtime system, and then the actual callee, using
   multiple unsafe FFI calls.

   Thus, to fix this we need to change the way GHC's codeGen lowers safe
   FFI calls, and then modify / add some utilities to the GHC runtime system
   such that the lowering can be formulated using the cps-call intrinsic in LLVM.

2. There are a number of ugly work-arounds in the LLVM implementation that
   need to be dealt with, as they are a sign of larger issues with the
   implementation. The overall way to fix this is to expand the intrinsic
   late in the llc pipeline, as is done for LLVM's Statepoint intrinsic.
   See the Bugs section of this document for examples
   of the problems: https://github.com/kavon/stack-rfc/blob/master/CURRENT.md


Here is a validate run that should still be representitive of the existing
implementation. Note that some of the failures are pre-existing in the version
of GHC (8.5) the code is based on.

```
Unexpected results from:
TEST="PatternSplice SplicesUsed T10508_api T10904 T12134 T12176 T12442 T1288 T12903 T13050 T13688 T13863 T13949 T14129 T14174a T14590 T1679 T2276 T2469 T2594 T4012 T4038 T4221 T5250 T5313 T5550 T7040 T7944 T8025 T8083 T9329 TH_spliceViewPat abstract_refinement_substitutions ann01 capi_value cgrun034 dataToExpQUnit dsrun023 dynCompileExpr fed001 ffi005 ffi006 ffi007 ffi008 ffi011 ffi013 ffi016 ffi017 ffi018 ffi019 ffi_parsing_001 holes holes2 massive_array mc03 overflow2 overflow3 overloadedlabelsrun04 overloadedrecfldsrun04 refinement_substitutions tc265 valid_substitutions"

SUMMARY for test run started at Tue Mar 13 00:17:17 2018 CDT
 0:50:49 spent to go through
    6278 total tests, which gave rise to
   26967 test cases, of which
   24427 were skipped

      20 had missing libraries
    2424 expected passes
      34 expected failures

       0 caused framework failures
       0 caused framework warnings
       0 unexpected passes
      62 unexpected failures
       0 unexpected stat failures

Unexpected failures:
   annotations/should_compile/T14129.run                           T14129 [exit code non-0] (llvm)
   annotations/should_compile/ann01.run                            ann01 [exit code non-0] (llvm)
   codeGen/should_compile/T9329.run                                T9329 [exit code non-0] (llvm)
   codeGen/should_run/cgrun034.run                                 cgrun034 [bad exit code] (llvm)
   codeGen/should_compile/massive_array.run                        massive_array [exit code non-0] (llvm)
   dependent/should_compile/T12442.run                             T12442 [exit code non-0] (llvm)
   dependent/should_compile/T12176.run                             T12176 [exit code non-0] (llvm)
   deSugar/should_run/dsrun023.run                                 dsrun023 [exit code non-0] (llvm)
   deSugar/should_run/mc03.run                                     mc03 [exit code non-0] (llvm)
   driver/T5313.run                                                T5313 [bad exit code] (llvm)
   ffi/should_run/fed001.run                                       fed001 [exit code non-0] (llvm)
   ffi/should_run/ffi006.run                                       ffi006 [exit code non-0] (llvm)
   ffi/should_run/ffi007.run                                       ffi007 [exit code non-0] (llvm)
   ffi/should_run/ffi008.run                                       ffi008 [exit code non-0] (llvm)
   ffi/should_run/ffi005.run                                       ffi005 [exit code non-0] (llvm)
   ffi/should_run/ffi011.run                                       ffi011 [exit code non-0] (llvm)
   ffi/should_run/ffi017.run                                       ffi017 [exit code non-0] (llvm)
   ffi/should_run/ffi013.run                                       ffi013 [exit code non-0] (llvm)
   ffi/should_run/ffi018.run                                       ffi018 [exit code non-0] (llvm)
   ffi/should_run/ffi016.run                                       ffi016 [exit code non-0] (llvm)
   ffi/should_run/T1288.run                                        T1288 [exit code non-0] (llvm)
   ffi/should_run/ffi019.run                                       ffi019 [exit code non-0] (llvm)
   ffi/should_run/T1679.run                                        T1679 [exit code non-0] (llvm)
   ffi/should_run/T2276.run                                        T2276 [exit code non-0] (llvm)
   ffi/should_run/T2469.run                                        T2469 [exit code non-0] (llvm)
   ffi/should_run/T2594.run                                        T2594 [exit code non-0] (llvm)
   ffi/should_run/T4038.run                                        T4038 [exit code non-0] (llvm)
   ffi/should_run/ffi_parsing_001.run                              ffi_parsing_001 [exit code non-0] (llvm)
   ffi/should_run/T4221.run                                        T4221 [exit code non-0] (llvm)
   ffi/should_run/capi_value.run                                   capi_value [exit code non-0] (llvm)
   ffi/should_run/T8083.run                                        T8083 [exit code non-0] (llvm)
   ffi/should_run/T12134.run                                       T12134 [exit code non-0] (llvm)
   ffi/should_run/T4012.run                                        T4012 [exit code non-0] (llvm)
   ghc-api/T10508_api.run                                          T10508_api [bad exit code] (llvm)
   ghc-api/dynCompileExpr/dynCompileExpr.run                       dynCompileExpr [bad exit code] (llvm)
   overloadedrecflds/should_run/overloadedrecfldsrun04.run         overloadedrecfldsrun04 [exit code non-0] (llvm)
   overloadedrecflds/should_run/overloadedlabelsrun04.run          overloadedlabelsrun04 [exit code non-0] (llvm)
   partial-sigs/should_compile/PatternSplice.run                   PatternSplice [exit code non-0] (llvm)
   partial-sigs/should_compile/SplicesUsed.run                     SplicesUsed [exit code non-0] (llvm)
   patsyn/should_run/T13688.run                                    T13688 [exit code non-0] (llvm)
   polykinds/T14174a.run                                           T14174a [exit code non-0] (llvm)
   quasiquotation/T13863/T13863.run                                T13863 [exit code non-0] (llvm)
   quotes/TH_spliceViewPat/TH_spliceViewPat.run                    TH_spliceViewPat [exit code non-0] (llvm)
   rts/T7040.run                                                   T7040 [exit code non-0] (llvm)
   rts/T5250.run                                                   T5250 [exit code non-0] (llvm)
   rts/overflow2.run                                               overflow2 [exit code non-0] (llvm)
   rts/overflow3.run                                               overflow3 [exit code non-0] (llvm)
   rts/T10904.run                                                  T10904 [exit code non-0] (llvm)
   simplCore/should_compile/T5550.run                              T5550 [exit code non-0] (llvm)
   simplCore/should_compile/T7944.run                              T7944 [exit code non-0] (llvm)
   th/should_compile/T8025/T8025.run                               T8025 [exit code non-0] (llvm)
   th/should_compile/T13949/T13949.run                             T13949 [exit code non-0] (llvm)
   rts/T12903.run                                                  T12903 [bad exit code] (llvm)
   typecheck/should_compile/tc265.run                              tc265 [exit code non-0] (llvm)
   typecheck/should_compile/holes.run                              holes [exit code non-0] (llvm)
   typecheck/should_compile/holes2.run                             holes2 [exit code non-0] (llvm)
   typecheck/should_compile/refinement_substitutions.run           refinement_substitutions [exit code non-0] (llvm)
   typecheck/should_compile/abstract_refinement_substitutions.run  abstract_refinement_substitutions [exit code non-0] (llvm)
   typecheck/should_compile/valid_substitutions.run                valid_substitutions [exit code non-0] (llvm)
   typecheck/should_compile/T13050.run                             T13050 [exit code non-0] (llvm)
   typecheck/should_compile/T14590.run                             T14590 [exit code non-0] (llvm)
   ../../libraries/template-haskell/tests/dataToExpQUnit.run       dataToExpQUnit [exit code non-0] (llvm)
```


---

## Tuning LLVM IR Passes


The optimization pass sequence in LLVM is well tested for languages like C/C++, but not Haskell. See #11295 for details and progress updates.

## Improving Heap Checks


See #8905 and #12231 and \[Compiling case expressions\] in StgCmmExpr

---


Here's something that's probably worth improving.


Code like this

```wiki
if y > 17
  then joinP (y - x)
  else joinP 10
```


Turns into this:

```wiki
  call GHC.Classes.>_info(R2) returns to c2mn, args: 32, res: 8, upd: 8;   // CmmCall
c2mn: // global
  _s2lC::P64 = P64[Sp + 8];   // CmmAssign
  _s2lE::P64 = P64[Sp + 24];   // CmmAssign
  if (R1 & 7 != 1) goto c2n1; else goto c2mX;   // CmmCondBranch
c2n1: // global
  Hp = Hp + 40;   // CmmAssign
  if (Hp > HpLim) (likely: False) goto c2n4; else goto c2n3;   // CmmCondBranch
c2mX: // global
  Hp = Hp + 24;   // CmmAssign
  if (Hp > HpLim) (likely: False) goto c2n0; else goto c2mZ;   // CmmCondBranch
```


Since both branches of the first conditional test lead to a heap check, it's better to commute the conditional tests here so we only have one heap test, since the heap tests produce extra code to enter the GC. The above would transform into:

```wiki
  call GHC.Classes.>_info(R2) returns to mergedHeapTest, args: 32, res: 8, upd: 8;   // CmmCall
mergedHeapTest:
  _localTemp = Hp + 40; // max(40, 24)
  if (_localTemp > HpLim) (likely: False) goto needGC; else goto continue;
needGC:
  HpAlloc = 40; // max(40, 24)
  R1 = R1;
  call stg_gc_unpt_r1(R1) returns to mergedHeapTest
continue:
  _s2lC::P64 = P64[Sp + 8];   // CmmAssign
  _s2lE::P64 = P64[Sp + 24];   // CmmAssign
  if (R1 & 7 != 1) goto leftSide; else goto rightSide;
leftSide:
  Hp = Hp + 40;  // could be merged into c2n3 if it has one pred.
  goto c2n3;
rightSide:
  Hp = Hp + 24;  // could be merged into c2mZ if it has one pred.
  goto c2mZ;
```

---


### Other Performance Bugs To Consider


- #12798
- #12808
