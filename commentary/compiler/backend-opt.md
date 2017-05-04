# Planned Backend Optimizations


This page gathers together a list of ideas and plans for improving the backend of the compiler. Any progress made will also be noted here.

## Removal of Proc Point Splitting


The reason we needed proc-point splitting for the LLVM backend is detailed [here](commentary/compiler/backends/llvm/wip#get-rid-of-proc-point-splitting).

#### Rationale


Ideally, we would expose an entire Cmm function to LLVM for better optimization and machine code generation, instead of breaking the functions apart just to grab a valid label.
The proc-point splitting is also a rather ugly transformation we would like to remove from the pipeline.

#### Challenges


The address of a basic block is [ not a well-defined concept](http://llvm.org/docs/LangRef.html#addresses-of-basic-blocks) outside of an LLVM function.
There has been no movement on this in LLVM due to the key assumption made by LLVM's IR that all basic blocks have *known* predecessors, forming a complete control-flow graph amongst blocks.
An escaping block label opens up the possibility of an edge from an unknown location.
This is why an indirect branch instruction must conservatively list [ all possible block targets](http://llvm.org/docs/LangRef.html#i-indirectbr).

#### Proposal


The current proposal can be found [ here.](http://lists.llvm.org/pipermail/llvm-dev/2017-April/112144.html)

#### Implementation Progress


After recieving a helpful [ suggestion](http://lists.llvm.org/pipermail/llvm-dev/2017-April/112214.html) from Ried on the LLVM mailing list, I've extended LLVM with an experimental CPS call. To help understand how it works, let's consider the following Cmm code:

```wiki
H:
  if x != 0 goto G; else goto J

G:
  x = x - 1
  if SP >= 1000 goto needGC; else goto H

needGC:
  call doGC returns to G  // a non-tail CPS call

J:
  call X  // a tail CPS call
```

`G` is a return point (aka, proc-point) that is also branched to locally by `H`. First, we will run a simple Cmm pass that introduces a new stand-in block for each block that is returned to by a call. Thus, the above will look like this:

```wiki
H:
  ...
  if ... goto G; else goto J

G:
  if ... goto needGC; else goto H

needGC:
  call doGC returns to G_standin

G_standin:
  goto G

J:
  call X
```


Stand-ins are currently required for the CPS call functionality in LLVM to work properly.
They are needed in this example because `G` is a loop header, and LLVM will introduce loop preheader.
A preheader merges incoming edges to `G` that are not back-edges, and in particular, it will place a block between the call in `needGC` and its return point, `G`, unless if there is a stand-in.


There's no harm in just generating stand-ins for all return points, since LLVM will merge them later.
Also note that `H` still branches directly to `G`.


Now, with the stand-ins in place, we generate LLVM IR corresponding to the last example that looks like this:

```wiki
type %structTy = {i64, i64, i64, i64, ....}

H:
  ...
  if ... goto G; else goto J

G:
  if ... goto needGC; else goto H

needGC:
  %retAddr = blockaddress(@thisFunc, %G_standin)
  store %retAddr, %SP
  %retVals = call ghccc %structTy @doGC (... args ...)
  %rv0 = extractvalue %retVals, 0
  %rv1 = extractvalue %retVals, 1
  ...
  %rvN = extractvalue %retVals, N
  goto G_standin

G_standin:
  goto G

J:
  %dummy = tail call ghccc %structTy @X (... args ...)
  ret %structTy %dummy
```


Notice that the non-tail CPS call is emitted as a non-tail call in LLVM, with a special register convention added to the GHC calling convention to match up the registers used to return the struct fields in `%retVals`.
This non-tail ghccc call is now interpreted by LLVM's code generator as a CPSCALL pseudo-instruction during isel.


Another important property of this LLVM IR is that the address of `G_standin` was taken, so LLVM cannot merge that block with its predecessor. The name of `G_standin` is important later when we place GC information above that block using the LLVM mangler.

TODO It seems jump-chain elimination can end up changing the block following the CPS call to be the actual target (it safely updates the `blockaddress`, though). Either we need to actually split apart the block, and somehow pass the desired block name to llc... or have the mangler look for the block whose address was taken, whose prefix is G?


Here is what the `needGC` block looks like by the time we're ready to expand the CPSCALL pseudo-instruction:

```wiki
  BB#4: derived from LLVM BB %needGC
    Predecessors according to CFG: BB#3
  %vreg10<def> = LEA64r %RIP, 1, %noreg, <blockaddress(@foo, %G)>, %noreg; GR64:%vreg10
  MOV64mr %vreg3, 1, %noreg, 0, %noreg, %vreg10<kill>; mem:ST8[%G_phi_sp] GR64:%vreg3,%vreg10
  ADJCALLSTACKDOWN64 0, 0, %RSP<imp-def,dead>, %EFLAGS<imp-def,dead>, %RSP<imp-use>
  %R13<def> = COPY %vreg3; GR64:%vreg3
  %RBP<def> = COPY %vreg4; GR64:%vreg4
  CPSCALLdi64 <ga:@doGC>, <regmask>, %RSP<imp-use>, %R13<imp-use>, %RBP<imp-use>, %RSP<imp-def>, %R13<imp-def>, %RBP<imp-def>
  ADJCALLSTACKUP64 0, 0, %RSP<imp-def,dead>, %EFLAGS<imp-def,dead>, %RSP<imp-use>
  %vreg11<def> = COPY %R13; GR64:%vreg11
  %vreg12<def> = COPY %RBP; GR64:%vreg12
  %vreg5<def> = COPY %vreg11; GR64:%vreg5,%vreg11
  %vreg6<def> = COPY %vreg12; GR64:%vreg6,%vreg12
  JMP_1 <BB#3>
    Successors according to CFG: BB#3(?%)
```


When LLVM's `expand-isel-pseudos` pass is run after isel is complete, we expand the CPSCALL at the MachineBasicBlock level in the following way:

1. The instructions following the CPSCALL are analyzed to determine which virtual registers correspond to physical registers used by the `ghccc` convention to return those struct fields.

1. The stack adjustment following the CPSCALL is moved before the CPSCALL.

1. The remaining instructions after the CPSCALL are then deleted, leaving the CPSCALL pseudo-op at the end of the `needGC` block.

1. The CPSCALL is converted into a TCRETURN, which is the pseudo-instruction used in the x86 backend of LLVM to later emit an LLVM tail-call.

1. The `G_standin` block has physical registers added to it as live-in values, it is marked as an EH landing pad, and `vReg = COPY pReg` copies are emitted in that block.

1. The phi-nodes of `G` are updated so that the virtual registers taken from `needGC` now come from `G_standin` instead.


This leaves us with the following MachineBasicBlock representation:

```wiki
  BB#4: derived from LLVM BB %needGC
    Predecessors according to CFG: BB#6
  %vreg10<def> = LEA64r %RIP, 1, %noreg, <blockaddress(@foo, %G)>, %noreg; GR64:%vreg10
  MOV64mr %vreg3, 1, %noreg, 0, %noreg, %vreg10<kill>; mem:ST8[%G_phi_sp] GR64:%vreg3,%vreg10
  ADJCALLSTACKDOWN64 0, 0, %RSP<imp-def,dead>, %EFLAGS<imp-def,dead>, %RSP<imp-use>
  %R13<def> = COPY %vreg3; GR64:%vreg3
  %RBP<def> = COPY %vreg4; GR64:%vreg4
  ADJCALLSTACKUP64 0, 0, %RSP<imp-def>, %EFLAGS<imp-def,dead>, %RSP<imp-use>
  TCRETURNdi64 <ga:@doGC>, 0, %RSP<imp-use>, %R13<imp-use>, %RBP<imp-use>
    Successors according to CFG: BB#3(?%)

BB#3: derived from LLVM BB %G, EH LANDING PAD, ADDRESS TAKEN
    Live Ins: %R13 %RBP
    Predecessors according to CFG: BB#4
  %vreg6<def> = COPY %RBP<kill>; GR64:%vreg6
  %vreg5<def> = COPY %R13<kill>; GR64:%vreg5
  JMP_1 <BB#6>
    Successors according to CFG: BB#6(?%)

BB#6: 
    Predecessors according to CFG: BB#2 BB#3
  %vreg2<def> = PHI %vreg0, <BB#2>, %vreg6, <BB#3>; GR64:%vreg2,%vreg0,%vreg6
  %vreg3<def> = PHI %vreg1, <BB#2>, %vreg5, <BB#3>; GR64:%vreg3,%vreg1,%vreg5
  %vreg4<def,tied1> = DEC64r %vreg2<tied0>, %EFLAGS<imp-def,dead>; GR64:%vreg4,%vreg2
  %vreg9<def,tied1> = SUB64ri32 %vreg3<tied0>, 1000, %EFLAGS<imp-def>; GR64:%vreg9,%vreg3
  JL_1 <BB#1>, %EFLAGS<imp-use>
  JMP_1 <BB#4>
    Successors according to CFG: BB#4(0x7c000000 / 0x80000000 = 96.88%) BB#1(0x04000000 / 0x80000000 = 3.12%)
```


The machine code for the loops in this example end up looking quite nice:

```
LBB0_5:## %needGC
leaqLtmp0(%rip),%rdxmovq%rdx,(%rax)movq%rax,%r13movq%rcx,%rbppopq%rax# modify PEI & RTS to omit this.
jmp_doGC## TAILCALL
Ltmp0:## Block address taken
LBB0_3:## %G
movq%rbp,%rcxmovq%r13,%raxLBB0_4:## BB#6
decq%rcxcmpq$1000,%raxjgeLBB0_5LBB0_1:## %H
testq%rcx,%rcxjneLBB0_4
```

---

## Improving Heap Checks


See [\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905) and [\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231) and \[Compiling case expressions\] in StgCmmExpr

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

- [\#12798](https://gitlab.haskell.org//ghc/ghc/issues/12798)
- [\#12808](https://gitlab.haskell.org//ghc/ghc/issues/12808)