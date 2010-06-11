# Development Notes & Bugs


This page contains information which is of interest to those hacking on the LLVM back-end.

# LLVM Bugs

## NoReturn


Don't use the `NoReturn` function attribute. It causes the LLVM optimiser to produce bad code as it replaces the following sequence of instructions:

```wiki
tail call fastcc void (i32,i32,i32,i32)* %nnO( i32 %nnP,i32 %nnQ,i32 %nnR,i32 %nnS )
ret void
```


with:

```wiki
tail call fastcc void (i32,i32,i32,i32)* %nnO( i32 %nnP,i32 %nnQ,i32 %nnR,i32 %nnS )
unreachable
```


which stops `llc` producing native code that actually tail calls and thus leads to a runtime segfault.

**TODO**: Need to investigate this further and submit a bug report to LLVM.

# GHC LLVM Back-end Bugs

## Foreign Calls on Mac OSX


Foreign calls on Mac OS X don't work. Seems to be because LLVM isn't generating correct code. All system calls must be 16 byte aligned in OS X and llvm isn't respecting this. Not sure if its a bug in LLVM or due to my changes to LLVM.


Update (20/02/10): I fixed this issue using the inline assembler approach (see below). This reduces the test failures on Mac OSX from 22 to 9. So doesn't fix everything. Still other issues. Also, I tried using the new stalk alignment feature but that interacts badly with the GHC calling convention, clobbering the Base register.

**Solutions**:

- A new function attribute has just landed in SVN which allows stack alignment to be specified when a call is made.
- Can use inline assembler to fix stack alignment.
- Fix stack calculation in LLVM (my changes must have broken it).

## Known Function mistaken for Unknown External Label


If a function is initially used as a label (e.g the address of it is taken) then the code generator creates an external reference label for it. Later if that function is called directly as a funciton then as it has previously been defined as a function the code generator gets confused and creates an invalid bitcast. Could either look to redefine the function label when more information is encountered, or just fix up the bitcast.

## Segfault running HRay

[ HRay](http://users.elis.ugent.be/~kehoste/Haskell/HRay/) is a Haskell Ray Tracer. If you download it and build it with the LLVM backend, some scenes (such as trans2, provided example scene) cause it to segfault. If built with NCG instead this doesn't occur.

## Possible Problems (Unconfirmed Bugs)

- See GHC trac ticket [\#1852](https://gitlab.haskell.org//ghc/ghc/issues/1852). Floats are padded to word size (4 extra bytes on a 64 bit machine) by putting an appropriate `CmmLit` before them. On `fasm` this is necessary and forces the NCG to produce correct code. On `fvia-C`, this isn't necessary so it strips this padding out. What approach does LLVM blocks end in a control flow statement which seems pretty useful to me.  need?

- `SPARC/CodeGen/Gen32.hs` seems to have a few special cases for `CmmMachOp`. Perhaps these should also be handled in LLVM to improve performance?

- tail call only supported on `x86`/`x86-64` and `PowerPC`. What about `SPARC`? How will we use the LLVM back-end on SPARC?
