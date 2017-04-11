# Code Generator


This page describes code generator ("codegen") in GHC. It is meant to reflect current state of the implementation. If you notice any inaccuracies please update the page (if you know how) or complain on ghc-devs.

- [Overview of the code generator](commentary/compiler/code-gen/overview)
- [Data types and modules for the code generator](commentary/compiler/new-code-gen-modules)
- [The STG language and how to execute it](commentary/compiler/generated-code)
- [List of code-gen stupidities](commentary/compiler/new-code-gen-stupidity) (some, but not all, fixed).
- [Clean-up ideas once the new codegen is in place (i.e. now)](commentary/compiler/new-code-gen/cleanup); not all done.
- [Loopification](commentary/compiler/loopification) i.e. turn tail calls into loops
- [LLVM back end](commentary/compiler/backends/llvm)
- [Hoopl/Cleanup](hoopl/cleanup)

## A brief history of code generator


You might occasionally hear about "old" and "new" code generator. GHC 7.6 and earlier used the old code generator. New code generator was being developed since 2007 and it was [enabled by default on 31 August 2012](/trac/ghc/changeset/832077ca5393d298324cb6b0a2cb501e27209768/ghc) after the release of GHC 7.6.1. The first stable GHC to use the new code generator is 7.8.1 released in early 2014. 


Various historical pages, with still-useful info:

- [Commentary on the old code generator](commentary/compiler/old-code-gen)

- [Status page on the "new code generator"](commentary/compiler/new-code-gen) (now the current one)
- [Replace native code generator with LLVM](commentary/compiler/backends/llvm/replacing-ncg)
- [IntegratedCodeGen](commentary/compiler/integrated-code-gen) One plan is to expand the capability of the pipeline so that it does native code generation too so that existing backends can be discarded.

## Tickets


Use Keyword = `CodeGen` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498)</th>
<td>Optimisation: eliminate unnecessary heap check in recursive function</td></tr>
<tr><th>[\#2725](https://gitlab.haskell.org//ghc/ghc/issues/2725)</th>
<td>Remove Hack in compiler/nativeGen/X86/CodeGen.hs</td></tr>
<tr><th>[\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731)</th>
<td>Avoid unnecessary evaluation when unpacking constructors</td></tr>
<tr><th>[\#8326](https://gitlab.haskell.org//ghc/ghc/issues/8326)</th>
<td>Place heap checks common in case alternatives before the case</td></tr>
<tr><th>[\#8871](https://gitlab.haskell.org//ghc/ghc/issues/8871)</th>
<td>No-op assignment I64\[BaseReg + 784\] = I64\[BaseReg + 784\]; is generated into optimized Cmm</td></tr>
<tr><th>[\#8887](https://gitlab.haskell.org//ghc/ghc/issues/8887)</th>
<td>Double double assignment in optimized Cmm on SPARC</td></tr>
<tr><th>[\#8903](https://gitlab.haskell.org//ghc/ghc/issues/8903)</th>
<td>Add dead store elimination</td></tr>
<tr><th>[\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905)</th>
<td>Function arguments are always spilled/reloaded if scrutinee is already in WHNF</td></tr>
<tr><th>[\#9718](https://gitlab.haskell.org//ghc/ghc/issues/9718)</th>
<td>Avoid TidyPgm predicting what CorePrep will do</td></tr>
<tr><th>[\#10012](https://gitlab.haskell.org//ghc/ghc/issues/10012)</th>
<td>Cheap-to-compute values aren't pushed into case branches inducing unnecessary register pressure</td></tr>
<tr><th>[\#10074](https://gitlab.haskell.org//ghc/ghc/issues/10074)</th>
<td>Implement the 'Improved LLVM Backend' proposal</td></tr>
<tr><th>[\#12232](https://gitlab.haskell.org//ghc/ghc/issues/12232)</th>
<td>Opportunity to do better in register allocations</td></tr>
<tr><th>[\#13861](https://gitlab.haskell.org//ghc/ghc/issues/13861)</th>
<td>Take more advantage of STG representation invariance (follows up \#9291)</td></tr>
<tr><th>[\#13904](https://gitlab.haskell.org//ghc/ghc/issues/13904)</th>
<td>LLVM does not need to trash caller-saved registers.</td></tr>
<tr><th>[\#14226](https://gitlab.haskell.org//ghc/ghc/issues/14226)</th>
<td>Common Block Elimination pass doesn't eliminate common blocks</td></tr>
<tr><th>[\#14372](https://gitlab.haskell.org//ghc/ghc/issues/14372)</th>
<td>CMM contains a bunch of tail-merging opportunities</td></tr>
<tr><th>[\#14373](https://gitlab.haskell.org//ghc/ghc/issues/14373)</th>
<td>Introduce PTR-tagging for big constructor families</td></tr>
<tr><th>[\#14461](https://gitlab.haskell.org//ghc/ghc/issues/14461)</th>
<td>Reuse free variable lists through nested closures</td></tr>
<tr><th>[\#14626](https://gitlab.haskell.org//ghc/ghc/issues/14626)</th>
<td>No need to enter a scrutinised value</td></tr>
<tr><th>[\#14672](https://gitlab.haskell.org//ghc/ghc/issues/14672)</th>
<td>Make likelyhood of branches/conditions available throughout the compiler.</td></tr>
<tr><th>[\#14677](https://gitlab.haskell.org//ghc/ghc/issues/14677)</th>
<td>Code generator does not correctly tag a pointer</td></tr>
<tr><th>[\#14791](https://gitlab.haskell.org//ghc/ghc/issues/14791)</th>
<td>Move stack checks out of code paths that don't use the stack.</td></tr>
<tr><th>[\#14830](https://gitlab.haskell.org//ghc/ghc/issues/14830)</th>
<td>Use test instead of cmp for comparison against zero.</td></tr>
<tr><th>[\#14914](https://gitlab.haskell.org//ghc/ghc/issues/14914)</th>
<td>Only turn suitable targets into a fallthrough in CmmContFlowOpt.</td></tr>
<tr><th>[\#14971](https://gitlab.haskell.org//ghc/ghc/issues/14971)</th>
<td>Use appropriatly sized comparison instruction for small values.</td></tr>
<tr><th>[\#15113](https://gitlab.haskell.org//ghc/ghc/issues/15113)</th>
<td>Do not make CAFs from literal strings</td></tr>
<tr><th>[\#15124](https://gitlab.haskell.org//ghc/ghc/issues/15124)</th>
<td>Improve block layout for the NCG</td></tr>
<tr><th>[\#15126](https://gitlab.haskell.org//ghc/ghc/issues/15126)</th>
<td>Opportunity to compress common info table representation.</td></tr>
<tr><th>[\#15148](https://gitlab.haskell.org//ghc/ghc/issues/15148)</th>
<td>Allow setting of custom alignments</td></tr>
<tr><th>[\#15155](https://gitlab.haskell.org//ghc/ghc/issues/15155)</th>
<td>How untagged pointers sneak into banged fields</td></tr>
<tr><th>[\#15258](https://gitlab.haskell.org//ghc/ghc/issues/15258)</th>
<td>Implement CMOV support.</td></tr>
<tr><th>[\#15580](https://gitlab.haskell.org//ghc/ghc/issues/15580)</th>
<td>Specialize min/max functions for GHC provided instances.</td></tr>
<tr><th>[\#15770](https://gitlab.haskell.org//ghc/ghc/issues/15770)</th>
<td>Missing optimisation opportunity in code gen for always-saturated applications?</td></tr>
<tr><th>[\#15901](https://gitlab.haskell.org//ghc/ghc/issues/15901)</th>
<td>Assert and record that code generation requires distinct uiques for let-binders</td></tr>
<tr><th>[\#16064](https://gitlab.haskell.org//ghc/ghc/issues/16064)</th>
<td>Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code</td></tr>
<tr><th>[\#16243](https://gitlab.haskell.org//ghc/ghc/issues/16243)</th>
<td>Improve fregs-graph.</td></tr>
<tr><th>[\#16333](https://gitlab.haskell.org//ghc/ghc/issues/16333)</th>
<td>Implement Loop-invariant code motion / Hoisting for Cmm</td></tr>
<tr><th>[\#16354](https://gitlab.haskell.org//ghc/ghc/issues/16354)</th>
<td>LLVM Backend generates invalid assembly.</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#4121](https://gitlab.haskell.org//ghc/ghc/issues/4121)</th>
<td>Refactor the plumbing of CafInfo to make it more robust</td></tr>
<tr><th>[\#7571](https://gitlab.haskell.org//ghc/ghc/issues/7571)</th>
<td>LLVM codegen does not handle integer literals in branch conditionals.</td></tr>
<tr><th>[\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574)</th>
<td>Register allocator chokes on certain branches with literals</td></tr>
<tr><th>[\#7575](https://gitlab.haskell.org//ghc/ghc/issues/7575)</th>
<td>LLVM backend does not properly widen certain literal types in call expressions</td></tr>
<tr><th>[\#8585](https://gitlab.haskell.org//ghc/ghc/issues/8585)</th>
<td>Loopification should omit stack check</td></tr>
<tr><th>[\#9157](https://gitlab.haskell.org//ghc/ghc/issues/9157)</th>
<td>cmm common block not eliminated</td></tr>
<tr><th>[\#9159](https://gitlab.haskell.org//ghc/ghc/issues/9159)</th>
<td>cmm case, binary search instead of jump table</td></tr>
<tr><th>[\#11372](https://gitlab.haskell.org//ghc/ghc/issues/11372)</th>
<td>Loopification does not trigger for IO even if it could</td></tr>
<tr><th>[\#12095](https://gitlab.haskell.org//ghc/ghc/issues/12095)</th>
<td>GHC and LLVM don't agree on what to do with byteSwap16\#</td></tr>
<tr><th>[\#14644](https://gitlab.haskell.org//ghc/ghc/issues/14644)</th>
<td>Improve cmm/assembly for pattern matches with two constants.</td></tr>
<tr><th>[\#14666](https://gitlab.haskell.org//ghc/ghc/issues/14666)</th>
<td>Improve assembly for dense jump tables.</td></tr>
<tr><th>[\#14989](https://gitlab.haskell.org//ghc/ghc/issues/14989)</th>
<td>CBE pass 2 invalidates proc points</td></tr>
<tr><th>[\#15103](https://gitlab.haskell.org//ghc/ghc/issues/15103)</th>
<td>Speed optimizations for elimCommonBlocks</td></tr>
<tr><th>[\#15104](https://gitlab.haskell.org//ghc/ghc/issues/15104)</th>
<td>Update JMP_TBL targets during shortcutting for x86 codegen.</td></tr>
<tr><th>[\#15188](https://gitlab.haskell.org//ghc/ghc/issues/15188)</th>
<td>Catch cases where both branches of an if jump to the same block.</td></tr>
<tr><th>[\#15196](https://gitlab.haskell.org//ghc/ghc/issues/15196)</th>
<td>Invert floating point comparisons such that no extra parity check is required.</td></tr>
<tr><th>[\#15867](https://gitlab.haskell.org//ghc/ghc/issues/15867)</th>
<td>STG scope error</td></tr></table>