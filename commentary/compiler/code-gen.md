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
- [Planned Backend Optimizations](commentary/compiler/backend-opt)
- [Details about how we place basic blocks](commentary/compiler/code-layout)

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

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1498">#1498</a></th>
<td>Optimisation: eliminate unnecessary heap check in recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2725">#2725</a></th>
<td>Remove Hack in compiler/nativeGen/X86/CodeGen.hs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2731">#2731</a></th>
<td>Avoid unnecessary evaluation when unpacking constructors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8326">#8326</a></th>
<td>Place heap checks common in case alternatives before the case</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8871">#8871</a></th>
<td>No-op assignment I64[BaseReg + 784] = I64[BaseReg + 784]; is generated into optimized Cmm</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8887">#8887</a></th>
<td>Double double assignment in optimized Cmm on SPARC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8903">#8903</a></th>
<td>Add dead store elimination</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8905">#8905</a></th>
<td>Function arguments are always spilled/reloaded if scrutinee is already in WHNF</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9718">#9718</a></th>
<td>Avoid TidyPgm predicting what CorePrep will do</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10012">#10012</a></th>
<td>Cheap-to-compute values aren&apos;t pushed into case branches inducing unnecessary register pressure</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10074">#10074</a></th>
<td>Implement the &apos;Improved LLVM Backend&apos; proposal</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12232">#12232</a></th>
<td>Opportunity to do better in register allocations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13861">#13861</a></th>
<td>Take more advantage of STG representation invariance (follows up #9291)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13904">#13904</a></th>
<td>LLVM does not need to trash caller-saved registers.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14226">#14226</a></th>
<td>Common Block Elimination pass doesn&apos;t eliminate common blocks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14372">#14372</a></th>
<td>CMM contains a bunch of tail-merging opportunities</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14373">#14373</a></th>
<td>Introduce PTR-tagging for big constructor families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14461">#14461</a></th>
<td>Reuse free variable lists through nested closures</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14626">#14626</a></th>
<td>No need to enter a scrutinised value</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14672">#14672</a></th>
<td>Make likelyhood of branches/conditions available throughout the compiler.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14677">#14677</a></th>
<td>Code generator does not correctly tag a pointer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14791">#14791</a></th>
<td>Move stack checks out of code paths that don&apos;t use the stack.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14830">#14830</a></th>
<td>Use test instead of cmp for comparison against zero.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14914">#14914</a></th>
<td>Only turn suitable targets into a fallthrough in CmmContFlowOpt.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14971">#14971</a></th>
<td>Use appropriatly sized comparison instruction for small values.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15113">#15113</a></th>
<td>Do not make CAFs from literal strings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15124">#15124</a></th>
<td>Improve block layout for the NCG</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15126">#15126</a></th>
<td>Opportunity to compress common info table representation.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15148">#15148</a></th>
<td>Allow setting of custom alignments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15155">#15155</a></th>
<td>How untagged pointers sneak into banged fields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15258">#15258</a></th>
<td>Implement CMOV support.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15580">#15580</a></th>
<td>Specialize min/max functions for GHC provided instances.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15770">#15770</a></th>
<td>Missing optimisation opportunity in code gen for always-saturated applications?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15901">#15901</a></th>
<td>Assert and record that code generation requires distinct uiques for let-binders</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16064">#16064</a></th>
<td>Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16243">#16243</a></th>
<td>Improve fregs-graph.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16333">#16333</a></th>
<td>Implement Loop-invariant code motion / Hoisting for Cmm</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16354">#16354</a></th>
<td>LLVM Backend generates invalid assembly.</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4121">#4121</a></th>
<td>Refactor the plumbing of CafInfo to make it more robust</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7571">#7571</a></th>
<td>LLVM codegen does not handle integer literals in branch conditionals.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7574">#7574</a></th>
<td>Register allocator chokes on certain branches with literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7575">#7575</a></th>
<td>LLVM backend does not properly widen certain literal types in call expressions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8585">#8585</a></th>
<td>Loopification should omit stack check</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9157">#9157</a></th>
<td>cmm common block not eliminated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9159">#9159</a></th>
<td>cmm case, binary search instead of jump table</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11372">#11372</a></th>
<td>Loopification does not trigger for IO even if it could</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12095">#12095</a></th>
<td>GHC and LLVM don&apos;t agree on what to do with byteSwap16#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14644">#14644</a></th>
<td>Improve cmm/assembly for pattern matches with two constants.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14666">#14666</a></th>
<td>Improve assembly for dense jump tables.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14989">#14989</a></th>
<td>CBE pass 2 invalidates proc points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15103">#15103</a></th>
<td>Speed optimizations for elimCommonBlocks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15104">#15104</a></th>
<td>Update JMP_TBL targets during shortcutting for x86 codegen.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15188">#15188</a></th>
<td>Catch cases where both branches of an if jump to the same block.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15196">#15196</a></th>
<td>Invert floating point comparisons such that no extra parity check is required.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15867">#15867</a></th>
<td>STG scope error</td></tr></table>



