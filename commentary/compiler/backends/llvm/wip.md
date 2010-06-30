# Work in Progress on the LLVM Backend


This page is meant to collect together information about people working on (or interested in working on) LLVM in GHC, and the projects they are looking at.  See also the [state of play of the whole back end](commentary/compiler/new-code-gen).

- David Terei is going to be at Microsoft Research for an internship to work on LLVM from the middle of April
- There is interest on working on LLVM as part of the Google Summer of Code

  - The GSoC timeline is May 24th - August 9th ([ http://socghop.appspot.com/document/show/gsoc_program/google/gsoc2010/faqs\#timeline](http://socghop.appspot.com/document/show/gsoc_program/google/gsoc2010/faqs#timeline)), so this overlaps with David's internship
  - Max Bolingbroke ([ http://www.cl.cam.ac.uk/\~mb566](http://www.cl.cam.ac.uk/~mb566)) has proposed a SoC project to work on LLVM [ http://hackage.haskell.org/trac/summer-of-code/ticket/1582](http://hackage.haskell.org/trac/summer-of-code/ticket/1582)
  - Alp Mestanogullari ([ http://alpmestan.wordpress.com/](http://alpmestan.wordpress.com/), [ http://twitter.com/alpmestan](http://twitter.com/alpmestan)) is interested in working on a SoC project on LLVM

## Small Ticket Items

- Use a new Monad instead of passing `LlvmEnv` around everywhere.
- Should be able to put all `CmmProc` and `CmmData` labels in environment at start and after that, can print out LLVM IR as I generate it for each data and proc instead of storing.
- Look at using LLVM intrinsic functions. There are a few math functions. Also, there is a `smul_overflow` detect function.
- Rearrange some functions and files better.
- handling of `LlvmVar` or `LlvmType` for function signature isn't nice. Whole function signature handling could be better really. We also don't support parameter attributes which we should enable for better performance.
- `LlvmCodeGen.CodeGen.genCall` code for foreign calls is quite complex, could use a clean-up.

## Big Ticket Items

### LLVM IR Representation


The LLVM IR is modeled in GHC using an algebraic data type to represent the first order abstract syntax of the LLVM assembly code. The LLVM representation lives in the 'Llvm' subdirectory and also contains code for pretty printing. This is the same approach taken by  EHC's LLVM Back-end, and we adapted the  module developed by them for this purpose. 


The current design is overly complicated and could be faster. It uses String + show operations for printing for example when it should be using FastString + Outputable. Before simplifying this design though it would be good to investigate using the LLVM API instead of the assembly language for interacting with LLVM. This would be done most likely by using the pre-existing Haskell LLVM API bindings found [ here](http://hackage.haskell.org/package/llvm). This should hopefully provide a speed up in compilation speeds which is greatly needed since the LLVM back-end is \~2x slower at the moment.

### TABLES_NEXT_TO_CODE


We now support [TNTC](commentary/compiler/backends/llvm/issues#) using an approach of gnu as subsections. This seems to work fine but we would like still to move to a pure LLVM solution. Ideally we would implement this in LLVM by allowing a global variable to be associated with a function, so that LLVM is aware that the two will be laid out next to each other and can better optimise (e.g using this approach LLVM should be able to perform constant propagation on info-tables).

**Update (30/06/2010):** The current TNTC solution doesn't work on Mac OS X. So we need to implement an LLVM based solution.

### Optimise the output of the LLVM Back-end


The LLVM back-end at the moment generally takes the most straight-forward approach to compiling Haskell (Cmm really) to LLVM. LLVM is designed in such a way that this is how things should be by and large done. Its instruction set is designed to be simple and generally have one way to approach a problem (especially when coming from fairly similar Cmm), you are encouraged to rely on the optimisation passes of LLVM to handle fixing things up. However, this doesn't mean there isn't potentially some room for improvement, especially since we simply don't know if there is or isn't. The LLVM back-end is new and experiments and benchmarks need to be done to figure out its limits and places it can be improved. Some quick ideas:

- Update the back-end to use some of the new features of LLVM 2.6 and 2.7. Currently it only uses features of 2.5. (e.g could maybe use the new LLVM integer specific add operation to detect overflow rather then the current custom code to do it). One quick improvement is that as of 2.7 the LLVM assembler ('llvm-as') stage in the LLVM back-end pipeline isn't needed now at the LLVM optimiser tool ('opt') can be directly given LLVM assembly now as well as LLVM bitcode.
- All the STG registers are passed around at the moment as just words. Some really should be passed as pointer type (e.g Sp, Hp). We can then use the noalias attribute on them which is useful. Also the nocapture attribute
- Look into the various [ parameter attributes](http://llvm.org/docs/LangRef.html#paramattrs) and [ function attributes](http://llvm.org/docs/LangRef.html#fnattrs) that LLVM supports and how they should be used by the LLVM back-end. (e.g the noalias parameter attribute should probably be used).
- Look at the various [ intrinsic functions](http://llvm.org/docs/LangRef.html#intrinsics) supported by LLVM. Some of them could maybe be used to replace existing code in LLVM or calls to the rts. (e.g Cmm expects support of a fair number of basic math operations \[e.g sin\], for which LLVM intrinsic functions exists. However the back-end currently calls the C library for them).

### Optimise LLVM for the type of Code GHC produces


At the moment only a some fairly basic benchmarking has been done of the LLVM back-end. Enough to give an indication of how it performs on the whole (well as far as you trust benchmarks anyway) and of what it can sometimes achieve. However this is by no means exauhstive or probably even close to it and doesn't give us enough information about the areas where LLVM performs badly. The LLVM optimisation pass also at the moment just uses the standard '-O\[123\]' levels, which like GCC entail a whole bunch of optimisation passes. These groups are designed for C programs mostly.


So:

- More benchmarking, particularly finding some bad spots for the LLVM back-end and generating a good picture of the characteristics of the back-end.
- Look into the LLVM optimiser, e.g perhaps some more work in the style of [ Don's work](http://donsbot.wordpress.com/2010/03/01/evolving-faster-haskell-programs-now-with-llvm/)
- Look at any new optimisation passes that could be written for LLVM which would help to improve the code it generates for GHC.
- Look at general fixes/improvement to LLVM to improve the code it generates for LLVM (e.g at the moment LLVM performs a lot of redundant stack manipulation in the code in generates for GHC, would be good to fix this up).

### Stabilise / Bug Fixing


The back-end needs a fair amount of love and care just to get it into a state where it could be used as the default back-end by GHC if desired.

- **Platform support**: Only supports x86 Linux. There are a number of serious bugs on Mac OS X. Windows hasn't been tested. SPARC also hasn't been tested and would need to have changes made in LLVM so that the SPARC LLVM back-end supported the GHC calling convention.
- Has been a report the back-end interacts badly with the '-dynamic' GHC flag.
- Back-end hasn't been thoroughly tested across the full range of GHC configurations (e.g threaded...)
- LLVM back-end is out of tree currently.
- Back-end can be reduced in size and use faster data structures (FastString instead of String, OrdList instead of List, might be able to get rid of the environment used by the back-end as I believe the label naming convention stores may store enough information for the back-ends uses).

### Update the Back-end to use the new Cmm data types / New Code Generator


There is ongoing work to produce a new, nicer, more modular code generator for GHC (the slightly confusingly name code generator in GHC refers to the pipeline stage where the Core IR is compiled to the Cmm IR). The LLVM back-end could be updated to make sure it works with the new code generator and does so in an efficient manner.

### LLVM's Link Time Optimisations


One of LLVM's big marketing features is its support for link time optimisation. This does thinks such as in-lining across module boundaries, more aggressive dead code elimination... ect). The LLVM back-end could be updated to make use of this. Roman apparently tried to use the new 'gold' linker with GHC and it doesn't support all the needed features.

- [ http://llvm.org/releases/2.6/docs/LinkTimeOptimization.html](http://llvm.org/releases/2.6/docs/LinkTimeOptimization.html)
- [ http://llvm.org/docs/GoldPlugin.html](http://llvm.org/docs/GoldPlugin.html)

### LLVM Cross Compiler / Port


This is more of an experimental idea but the LLVM back-end looks like it would make a great choice for Porting LLVM. That is, instead of porting LLVM through the usual route of via-C and then fixing up the NCG, just try to do it all through the LLVM back-end. As LLVM is quite portable and supported on more platforms then GHC, it would be an interesting and valuable experiment to try to port GHC to a new platform by simply getting the LLVM back-end working on it. (The LLVM back-end works in both unregistered and registered mode, another advantage for porting compared to the C and NCG back-ends).


It would also be interesting to looking into improving GHC to support cross compiling and doing this through the LLVM back-end as it should be easier to fix up to support this feature than the C or NCG back-ends.
