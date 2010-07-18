# Installing & Using the LLVM Back-end

## Installing


The LLVM backend is now included in GHC HEAD. Just grab the darcs HEAD version of GHC and build it. The backend now also supports all modes that GHC can be built in, except perhaps -dynamic which hasn't been tested yet, so you shouldn't need to change your build.mk file either (you used to have disabled an optimisation called tables-next-to-code, but the LLVM backend supports that now).


For instructions on building GHC go [ here](http://hackage.haskell.org/trac/ghc/wiki/Building)

## LLVM Support


The LLVM backend only supports LLVM version **2.7** or later. Simply install it and make sure the various llvm tools (opt, llc) are available on your path.

## Using


Once built you can check that you have the LLVM backend GHC will support these extra options:

- *--info* - Now will report that it has the llvm backend
- *-fllvm* - Compile code using the llvm backend
- *-ddump-llvm* - Dumps the llvm IR while compiling
- *-pgmla* - The program to use as the llvm assembler
- *-pgmlo* - The program to use as the llvm optimiser
- *-pgmlc* - The program to use as the llvm compiler
- *-optla* - Extra options to pass to the llvm assembler
- *-optlo* - Extra options to pass to the llvm optimiser
- *-optlc* - Extra options to pass to the llvm compiler

## Supported Platforms & Correctness

- Linux x86-32/x86-64: Currently well supported. The back-end can pass the test suite and build a working version of GHC (bootstrap test).
- Windows x86-32: Currently well supported. The back-end can pass the test suite and build a working version of GHC (bootstrap test).
- Mac OS X 10.5/10.6: Can pass the test suite, so most things should work but there are some programs known to segfault if compiled with the LLVM backend. The cause for this is still being investigated.
- Other platforms haven't been tested at all.

## Performance


(All done on linux/x86-32)


A quick summary of the results are that for the 'nofib' benchmark suite, the LLVM code generator was 3.8% slower than the NCG (the C code generator was 6.9% slower than the NCG). The DPH project includes a benchmark suite which I (David Terei) also ran and for this type of code using the LLVM back-end shortened the runtime by an average of 25% compared to the NCG. Also, while not included in my thesis paper as I ran out of time, I did do some benchmarking with the 'nobench' benchmark suite. It gave performance ratios for the back-ends of around:

<table><tr><th>NCG </th>
<th> 1.11
</th></tr>
<tr><th>C </th>
<th> 1.05
</th></tr>
<tr><th>LLVM </th>
<th> 1.14
</th></tr></table>


A nice demonstration of the improvements the LLVM back-end can bring to some code though can be see at [ http://donsbot.wordpress.com/2010/02/21/smoking-fast-haskell-code-using-ghcs-new-llvm-codegen/](http://donsbot.wordpress.com/2010/02/21/smoking-fast-haskell-code-using-ghcs-new-llvm-codegen/)