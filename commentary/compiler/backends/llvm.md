
David Terei wrote a new code generator for GHC which targets the LLVM compiler infrastructure. Most of the work was done as part of an honours thesis at the University of New South Wales under the supervision of Manuel Chakravarty. Its now at a stage where it is under consideration to be merged into GHC mainline.


The patch for the llvm back-end can be found here (should apply cleanly to GHC head):

[ http://www.cse.unsw.edu.au/\~davidt/downloads/ghc-llvmbackend-full.gz](http://www.cse.unsw.edu.au/~davidt/downloads/ghc-llvmbackend-full.gz)


This is what is under consideration to be merged into GHC head.


The thesis paper which offers a detailed performance evaluation, as well as the motivation and design of the back-end can be found at:

[ http://www.cse.unsw.edu.au/\~pls/thesis/davidt-thesis.pdf](http://www.cse.unsw.edu.au/~pls/thesis/davidt-thesis.pdf)


Below I'll quickly detail out the important points though. There are also instructions on how to get started with the back-end.


Finally there are also some issues that I think may need to be sorted out before a merge could be done. They are at the end.

# Installing

[ http://www.cse.unsw.edu.au/\~davidt/downloads/ghc-llvmbackend-full.gz](http://www.cse.unsw.edu.au/~davidt/downloads/ghc-llvmbackend-full.gz)


Apply the darcs patch linked above to GHC head. This will make some changes across GHC, with the bulk of the new code ending up in 'compiler/llvmGen'.


To build GHC you need to add two flags to build.mk, they are:

```wiki
GhcWithLlvmCodeGen = YES
GhcEnableTablesNextToCode = NO
```


The llvm code generator doesn't support at this time the `TABLES_NEXT_TO_CODE` optimisation due to limitations with LLVM.


You will also need LLVM installed on your computer to use the back-end. Version 2.6 or SVN trunk is supported. If you want to use the back-end in an unregistered ghc build, then you can use a vanilla build of LLVM. However if you want to use a registered ghc build (very likely) then you need to patch LLVM for this to work. The patch for llvm can be found here:

[ http://www.cse.unsw.edu.au/\~davidt/downloads/llvm-ghc.patch](http://www.cse.unsw.edu.au/~davidt/downloads/llvm-ghc.patch)


LLVM is very easy to build and install. It can be done as follows:

```wiki
$ svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm
$ cd llvm
$ patch -p0 -i ~/llvm-ghc.patch
$ ./configure --enable-optimized # probably also want to set --prefix
$ make
$ make install
```


Just make sure this modified version of LLVM is on your path and takes precedence over any other builds.

# Using


Once GHC is built, you can trigger GHC to use the LLVM back-end with the `-fllvm` flag. There is also a new `-ddump-llvm` which will dump out the llvm IR code generated (must be used in combination with the `-fllvm` flag. (or use the `-keep-tmp-files` flag).

`ghc --info` should also now report that it includes the llvm code generator.


The [ ghc-core](http://hackage.haskell.org/package/ghc-core) tool also supports the llvm backend, and will display the generated assembly code for your platform.

# Performance


(All done on linux/x86-32)


A quick summary of the results are that for the 'nofib' benchmark suite, the llvm code generator was 3.8% slower than the NCG (the C code generator was 6.9% slower than the NCG). The DPH project includes a benchmark suite which I also ran and for this type of code using the llvm back-end shortened the runtime by an average of 25% compared to the NCG. Also, while not included in my thesis paper as I ran out of time, I did do some benchmarking with the 'nobench' benchmark suite. It gave performance ratios for the back-ends of around:

<table><tr><th>NCG </th>
<th> 1.11
</th></tr>
<tr><th>C </th>
<th> 1.05
</th></tr>
<tr><th>LLVM </th>
<th> 1.14
</th></tr></table>

# Supported Platforms & Correctness


Linux x86-32/x86-64 are currently well supported. The back-end can pass the test suite and build a working version of GHC (bootstrap test).


Mac OS X 10.5 currently has a rather nasty bug with any dynamic lib calls (all libffi stuff) \[due to the stack not being 16byte aligned when the calls are made as required by OSX ABI for the curious\]. Test suite passes except for most the ffi tests.


Other platforms haven't been tested at all. As using the back-end with a registered build of GHC requires a modified version of LLVM, people wanting to try it out on those platforms will need to either make the needed changes to LLVM themselves, or use an unregistered build of GHC which will work with a vanilla install of LLVM. (A patch for LLVM for x86 is linked to below.)

## Validate


I've validated my GHC patch to make sure it won't break anything. This is just compiling and running GHC normally but with the llvm back-end code included. It doesn't actually test the llvm code generator, just makes sure it hasn't broken the NCG or C code generator.

**Linux/x86-32:**

```wiki
OVERALL SUMMARY for test run started at Do 18. Feb 11:21:48 EST 2010
2457 total tests, which gave rise to
9738 test cases, of which
0 caused framework failures
7573 were skipped

2088 expected passes
76 expected failures
0 unexpected passes
1 unexpected failures

Unexpected failures:
user001(normal)
```

**Linux/x86-64:**

```wiki
OVERALL SUMMARY for test run started at Thu 18 Feb 15:28:32 EST 2010
2458 total tests, which gave rise to
9739 test cases, of which
0 caused framework failures
7574 were skipped

2087 expected passes
77 expected failures
0 unexpected passes
1 unexpected failures

Unexpected failures:
T1969(normal)
```

**Mac OS X 10.5/x86-32:**

```wiki
OVERALL SUMMARY for test run started at Thu Feb 18 12:35:49 EST 2010
2458 total tests, which gave rise to
9122 test cases, of which
0 caused framework failures
6959 were skipped

2085 expected passes
76 expected failures
0 unexpected passes
2 unexpected failures

Unexpected failures:
T1969(normal)
ffi005(optc)
```


All of the test failures fail for me with a unmodified GHC head build as well as when the llvm patch is included, so the llvm patch isn't introducing any new failures.

# Issues


Issues that might need to be resolved before merging the patch:

1. Developed in isolation by 1 person with no Haskell knowledge at first. So usual issues with that may apply, misused data structures, bad style... ect. Criticisms of the code are very welcome. There are some specific notes on what I think may be wrong with the code atm in 'compiler/llvmGen/NOTES'.

1. The back-end has a LLVM binding of sorts, this binding is similar in design to say the Cmm representation used in GHC. It represents the LLVM Assembly language using a collection of data types and can pretty print it out correctly. This binding lives in the 'compiler/llvmGen/Llvm' folder. Should this binding be split out into a separate library?

1. As mentioned above, LLVM needs to be patched to work with a registered build of GHC. If the llvm back-end was merged, how would this be handled? I would suggest simply carrying the patch with some instructions on how to use it in the GHC repo. People using GHC head could be expected to grab the LLVM source code and apply the patch themselves at this stage.
