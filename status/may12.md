# GHC Status Report May 2012


GHC 7.4.1 was released at the beginning of February, and has been by and large a successful release.  Nevertheless the tickets keep pouring in, and a large collection of bug fixes\[1\] have been made since the 7.4.1 release.  We plan to put out a 7.4.2 release candidate very soon (it may be out by the time you read this), followed shortly by the release.


We have a new member of the team! Please welcome Paolo Capriotti who is assuming some of the GHC maintenance duties for Well-Typed.


7.4.1 included a few major improvements.  For more details on these, see the previous status report\[2\]

- Support for all declarations at the GHCi prompt 
- Data type promotion and kind polymorphism \[3\]
- Improvements to Safe Haskell (safety is now inferred)
- Constraint Kinds\[4\]
- Profiling improvements: a major internal overhaul, and support for stack traces with `+RTS -xc`.
- Preliminary support for registerised ARM compilation


Here are the projects we're currently working on:

- **Completing the support for kind polymorphism** (Simon PJ)

- **Typechecker performance improvements** (Dimitrios?)

- **Type-level natural numbers** (Iavor D)

- **Windows x64 Support** (Ian L)

- **The new code generator** (Simon M).  The glorious new code generator \[6\] has been an ongoing project for some time now.  The basic idea is to replace the pass of the compiler that converts from STG to Cmm (our internal C-- representation) with a more flexible framework consisting of two main passes: one that generates C-- without explicit stack manipulation, and a second pass that makes the stack explicit.  This will enable a host of improvements and optimisations in due course.  The new code generator uses the Hoopl framework for code analysis and rewriting \[7\].  Earlier this year I (Simon M) took over this project, and spent a lot of time optimising the existing framework and Hoopl itself.  I also rewrote the stack allocator, and made a number of simplifications.  The current state is that the new code generator produces code that is almost as good as the old one (and occasionally better), and is somewhat slower (roughly 15% slower compilation with -O).  The goal is to further improve on this, and I'm confident that we can generate better code in most cases than the old code generator.  I hope this can make it into 7.6.1, but no guarantees.

- **Changing the +RTS -N setting at runtime**.  Up until recently, the number of cores ("Capabilities" in GHC terminology) that GHC uses was fixed by the `+RTS -N` flag when you start the program.  For instance, to use 2 cores, we pass the flag `+RTS -N2` to the Haskell program.  GHC now has support for modifying this setting programmatically at runtime, both up and down, via the API `Control.Concurrent.setNumCapabilities`.  So a parallel Haskell program can now set the number of cores to run on itself, without the user needing to pass `+RTS -N`.  Another use for this feature is to drop back to using a single core during sequential sections of the program, which is likely to give better performance, especially on a loaded system.  A threadscope diagram showing this in action is here: \[5\].  In the future we hope to use heuristics to dynamically adjust the number of cores in use according to system load or application demand, for example.

- **Profiling and stack traces**  (Simon M). 7.4.1 has an overhauled profiling system, and in many cases gives better results than earlier versions.  However, some details remain to be resolved around the precise semantics of cost-centre stacks.  Also, I hope that it might be possible to provide stack traces of a kind without having to compile for profiling, perhaps in GHCi only.

- **Support for SSE primitives when using the LLVM back end** (Geoffrey M). The `simd` git branch of GHC adds support for primitive 128-bit SIMD vector types and associated primops when using the LLVM back end, meaning this branch can now generate SSE instructions on x86 platforms. We hope this support will make it into 7.6.1. Experimental versions of the vector library \[8\] and DPH \[9\] provide higher-level interfaces to the new primitives. Initial benchmarks indicate that numerical code can benefit substantially.

- **Data Parallel Haskell**. The vectorisation transformation underlying our implementation of nested data parallelism in GHC had a fundamental and long standing asymptotic complexity problem that we were finally able to resolve. Details are in a recent draft paper entitled *Work Efficient Higher-Order Vectorisation*\[11\]. The implementation described in the paper is available in the DPH packages from Hackage (which need to be used with GHC 7.4.1). The new implementation of the DPH libraries still needs to be optimised; hence, our next step will be to optimise constant factors.

>
> In addition, we released Repa 3 \[12\], which uses type-indices to control array representations. This leads to more predictable performance. You can install Repa 3, which requires GHC 7.4.1, from Hackage. We are currently writing a paper describing the new design in detail.

>
> Finally, we are about to release (it may be out by the time you read this) a stable, end-user ready version of the Repa-like array library Accelerate for GPU computing on Hackage. It integrates with Repa, so you can mix GPU and CPU multicore computing, and via the new `meta-par` package you can share workload between CPUs and GPUs \[13\]. This new version 0.12 is already available on GitHub \[14\]. You need a CUDA-capable NVIDIA GPU to use it.

\[1\][ http://hackage.haskell.org/trac/ghc/query?status=closed&order=priority&col=id&col=summary&col=status&col=owner&col=type&col=priority&col=component&milestone=7.4.2&resolution=fixed](http://hackage.haskell.org/trac/ghc/query?status=closed&order=priority&col=id&col=summary&col=status&col=owner&col=type&col=priority&col=component&milestone=7.4.2&resolution=fixed)
\[2\][ http://hackage.haskell.org/trac/ghc/wiki/Status/Oct11](http://hackage.haskell.org/trac/ghc/wiki/Status/Oct11)
\[3\][http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html\#kind-polymorphism](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html#kind-polymorphism)
\[4\][http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/constraint-kind.html](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/constraint-kind.html)
\[5\][ https://plus.google.com/107890464054636586545/posts/GsfcJfdkEYL](https://plus.google.com/107890464054636586545/posts/GsfcJfdkEYL)
\[6\][ http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen)
\[7\][ http://www.cs.tufts.edu/\~nr/pubs/dfopt-abstract.html](http://www.cs.tufts.edu/~nr/pubs/dfopt-abstract.html)
\[9\][ http://ghc-simd.blogspot.co.uk/2012/03/simd-support-for-vector-library.html](http://ghc-simd.blogspot.co.uk/2012/03/simd-support-for-vector-library.html)
\[10\][ http://ghc-simd.blogspot.co.uk/2012/04/adding-simd-support-to-data-parallel.html](http://ghc-simd.blogspot.co.uk/2012/04/adding-simd-support-to-data-parallel.html)
\[11\][ http://www.cse.unsw.edu.au/\~chak/papers/LCKLP12.html](http://www.cse.unsw.edu.au/~chak/papers/LCKLP12.html)
\[12\][ http://repa.ouroborus.net/](http://repa.ouroborus.net/)
\[13\][ http://parfunk.blogspot.com.au/2012/05/how-to-write-hybrid-cpugpu-programs.html](http://parfunk.blogspot.com.au/2012/05/how-to-write-hybrid-cpugpu-programs.html)
\[14\][ https://github.com/AccelerateHS/accelerate](https://github.com/AccelerateHS/accelerate)