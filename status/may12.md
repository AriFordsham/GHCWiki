# GHC Status Report May 2012


GHC 7.4.1 was released at the beginning of February, and has been by and large a successful release.  Nevertheless the tickets keep pouring in, and a large collection of bug fixes\[1\] have been made since the 7.4.1 release.  We plan to put out a 7.4.2 release candidate very soon (it may be out by the time you read this), followed shortly by the release.


We have a new member of the team! Please welcome Paolo Capriotti who is assuming some of the GHC maintenance duties for Well-Typed.


7.4.1 included a few major improvements.  For more details on these, see the previous status report\[2\]

- Support for all declarations at the GHCi prompt 
- Data type promotion and kind polymorphism \[3\]
- Improvements to Safe Haskell (safety is now inferred)
- Constraint Kinds\[4\]
- Profiling improvements: a major internal overhaul, and support for stack traces with `+RTS -xc`.
- Preliminary support for registerised ARM compilation (with full GHCi support being introduced in 7.4.2)


Here are the projects we're currently working on:

- **Kind polymorphism**.  Simon PJ has been working hard on completing the implementation of kind polymorphism and data type promotion \[3\].  This will appear for the first (supported) time in GHC 7.6; please do stress-test the HEAD.

- **Deferred type errors**.  Etienne Laurin suggested \[16\] that GHC could compile and run a program even though it contains type errors.  After all, the bit you want to run might not contain the error, and it's sometimes annoying to have to fix *every* type error before you can run *any* code.  It turned out that there was a beautifully simple way to fit this idea into GHC's new constraint-based type inference engine, and we have now done so.  It's all explained in "Equality proofs and deferred type errors" \[17\], and will be in GHC 7.6.  

- **Holes in terms**.  Thijs Alkemade and Sean Leather have been working on another variant of deferred error messages, that would allow you to write a program that contains as-yet-unwritten sub-terms, or "holes" and have GHC report a fairly precise type for the hole.  The idea is inspired by Agda's interactive programming environment, which has a facility of this kind.  The more complicated the types get, the more useful this is!  Details on their wiki page \[18\].

- **Type level literals**.  Iavor Diatchki has added type-level natural numbers (kind `Nat`) and strings (kind `Symbol`) to GHC.  You can find lots of details on the wiki page \[20\].  At the moment there is no useful *computation* over the type-level naturals, but he is extending GHC's constraint solver with support for reasoning about type-expressions involving addition, multiplication, and exponentiation.   This work is happening on branch 'type-nats' in the repo, and we expect to have something working fairly soon.

- **Typechecker performance improvements**.  Most of the smarts of type inference are now located in the type constraint solver, described in our paper "Modular type inference with local assumptions: OutsideIn(X)" \[19\].  It works just fine for redgular old ML-style programs, but was a bit slow for programs that make heavy use of type-level computation.  Dimitrios has been working hard to improve its performance; we have carried out at least three major refactorings, deleted tons of code, and made it faster and more beautiful.

- **Windows x64 Support** (Ian L).  The Industrial Haskell Group has funded work to implement 64bit Windows support in GHC. The port is now self-hosting and mostly complete, with just a number of bugs in the periphery to fix, and some logistics to work out. We expect a 64bit Windows installer to be included in the GHC 7.6 releases.

- **The new code generator** (Simon M).  The glorious new code generator \[6\] has been an ongoing project for some time now.  The basic idea is to replace the pass of the compiler that converts from STG to Cmm (our internal C-- representation) with a more flexible framework consisting of two main passes: one that generates C-- without explicit stack manipulation, and a second pass that makes the stack explicit.  This will enable a host of improvements and optimisations in due course.  The new code generator uses the Hoopl framework for code analysis and rewriting \[7\].  Earlier this year I (Simon M) took over this project, and spent a lot of time optimising the existing framework and Hoopl itself.  I also rewrote the stack allocator, and made a number of simplifications.  The current state is that the new code generator produces code that is almost as good as the old one (and occasionally better), and is somewhat slower (roughly 15% slower compilation with -O).  The goal is to further improve on this, and I'm confident that we can generate better code in most cases than the old code generator.  I hope this can make it into 7.6.1, but no guarantees.

- **Changing the +RTS -N setting at runtime**.  Up until recently, the number of cores ("Capabilities" in GHC terminology) that GHC uses was fixed by the `+RTS -N` flag when you start the program.  For instance, to use 2 cores, we pass the flag `+RTS -N2` to the Haskell program.  GHC now has support for modifying this setting programmatically at runtime, both up and down, via the API `Control.Concurrent.setNumCapabilities`.  So a parallel Haskell program can now set the number of cores to run on itself, without the user needing to pass `+RTS -N`.  Another use for this feature is to drop back to using a single core during sequential sections of the program, which is likely to give better performance, especially on a loaded system.  A threadscope diagram showing this in action is here: \[5\].  In the future we hope to use heuristics to dynamically adjust the number of cores in use according to system load or application demand, for example.

- **Profiling and stack traces**  (Simon M). 7.4.1 has an overhauled profiling system, and in many cases gives better results than earlier versions.  However, some details remain to be resolved around the precise semantics of cost-centre stacks.  Also, I hope that it might be possible to provide stack traces of a kind without having to compile for profiling, perhaps in GHCi only.

- **Support for SSE primitives when using the LLVM back end** (Geoffrey M). The `simd` git branch of GHC adds support for primitive 128-bit SIMD vector types and associated primops when using the LLVM back end, meaning this branch can now generate SSE instructions on x86 platforms. We hope this support will make it into 7.6.1. Experimental versions of the vector library \[8\] and DPH \[9\] provide higher-level interfaces to the new primitives. Initial benchmarks indicate that numerical code can benefit substantially.

- **Data Parallel Haskell**. The vectorisation transformation underlying our implementation of nested data parallelism in GHC had a fundamental and long standing asymptotic complexity problem that we were finally able to resolve. Details are in a recent draft paper entitled *Work Efficient Higher-Order Vectorisation* \[11\]. The implementation described in the paper is available in the DPH packages from Hackage (which need to be used with GHC 7.4.1). The new implementation of the DPH libraries still needs to be optimised; hence, our next step will be to optimise constant factors.

>
>
> In addition, we released Repa 3 \[12\], which uses type-indices to control array representations. This leads to more predictable performance. You can install Repa 3, which requires GHC 7.4.1, from Hackage. We are currently writing a paper describing the new design in detail.
>
>

>
>
> Finally, we are about to release (it may be out by the time you read this) a stable, end-user ready version of the Repa-like array library Accelerate for GPU computing on Hackage. It integrates with Repa, so you can mix GPU and CPU multicore computing, and via the new `meta-par` package you can share workload between CPUs and GPUs \[13\]. This new version 0.12 is already available on GitHub \[14\]. You need a CUDA-capable NVIDIA GPU to use it.
>
>

- **Lightweight concurrency substrate** (Sivaramakrishnan Krishnamoorthy Chandrasekaran, aka "KC").  During his internship at MSR Cambridge, KC has been working on replacing the RTS scheduler with some APIs that enable the scheduler to be implemented in Haskell.  The aim is to not just move the scheduler into Haskell, but also enable user-defined schedulers to coexist, which will ultimately enable much greater control over scheduling behaviour.  This follows on from previous work \[15\] with Peng Li and Andrew Tolmach, but this time we are taking a slightly different approach that has a couple of important benefits.

>
>
> Firstly, KC found a way to enable concurrency abstractions to be defined without depending on a particular scheduler.  This means for example that we can provide `MVar`s that work with any user-defined scheduler, rather than needing one `MVar` implementation per scheduler.  Secondly, we found ways to coexist with some of the existing RTS machinery for handling blackholes and asynchronous exceptions in particular, which means that these facilities will continue to work as before (with the same performance), and writers of user-defined schedulers do not need to worry about them.  Furthermore this significantly lowers the barrier for writing a new scheduler.
>
>

>
>
> This is all still very much experimental, and it is not clear whether it will ever be in GHC proper.  It depends on whether we can achieve good enough performance, amongst other things.  All we can say for now is that the approach is promising.  You can find KC's work on the `ghc-lwc` branch of the git repo.
>
>


 


- **Full support for GHCi on ARM** (Ben Gamari).  Thanks to Ben, we now have support for ARM in the GHCi linker \[21\].  This will be shipped in 7.4.2 (it wasn't in 7.4.1).


\[1\] [http://hackage.haskell.org/trac/ghc/query?status=closed&order=priority&col=id&col=summary&col=status&col=owner&col=type&col=priority&col=component&milestone=7.4.2&resolution=fixed](http://hackage.haskell.org/trac/ghc/query?status=closed&order=priority&col=id&col=summary&col=status&col=owner&col=type&col=priority&col=component&milestone=7.4.2&resolution=fixed) 

\[2\] [http://hackage.haskell.org/trac/ghc/wiki/Status/Oct11](http://hackage.haskell.org/trac/ghc/wiki/Status/Oct11)

\[3\] [http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html\#kind-polymorphism](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html#kind-polymorphism) 

\[4\] [http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/constraint-kind.html](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/constraint-kind.html) 

\[5\] [https://plus.google.com/107890464054636586545/posts/GsfcJfdkEYL](https://plus.google.com/107890464054636586545/posts/GsfcJfdkEYL) 

\[6\] [http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen) 

\[7\] [http://www.cs.tufts.edu/\~nr/pubs/dfopt-abstract.html](http://www.cs.tufts.edu/~nr/pubs/dfopt-abstract.html) 

\[9\] [http://ghc-simd.blogspot.co.uk/2012/03/simd-support-for-vector-library.html](http://ghc-simd.blogspot.co.uk/2012/03/simd-support-for-vector-library.html) 

\[10\] [http://ghc-simd.blogspot.co.uk/2012/04/adding-simd-support-to-data-parallel.html](http://ghc-simd.blogspot.co.uk/2012/04/adding-simd-support-to-data-parallel.html) 

\[11\] [http://www.cse.unsw.edu.au/\~chak/papers/LCKLP12.html](http://www.cse.unsw.edu.au/~chak/papers/LCKLP12.html) 

\[12\] [http://repa.ouroborus.net/](http://repa.ouroborus.net/) 

\[13\] [http://parfunk.blogspot.com.au/2012/05/how-to-write-hybrid-cpugpu-programs.html](http://parfunk.blogspot.com.au/2012/05/how-to-write-hybrid-cpugpu-programs.html) 

\[14\] [https://github.com/AccelerateHS/accelerate](https://github.com/AccelerateHS/accelerate) 

\[15\] [http://community.haskell.org/\~simonmar/papers/conc-substrate.pdf](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf) 

\[16\] Deferring type errors to runtime [http://hackage.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime](http://hackage.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime) 

\[17\] Equality proofs and deferred type errors [http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 

\[18\] Holes in GHC: [http://hackage.haskell.org/trac/ghc/wiki/Holes](http://hackage.haskell.org/trac/ghc/wiki/Holes) 

\[19\] Modular type inference with local assumptions: OutsideIn(X) [http://www.haskell.org/haskellwiki/Simonpj/Talk:OutsideIn](http://www.haskell.org/haskellwiki/Simonpj/Talk:OutsideIn) 

\[20\] Type level literals.  [http://hackage.haskell.org/trac/ghc/wiki/TypeNats/Basics](http://hackage.haskell.org/trac/ghc/wiki/TypeNats/Basics) 

\[21\] ARM linker support. [http://hackage.haskell.org/trac/ghc/changeset/27302c9094909e04eb73f200d52d5e9370c34a8a](http://hackage.haskell.org/trac/ghc/changeset/27302c9094909e04eb73f200d52d5e9370c34a8a)


