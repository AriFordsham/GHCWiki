# Status of DPH Benchmarks

**Last updated**: 2nd December 2010.


This page gives an overview of how well the benchmarks in the [ dph-examples/](http://darcs.haskell.org/packages/dph/dph-examples) directory of package dph are currently working.


The benchmarks are run each night by [ DPH BuildBot](http://darcs.haskell.org/packages/dph/dph-buildbot). The results are posted to cvs-ghc and uploaded to [ http://log.ouroborus.net/limitingfactor/dph/](http://log.ouroborus.net/limitingfactor/dph/). Check there for the latest numbers.

# Summary

- Evens gets slower as number of threads increases, probably because it's using a filtering operation.
- QuickHull is 4x slower than the immutable Data.Vector version in absolute terms. This may be related to the problem with Evens.
- Vectorised sequential QuickSort doesn't compile due to a blow-up in SpecConstr.
- Vectorised NBody has a core-lint error due to a bug in the rule matcher. If you turn off -dcore-lint it segfaults when run.


ToDo: Benchmarks are currently being run with -fasm, and not via the LLVM backend. This will affect comparisons with C, but not with Data.Vector as it uses the same backend.

---

# Flat Parallelism


Todo: add Repa benchmarks.

# Statically Nested Parallelism


Statically nested parallelism is where the parallelism has a fixed, finite depth. For example ``mapP f (filterP g xs)``. Statically nested programs are easier to vectorize than dynamically nested programs. At present, single threaded statically nested programs should run as fast as equivalent Data.Vector programs. Parallel versions should display a good speedup.

<table><tr><th>[ SumSquares](http://darcs.haskell.org/packages/dph/dph-examples/imaginary/SumSquares/)</th>
<td>
Computes the sum of the squares from 1 to N using `Int`.  N = 100M.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.sumsq.vector.seq.N4 </th>
> <th>  404ms </th>
> <th> 1 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.seq.N4 </th>
> <th> 434ms </th>
> <th> 0.93 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.par.N1 </th>
> <th> 443ms </th>
> <th> 0.91 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.par.N2 </th>
> <th> 222ms </th>
> <th> 1.82 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.par.N4 </th>
> <th> 111ms </th>
> <th> 3.63 </th>
> <th></th></tr></table>

> **Status**: fine
> **Todo**: Add a sequential C version.

<table><tr><th>[ DotProduct](http://darcs.haskell.org/packages/dph/dph-examples/imaginary/DotProduct)</th>
<td>
Computes the dot product of two vectors of `Double`s. N=10M.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.dotp.vector.seq.N4 </th>
> <th>  68ms </th>
> <th> 1 </th>
> <th></th></tr>
> <tr><th> dph.dotp.vectorised.seq.N4 </th>
> <th> 58ms </th>
> <th> 1.17 </th>
> <th> A 
> </th></tr>
> <tr><th> dph.dotp.vectorised.par.N1 </th>
> <th> 55ms </th>
> <th> 1.24 </th>
> <th></th></tr>
> <tr><th> dph.dotp.vectorised.par.N2 </th>
> <th> 33ms </th>
> <th> 2.06 </th>
> <th></th></tr>
> <tr><th> dph.dotp.vectorised.par.N4 </th>
> <th> 25ms </th>
> <th> 2.72 </th>
> <th></th></tr></table>

>
> A: The sequential vectorised version is faster than with Data.Vector. Why was this?

> **Status**: fine
> **Todo**: Add a sequential C version.

<table><tr><th>[ Evens](http://darcs.haskell.org/libraries/dph/dph-examples/imaginary/Evens/)**(SLOWDOWN)**</th>
<td>
Takes the even valued `Int`s from a vector. N=10M.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.evens.vectorised.seq.N4 </th>
> <th> 1.075s </th>
> <th> 1 </th>
> <th></th></tr>
> <tr><th> dph.evens.vectorised.par.N1 </th>
> <th> 736ms </th>
> <th>  1.46 </th>
> <th></th></tr>
> <tr><th> dph.evens.vectorised.par.N2 </th>
> <th> 768ms </th>
> <th>  1.40 </th>
> <th></th></tr>
> <tr><th> dph.evens.vectorised.par.N4 </th>
> <th> 859ms </th>
> <th>  1.25 </th>
> <th></th></tr></table>

> **Status**: Benchmark runs slower when number of threads increases. This benchmark invokes `packByTag` due to the filtering operation. This is probably affecting Quickhull as it also uses filtering. 
> **Todo**: Fix slowdown. Add a sequential C version. 

<table><tr><th>[ SMVM](http://darcs.haskell.org/packages/dph/examples/smvm/)</th>
<td>
Multiplies a dense vector with a sparse matrix represented in the *compressed sparse row format (CSR).*</td></tr></table>

> > **Todo**: Add this to the nightly run.

# Dynamically Nested Parallelism


Dynamically nested programs have a recursive structure where each level of the recursion invokes more parallel computations. This is common for benchmarks that use divide-and-conquer style algorithms.

<table><tr><th>[ Primes](http://darcs.haskell.org/packages/dph/examples/primes/)</th>
<td>
The Sieve of Eratosthenes using parallel writes into a sieve structure represented as an array of `Bool`s.  
</td></tr></table>

> **Todo**: We currently don't have a proper parallel implementation of this benchmark, as we are missing a parallel version of default backpermute.  The problem is that we need to make the representation of parallel arrays of `Bool` dependent on whether the hardware supports atomic writes of bytes. Investigate whether any of the architectures relevant for DPH actually do have trouble with atomic writes of bytes (aka `Word8`).

<table><tr><th>[ QuickSort](http://darcs.haskell.org/libraries/dph/dph-examples/spectral/QuickSort/)**(BROKEN)**</th>
<td>
Sort a vector of doubles by recursively splitting the vector and sorting the two halves. This is a "fake" benchmark because we divide right down to two-point vectors and construct the result using copying append. A production algorithm would switch to an in-place sort once the size of the vector reaches a few thousand elements.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.quicksort.vectorised.par.N1 </th>
> <th> 428ms </th>
> <th>  1 </th>
> <th></th></tr>
> <tr><th> dph.quicksort.vectorised.par.N2 </th>
> <th> 400ms </th>
> <th>  1.07 </th>
> <th></th></tr>
> <tr><th> dph.quicksort.vectorised.par.N4 </th>
> <th> 392ms </th>
> <th>  1.09 </th>
> <th></th></tr></table>

> **Status**: Sequential vectorised version does not compile due to a blowup in SpecConstr.

<table><tr><th>[ Quickhull](http://darcs.haskell.org/libraries/dph/dph-examples/spectral/QuickHull/)**(SLOWLORIS)**</th>
<td>
Given a set of points in the plane, compute the sequence of points that encloses all points in the set. This benchmark is interesting as it is the simplest code that exploits the ability to implement divide-and-conquer algorithms with nested data parallelism.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.quickhull.vector-immutable.seq.N4 </th>
> <th> 0.166s </th>
> <th> 1 </th>
> <th></th></tr>
> <tr><th> dph.quickhull.vectorised.seq.N4 </th>
> <th> 0.677s </th>
> <th>  0.24 </th>
> <th> 4x slower 
> </th></tr>
> <tr><th> dph.quickhull.vectorised.par.N1 </th>
> <th> 1.059s </th>
> <th>  0.15 </th>
> <th> 6x slower
> </th></tr>
> <tr><th> dph.quickhull.vectorised.par.N2 </th>
> <th> 0.809s </th>
> <th>  0.21 </th>
> <th></th></tr>
> <tr><th> dph.quickhull.vectorised.par.N4 </th>
> <th> 0.686s </th>
> <th>  0.24 </th>
> <th></th></tr>
> <tr><th> dph.quickhull.vector-mutable.seq.N4 </th>
> <th> 0.086s </th>
> <th>  1.93 </th>
> <th></th></tr>
> <tr><th> dph.quickhull.vector-forkIO.par.N4 </th>
> <th> 0.064s </th>
> <th>  2.59 </th>
> <th></th></tr>
> <tr><th> dph.quickhull.c.seq </th>
> <th> 0.044s </th>
> <th> 3.77 </th>
> <th></th></tr></table>

> **Status**: Benchmark scales but is 4x slower than version using immutable Data.Vectors. QuickHull is based around filtering operations, so the fact that Evens is also slow is probably related.

# Dynamically Nested Parallelism with Algebraic Data Types


These programs also use user defined algebraic data types. Vectorization of these programs is still a work in progress.

<table><tr><th>[ BarnesHut](http://darcs.haskell.org/packages/dph/dph-examples/barnesHut/)</th>
<td>
This benchmark implements the Barnes-Hut algorithm to solve the *n*-body problem in two dimensions.  **Currently won't compile with vectorisation due to excessive inlining of dictionaries.**</td></tr></table>

---

---

# Key


\<project\>.\<benchmark\>.\<version\>.\<parallelism\>.\<threads\>


Project

- Either *dph* or *repa*. Repa programs use the same parallel array library as DPH, but do not go through the vectorising transform.


Version

- *vectorised* means it's been through the DPH vectorising transform. 
- *vector* is a hand written version using immutable Data.Vectors
- *vector-mutable* is a hand written version using mutable Data.Vectors.
- *vector-immutable* means the same as *vector* and is used when there is also an mutable version.


Parallelism 

- Whether a benchmark is natively parallel or sequential. 
- Parallel versions are also run single threaded (with -N1) and sequential versions are also run with (-N4) so we get the parallel GC.
- Parallel versions with -N1 will tend to be slower than natively sequential versions due to overheads for supporting parallelism.


Status

- **BROKEN**: Benchmark doesn't compile, or crashes when run.
- **SLOWDOWN**: Benchmark gets slower as number of threads increases. 
- **SLOWLORIS**: Benchmark scales as the number of threads increases, but the absolute performance is not acceptable compared with equivalent versions using immutable Data.Vectors.

# Benchmark machine

- 2x 3.0GHz Quad-Core Intel Xeon 5400
- 12MB (2x6MB) on-die L2 cache per processor
- independent 1.6GHz frontside bus per processor
- 800MHz DDR2 FB-DIMM
- 256-bit-wide memory architecture
- Mac OS X Server 10.5.6
