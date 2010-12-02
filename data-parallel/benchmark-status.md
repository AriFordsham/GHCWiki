## Status of DPH Benchmarks


This page gives an overview of how well the benchmarks in the [ dph-examples/](http://darcs.haskell.org/packages/dph/dph-examples) directory of package dph are currently working.


The benchmarks are run each night by [ DPH BuildBot](http://darcs.haskell.org/packages/dph/dph-buildbot). The results are posted to cvs-ghc and uploaded to [ http://log.ouroborus.net/limitingfactor/dph/](http://log.ouroborus.net/limitingfactor/dph/). Check there for the latest numbers.

## Key


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

## Statically Nested


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

> **Summary**: fine
> **Todo**: Add the sequential C version.

<table><tr><th>[ DotProduct](http://darcs.haskell.org/packages/dph/dph-examples/imaginary/DotProduct)</th>
<td>
Computes the dot product of two vectors of `Double`s. N=10M.
</td></tr></table>

> <table><tr><th>**name**</th>
> <th>**runtime**</th>
> <th>**speedup**</th>
> <th>**notes**</th></tr>
> <tr><th> dph.sumsq.vector.seq.N4 </th>
> <th>  68ms </th>
> <th> 1 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.seq.N4 </th>
> <th> 58ms </th>
> <th> 1.17 </th>
> <th> A 
> </th></tr>
> <tr><th> dph.sumsq.vectorised.par.N1 </th>
> <th> 55ms </th>
> <th> 1.24 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.par.N2 </th>
> <th> 33ms </th>
> <th> 2.06 </th>
> <th></th></tr>
> <tr><th> dph.sumsq.vectorised.par.N4 </th>
> <th> 25ms </th>
> <th> 2.72 </th>
> <th></th></tr></table>

>
> A: The vectorised version is faster than with Data.Vector. Why was this?

> **Summary**: fine.
> **Todo**: Add the sequential C version.

<table><tr><th>[ SMVM](http://darcs.haskell.org/packages/dph/examples/smvm/)</th>
<td>
Multiplies a dense vector with a sparse matrix represented in the *compressed sparse row format (CSR).*</td></tr></table>

> **Todo**: Add this to the nightly run.

### Dynamically Nested


Dynamically nested programs have a recursive structure where each level of the recursion invokes more parallel computations. This is common for benchmarks that use divide-and-conquer style algorithms.

<table><tr><th>[ Primes](http://darcs.haskell.org/packages/dph/examples/primes/)</th>
<td>
The Sieve of Eratosthenes using parallel writes into a sieve structure represented as an array of `Bool`s.  We currently don't have a proper parallel implementation of this benchmark, as we are missing a parallel version of default backpermute.  The problem is that we need to make the representation of parallel arrays of `Bool` dependent on whether the hardware supports atomic writes of bytes.  **Investigate whether any of the architectures relevant for DPH actually do have trouble with atomic writes of bytes (aka `Word8`).**</td></tr></table>

<table><tr><th>[ Quickhull](http://darcs.haskell.org/packages/dph/examples/quickhull/)</th>
<td>
Given a set of points (in a plane), compute the sequence of points that encloses all points in the set. This benchmark is interesting as it is the simplest code that exploits the ability to implement divide-and-conquer algorithms with nested data parallelism.  We have only a "vectorised" version of this benchmark and a sequential Haskell reference implementation, "ref Haskell", using vanilla lists.
</td></tr></table>

<table><tr><th>[ Quicksort](http://darcs.haskell.org/packages/dph/examples/qsort/)</th>
<td>FIXME</td></tr></table>

### Dynamically Nested with Algebraic Data Types


These programs also use user defined algebraic data types. Vectorization of these programs is still a work in progress.

<table><tr><th>[ BarnesHut](http://darcs.haskell.org/packages/dph/dph-examples/barnesHut/)</th>
<td>
This benchmark implements the Barnes-Hut algorithm to solve the *n*-body problem in two dimensions.  **Currently won't compile with vectorisation due to excessive inlining of dictionaries.**</td></tr></table>

### Execution on LimitingFactor (2x Quad-Core Xeon)


Hardware spec: 2x 3.0GHz Quad-Core Intel Xeon 5400; 12MB (2x6MB) on-die L2 cache per processor; independent 1.6GHz frontside bus per processor; 800MHz DDR2 FB-DIMM; 256-bit-wide memory architecture; Mac OS X Server 10.5.6
