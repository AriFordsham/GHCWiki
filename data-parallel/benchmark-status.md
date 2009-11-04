## Status of DPH Benchmarks


This page gives an overview of how well the benchmarks in the [ examples/](http://darcs.haskell.org/packages/dph/examples/) directory of package dph are currently working.

### Overview over the benchmark programs

<table><tr><th>[ SumSq](http://darcs.haskell.org/packages/dph/examples/sumsq/)</th>
<td>
Computes the sum of the squares from 1 to N using `Int`.  There are two variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.  As a reference implementation, we have a sequential C program denoted by "ref C".
</td></tr>
<tr><th>[ DotP](http://darcs.haskell.org/packages/dph/examples/dotp/)</th>
<td>
Computes the dot product of two vectors of `Double`s.  There are two variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.  In addition to these two DPH variants of the dot product, we also have two non-DPH reference implementations: (a) "ref Haskell" is a Haskell program using imperative, unboxed arrays and and (b) "ref C" is a C implementation using pthreads.
</td></tr>
<tr><th>[ SMVM](http://darcs.haskell.org/packages/dph/examples/smvm/)</th>
<td>
Multiplies a dense vector with a sparse matrix represented in the *compressed sparse row format (CSR).*  There are three variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.  As a reference implementation, we have a sequential C program denoted by "ref C".
</td></tr>
<tr><th>[ Quickhull](http://darcs.haskell.org/packages/dph/examples/quickhull/)</th>
<td>
Given a set of points (in a plane), compute the sequence of points that encloses all points in the set. This benchmark is interesting as it is the simplest code that exploits the ability to implement divide-and-conquer algorithms with nested data parallelism.  We have only a "vectorised" version of this benchmark and a sequential Haskell reference implementation, "ref Haskell", using vanilla lists.
</td></tr>
<tr><th>[ Primes](http://darcs.haskell.org/packages/dph/examples/primes/)</th>
<td>
The Sieve of Eratosthenes using parallel writes into a sieve structure represented as an array of `Bool`s.  We currently don't have a proper parallel implementation of this benchmark, as we are missing a parallel version of default backpermute.  The problem is that we need to make the representation of parallel arrays of `Bool` dependent on whether the hardware supports atomic writes of bytes.  **Investigate whether any of the architectures relevant for DPH actually do have trouble with atomic writes of bytes (aka `Word8`).**</td></tr>
<tr><th>[ Quicksort](http://darcs.haskell.org/packages/dph/examples/qsort/)</th>
<td>FIXME</td></tr>
<tr><th>[ ConComp](http://darcs.haskell.org/packages/dph/examples/concomp/)</th>
<td>
Implementation of the Awerbuch-Shiloach and Hybrid algorithms for finding connected components in undirected graphs.  There is only a version directly coded against the array primitives.  **Needs to be adapted to new benchmark framework.**</td></tr>
<tr><th>[ BarnesHut](http://darcs.haskell.org/packages/dph/examples/barnesHut/)</th>
<td>
This benchmark implements the Barnes-Hut algorithm to solve the *n*-body problem in two dimensions.  **Currently won't compile with vectorisation due to excessive inlining of dictionaries.**</td></tr></table>

### Execution on LimitingFactor (2x Quad-Core Xeon)


Hardware spec: 2x 3.0GHz Quad-Core Intel Xeon 5400; 12MB (2x6MB) on-die L2 cache per processor; independent 1.6GHz frontside bus per processor; 800MHz DDR2 FB-DIMM; 256-bit-wide memory architecture; Mac OS X Server 10.5.6


Software spec: GHC 6.11 (from first week of Mar 09); gcc 4.0.1

<table><tr><th>**Program**</th>
<th>**Problem size**</th>
<th>**sequential**</th>
<th>**P=1**</th>
<th>**P=2**</th>
<th>**P=4**</th>
<th>**P=8**</th></tr>
<tr><th> SumSq, primitives </th>
<th> 10M </th>
<th> 22 </th>
<th> 40 </th>
<th> 20 </th>
<th> 10 </th>
<th> 5 
</th></tr>
<tr><th> SumSq, vectorised </th>
<th> 10M </th>
<th> 22 </th>
<th> 40 </th>
<th> 20 </th>
<th> 10 </th>
<th> 5 
</th></tr>
<tr><th> SumSq, ref C </th>
<th>10M </th>
<th> 9 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr>
<tr><th> DotP, primitives </th>
<th> 100M elements </th>
<th> 823/823/824 </th>
<th> 812/813/815 </th>
<th> 408/408/409 </th>
<th> 220/223/227 </th>
<th> 210/214/221 
</th></tr>
<tr><th> DotP, vectorised </th>
<th> 100M elements </th>
<th> 823/824/824 </th>
<th> 814/816/818 </th>
<th> 412/417/421 </th>
<th> 222/225/227 </th>
<th> 227/232/238 
</th></tr>
<tr><th> DotP, ref Haskell </th>
<th> 100M elements </th>
<th> – </th>
<th> 810 </th>
<th> 437 </th>
<th> 221 </th>
<th> 209 
</th></tr>
<tr><th> DotP, ref C </th>
<th> 100M elements </th>
<th> – </th>
<th> 458 </th>
<th> 235 </th>
<th> 210 </th>
<th> 210 
</th></tr>
<tr><th> SMVM, primitives </th>
<th> 10kx10k @ density 0.1 </th>
<th> 119/119 </th>
<th> 111/111 </th>
<th> 78/78 </th>
<th> 36/36 </th>
<th> 21/21 
</th></tr>
<tr><th> SMVM, vectorised </th>
<th> 10kx10k @ density 0.1 </th>
<th> 175/175 </th>
<th> 137/137 </th>
<th> 74/74 </th>
<th> 47/47 </th>
<th> 23/23 
</th></tr>
<tr><th> SMVM, ref C </th>
<th> 10kx10k @ density 0.1 </th>
<th>  35 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr>
<tr><th> SMVM, primitives </th>
<th> 100kx100k @ density 0.001 </th>
<th> 132/132 </th>
<th> 135/135 </th>
<th> 81/81 </th>
<th> 91/91 </th>
<th> 48/48 
</th></tr>
<tr><th> SMVM, vectorised </th>
<th> 100kx100k @ density 0.001 </th>
<th> 182/182 </th>
<th> 171/171 </th>
<th> 93/93 </th>
<th> 89/89 </th>
<th> 53/53 
</th></tr>
<tr><th> SMVM, ref C </th>
<th> 100kx100k @ density 0.001 </th>
<th>  46 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr></table>


All results are in milliseconds, and the triples report best/average/worst execution time (wall clock) of three runs.  The column marked "sequential" reports times when linked against `dph-seq` and the columns marked "P=n" report times when linked against `dph-par` and run in parallel using the specified number of parallel OS threads.

#### Comments regarding SumSq


The versions compiled against `dph-par` are by factor of two slower than the ones linked against `dph-seq`.  


However, found a number of general problems when working on this example:

- We need an extra -funfolding-use-threshold.  We don't really want users having to worry about that.
- `enumFromTo` doesn't fuse due to excessive dictionaries in the unfolding of `zipWithUP`.
- `mapP (\x -> x * x) xs` essentially turns into `zipWithU (*) xs xs`, which doesn't fuse with `enumFromTo` anymore.  We have a rewrite rule in the library to fix that, but that's not general enough.  We really would rather not vectorise the lambda abstraction at all.
- Finally, to achieve the current result, we needed an analysis that avoids vectorising subcomputations that don't to be vectorised, and worse, that fusion has to turn back into their original form.  In this case, the lambda abstraction `\x -> x * x`.  This is currently implemented in a rather limited and ad-hoc way.  We should implement this on the basis of a more general analysis.

#### Comments regarding DotP


Performance is memory bound, and hence, the benchmark stops scaling once the memory bus saturated.  As a consequence, the wall-clock execution time of the Haskell programs and the C reference implementation are the same when all available parallelism is exploited.  The parallel DPH library delivers the same single core performance as the sequential one in this benchmark.

#### Comments regarding smvm


"SMVM, vectorised" needs a lot of tinkering in the form of special rules at the moment and forcing particular inlines.  We need more expressive rewrite rules; in particular, we need these more expressive rules to express important rewrites for the replicate combinator in its various forms and to optimise shape computations that enable other optimisations.


Moreover, "SMVM, primitives" & "SMVM, vectorised" exhibit a strange behaviour from 2 to 4 threads with the matrix of density 0.001.  This might be a scheduling problem.

### Execution on greyarea (1x UltraSPARC T2)


Hardware spec: 1x 1.4GHz UltraSPARC T2; 8 cores/processors with 8 hardware threads/core; 4MB on-die L2 cache per processor; FB-DIMM; Solaris 5.10


Software spec: GHC 6.11 (from first week of Mar 09) with gcc 4.1.2 for Haskell code; gccfss 4.0.4 (gcc front-end with Sun compiler backend) for C code (as it generates code that is more than twice as fast for numeric computations than vanilla gcc)

<table><tr><th>**Program**</th>
<th>**Problem size**</th>
<th>**sequential**</th>
<th>**P=1**</th>
<th>**P=2**</th>
<th>**P=4**</th>
<th>**P=8**</th>
<th>**P=16**</th>
<th>**P=32**</th>
<th>**P=64**</th></tr>
<tr><th> SumSq, primitives </th>
<th> 10M </th>
<th> 212/212 </th>
<th> 254/254 </th>
<th> 127/127 </th>
<th> 64/64 </th>
<th> 36/36 </th>
<th> 25/25 </th>
<th> 17/17 </th>
<th> 10/10 
</th></tr>
<tr><th> SumSq, vectorised </th>
<th> 10M </th>
<th> 212/212 </th>
<th> 254/254 </th>
<th> 128/128 </th>
<th> 64/64 </th>
<th> 32/32 </th>
<th> 25/25 </th>
<th> 17/17 </th>
<th> 10/10 
</th></tr>
<tr><th> SumSq, ref C </th>
<th>10M </th>
<th> 120 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr>
<tr><th> DotP, primitives </th>
<th> 100M elements </th>
<th> 937/937 </th>
<th> 934/934 </th>
<th> 474/474 </th>
<th> 238/238 </th>
<th> 120/120 </th>
<th> 65/65 </th>
<th> 38/38 </th>
<th> 28/28 
</th></tr>
<tr><th> DotP, vectorised </th>
<th> 100M elements </th>
<th> 937/937 </th>
<th> 942/942 </th>
<th> 471/471 </th>
<th> 240/240 </th>
<th> 118/118 </th>
<th> 65/65 </th>
<th> 43/43 </th>
<th> 29/29 
</th></tr>
<tr><th> DotP, ref Haskell </th>
<th> 100M elements </th>
<th> – </th>
<th> 934 </th>
<th> 467 </th>
<th> 238 </th>
<th> 117 </th>
<th> 61 </th>
<th> 65 </th>
<th> 36 
</th></tr>
<tr><th> DotP, ref C </th>
<th> 100M elements </th>
<th> – </th>
<th> 554 </th>
<th> 277 </th>
<th> 142 </th>
<th> 72 </th>
<th> 37 </th>
<th> 22 </th>
<th> 20 
</th></tr>
<tr><th> SMVM, primitives </th>
<th> 10kx10k @ density 0.1 </th>
<th> 1102/1102 </th>
<th> 1112/1112 </th>
<th> 561/561 </th>
<th> 285/285 </th>
<th> 150/150 </th>
<th> 82/82 </th>
<th> 63/70 </th>
<th> 54/100 
</th></tr>
<tr><th> SMVM, vectorised </th>
<th> 10kx10k @ density 0.1 </th>
<th> 1784/1784 </th>
<th> 1810/1810 </th>
<th> 910/910 </th>
<th> 466/466 </th>
<th> 237/237 </th>
<th> 131/131 </th>
<th> 96/96 </th>
<th> 87/87 
</th></tr>
<tr><th> SMVM, ref C </th>
<th> 10kx10k @ density 0.1 </th>
<th> 580 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr>
<tr><th> SMVM, primitives </th>
<th> 100kx100k @ density 0.001 </th>
<th> 1112/1112 </th>
<th> 1299/1299 </th>
<th> 684/684 </th>
<th> 653/653 </th>
<th> 368/368 </th>
<th> 294/294 </th>
<th> 197/197 </th>
<th> 160/160 
</th></tr>
<tr><th> SMVM, vectorised </th>
<th> 100kx100k @ density 0.001 </th>
<th> 1824/1824 </th>
<th> 2008/2008 </th>
<th> 1048/1048 </th>
<th> 1010/1010 </th>
<th> 545/545 </th>
<th> 426/426 </th>
<th> 269/269 </th>
<th> 258/258 
</th></tr>
<tr><th> SMVM, ref C </th>
<th> 100kx100k @ density 0.001 </th>
<th> 600 </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – </th>
<th> – 
</th></tr></table>


All results are in milliseconds, and the triples report best/worst execution time (wall clock) of three runs.  The column marked "sequential" reports times when linked against `dph-seq` and the columns marked "P=n" report times when linked against `dph-par` and run in parallel using the specified number of parallel OS threads.

#### Comments regarding SumSq


As on LimitingFactor.

#### Comments regarding DotP


The benchmark scales nicely up to the maximum number of hardware threads.  Memory latency is largely covered by excess parallelism.  It is unclear why the Haskell reference implementation "ref Haskell" falls of at 32 and 64 threads.  See also [ a comparison graph between LimitingFactor and greyarea](http://justtesting.org/post/83014052/this-is-the-performance-of-a-dot-product-of-two).

#### Comments regarding smvm


As on LimitingFactor, but it scales much more nicely and improves until using four threads per core.  This suggets that memory bandwidth is again a critical factor in this benchmark (this fits well with earlier observations on other architectures).


On this machine, "SMVM primitives" & "SMVM, vectorised" also have a quirk from 2 to 4 threads.  This re-enforces the suspicion that this is a scheduling problem.

### Summary


The speedup relative to a sequential C program for SumSq, DotP, and SMVM on both architectures is illustrated by [ two summary graphs.](http://justtesting.org/post/85103645/these-graphs-summarise-the-performance-of-data)  In all cases, the data parallel Haskell program outperforms the sequential C program by a large margin on 8 cores.  The gray curve is a parallel C program computing the dot product using pthreads.  It clearly shows that the two Quad-Core Xeon with 8x1 threads are memory-limited for this benchmark, and the C code is barely any faster on 8 cores than the Haskell code.

### Regular, multidimensional arrays


First benchmark results for the multiplication of two dense matrices using `dph-seq` are summarised in [ comparison graph](http://www.scribd.com/doc/22091707/Delayed-Regular-Arrays-Sep09).
