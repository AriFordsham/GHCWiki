## Status of DPH Benchmarks


This page gives an overview of how well the benchmarks in the [ examples/](http://darcs.haskell.org/packages/dph/examples/) directory of package dph are currently working.

### Overview over the benchmark programs

<table><tr><th>[ DotP](http://darcs.haskell.org/packages/dph/examples/dotp/)</th>
<td>
Computes the dot product of two vectors of `Double`s.  There are two variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.  In addition to these two DPH variants of the dot product, we also have two non-DPH reference implementations: (a) "ref Haskell" is a Haskell program using imperative, unboxed arrays and and (b) "ref C" is a C implementation using pthreads.
</td></tr>
<tr><th>[ SMVM](http://darcs.haskell.org/packages/dph/examples/smvm/)</th>
<td>
Multiplies a dense vector with a sparse matrix represented in the *compressed sparse row format (CSR).*  There are three variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.
</td></tr></table>

### Execution on LimitingFactor (2x Quad-Core Xeon)


Hardware spec: 2x 3.0GHz Quad-Core Intel Xeon 5400; 12MB (2x6MB) on-die L2 cache per processor; independent 1.6GHz frontside bus per processor; 800MHz DDR2 FB-DIMM; 256-bit-wide memory architecture; Mac OS X Server 10.5.6


Software spec: GHC 6.11 (from end of Feb 09); gcc 4.0.1

<table><tr><th>**Program**</th>
<th>**Problem size**</th>
<th>**sequential**</th>
<th>**P=1**</th>
<th>**P=2**</th>
<th>**P=4**</th>
<th>**P=8**</th></tr>
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
<th> ?? elems, density ?? </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> SMVM, vectorised </th>
<th> ?? elems, density ?? </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>


All results are in milliseconds, and the triples report best/average/worst execution time (wall clock) of three runs.  The column marked "sequential" reports times when linked against `dph-seq` and the columns marked "P=n" report times when linked against `dph-par` and run in parallel using the specified number of parallel OS threads.

#### Observations regarding DotP


Performance is memory bound, and hence, the benchmark stops scaling once the memory bus saturated.  As a consequence, the wall-clock execution time of the Haskell programs and the C reference implementation are the same when all available parallelism is exploited.  The parallel DPH library delivers the same single core performance as the sequential one in this benchmark.

### Execution on greyarea (1x UltraSPARC T2)


Hardware spec: 1x 1.4GHz UltraSPARC T2; 8 cores/processors with 8 hardware threads/core; 4MB on-die L2 cache per processor; FB-DIMM; Solaris 5.10


Software spec: GHC 6.11 (from end of Feb 09); gccfss 4.0.4 (gcc front-end with Sun compiler backend)

<table><tr><th>**Program**</th>
<th>**Problem size**</th>
<th>**sequential**</th>
<th>**P=1**</th>
<th>**P=2**</th>
<th>**P=4**</th>
<th>**P=8**</th>
<th>**P=16**</th>
<th>**P=32**</th>
<th>**P=64**</th>
<th></th></tr>
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
</th>
<th></th></tr>
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
</th>
<th></th></tr>
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
</th>
<th></th></tr>
<tr><th> DotP, ref C </th>
<th> 100M elements </th>
<th> – </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> SMVM, primitives </th>
<th> ?? elems, density ?? </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> SMVM, vectorised </th>
<th> ?? elems, density ?? </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>


All results are in milliseconds, and the triples report best/worst execution time (wall clock) of three runs.  The column marked "sequential" reports times when linked against `dph-seq` and the columns marked "P=n" report times when linked against `dph-par` and run in parallel using the specified number of parallel OS threads.

#### Observations regarding DotP


The benchmark scales nicely up to the maximum number of hardware threads.  Memory latency is largely covered by excess parallelism.  It is unclear why the Haskell reference implementation "ref Haskell" falls of at 32 and 64 threads.

---

<table><tr><th>**Program**</th>
<th>**Sequential (manually vectorised) **</th>
<th>**Vectorised**</th>
<th>** Parallel**</th></tr>
<tr><th> DotP           </th>
<th>Order of mag. faster than list impl      </th>
<th> Same performance as seq. </th>
<th> speedup of 2 for 2 CPUs, 4 threads  
</th></tr>
<tr><th> QuickSort     </th>
<th>Slower than list (fusion)                </th>
<th> Slower than seq. (why?)  </th>
<th> speedup of 1.4 on 2 CPUs            
</th></tr>
<tr><th> SparseVector  </th>
<th>Similar to DotP                          </th>
<th></th>
<th></th></tr>
<tr><th> Primes (Nesl)  </th>
<th>15 x faster than list version            </th>
<th> NYI                      </th>
<th> 20 x slower than seq (fusion?)      
</th></tr>
<tr><th> Primes (Simon) </th>
<th>NYI                                      </th>
<th> Working                  </th>
<th> NYI                                 
</th></tr>
<tr><th> BarnesHut     </th>
<th>Small bug in alg                         </th>
<th> Working                  </th>
<th> See seq.                            
</th></tr></table>


General remarks:

- I only ran a first set of benchmarks when checking for what's there. I'll run the benchmarks properly as next step

- Fusion doesn't work well on parallel programs yet, so for all but simple examples, the parallel program performs worse than the sequential

- The compiler doesn't exploit all fusion opportunities for QSort and BarnesHut. Once this is fixed, they should run considerably faster.

- Interestingly, the automatically vectorised version of qsort is quite a bit faster than the hand-flattened. Need to find out why.
