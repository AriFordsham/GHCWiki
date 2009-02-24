## Status of DPH Benchmarks


This page gives an overview of how well the benchmarks in the [ examples/](http://darcs.haskell.org/packages/dph/examples/) directory of package dph are currently working.

### Overview over the benchmark programs

<table><tr><th>[ DotP](http://darcs.haskell.org/packages/dph/examples/dotp/)</th>
<td>
Computes the dot product of two vectors of `Double`s.  There are two variants of this program: (1) "primitives" is directly coded against the array primitives from package dph and (2) "vectorised" is a high-level DPH program transformed by GHC's vectoriser.
</td></tr></table>

### Execution on LimitingFactor (2x Quad-Core Xeon)


Hardware spec: 2x 3.0GHz Quad-Core Intel Xeon 5400; 12MB (2x6MB) on-die L2 cache per processor; independent 1.6GHz frontside bus per processor; 800MHz DDR2; 256-bit-wide memory architecture; Mac OS X Server 10.5.6

<table><tr><th>**Program**</th>
<th>**Problem size**</th>
<th>**sequential**</th>
<th>**1 core**</th>
<th>**2 cores**</th>
<th>**4 cores**</th>
<th>**8 cores**</th></tr>
<tr><th> DotP, primitives </th>
<th> 10M elements </th>
<th> 823/823/824 </th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th> DotP, vectorised </th>
<th> 10M elements </th>
<th> 823/824/824 </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>


All results are in milliseconds, and the triples report best/average/worst execution case time (wall clock) of three runs.  The column marked "sequential" reports times when linked against `dph-seq` and the columns marked "N cores" report times when linked against `dph-par` and run in parallel on the specified number of processor cores.

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
