## Status of DPH Banchmarks

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

- I only ran a first set of benchmarks when checking for what's there. I'll run the benchmarks 
  properly as next step

- Fusion doesn't work well on parallel programs yet, so for all but simple examples, the parallel program performs worse than the
  sequential

- The compiler doesn't exploit all fusion opportunities for QSort and BarnesHut. Once this is fixed, they should run considerably faster.

- Interestingly, the automatically vectorised version of qsort is quite a bit faster than the hand-flattened. Need to find out why.
