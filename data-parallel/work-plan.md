## Work plan for implementing Data Parallel Haskell


Major milestones:

<table><tr><th>**Milestone 1:** basic SMP library (End of Feb 2007)</th>
<td>
Implementation of the central parts of an array library of flat and segmented arrays that uses distributed types to partition work on an SMP thread gang and uses fusion to eliminate superflous join points and intermediate arrays. **\[Completed\]**</td></tr>
<tr><th>**Milestone 2:** basic vectorisation (End of Aug 2007)</th>
<td>
Implementation of the vectorisation transformation and basic interaction of vectorised and non-vectorised code to provide (including the results from Milestone 1) a complete path from source programs to parallel executable for simple examples.
</td></tr>
<tr><th>**Milestone 2.5:** stocktake (by ICFP)</th>
<td>
Technical report summarising the results so far & release of a first publicly announced end-to-end NDP system.
</td></tr>
<tr><th>**Milestone 3:** larger examples (End of Feb 2008)</th>
<td>
Optimisations and added functionality to handle larger example programs.
</td></tr></table>

### Current work items


For vectorisation:

- \[Roman, June/July\] Implement vectorisation transformation according to the scheme in `ghc-ndp/docs/ndp/`.
- \[Manuel\] Work with Roman and in particular implement all the iface-related code (`HscTypes.VectInfo` and friends).


For the library:

- \[Gabi\] Complete quicksort implementation.

### Todo list for vectorisation

- Implement the first version
- Integeration with package ndp
- Testing

### Todo list for package ndp

- Lifted functions
- Add missing functions
- Fusion for segmented operations
- Fusion with cost function
- NUMA support
