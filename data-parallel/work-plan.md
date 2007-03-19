## Work plan for implementing Data Parallel Haskell


Major milestones:

<table><tr><th>**Milestone 1:** basic SMP library (End of Feb 2007)</th>
<td>
Implementation of the central parts of an array library of flat and segmented arrays that uses distributed types to partition work on an SMP thread gang and uses fusion to eliminate superflous join points and intermediate arrays.
</td></tr>
<tr><th>**Milestone 2:** basic vectorisation (End of Aug 2007)</th>
<td>
Implementation of the vectorisation transformation and basic interaction of vectorised and non-vectorised code to provide (including the results from Milestone 1) a complete path from source programs to parallel executable for simple examples.
</td></tr>
<tr><th>**Milestone 3:** larger examples (End of Feb 2008)</th>
<td>
Optimisations and added functionality to handle larger example programs.
</td></tr></table>

### Current work items


For vectorisation:

- \[Roman by **30 Mar 07**\] Implement plain closure conversion


For the library:

- \[Gabi\] Implement quicksort and all missing library functions for that.

### Todo list for vectorisation

- We need closure conversion to handle the full higher-order case.
- We need to handle cross calls to un-vectorised code.

### Todo list for vectorisation

- Lifted functions
- Add missing functions
- Fusion for segmented operations
- Fusion with cost function
- NUMA support
