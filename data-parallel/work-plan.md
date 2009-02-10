## Work plan for implementing Data Parallel Haskell

### Milestones

1. **DUE 6 March.** Solve major performance and scalability problems for our current benchmarks.  Try to get a performance advantage over plain Haskell on LimitingFactor (i.e., 8 cores).
1. **DUE 30 March.** Presentation/poster for *Microsoft External Research Symposium*.

### Task assignments

<table><tr><th>*Roman*</th>
<td>**Template Haskell**, **Replicate**, [\#2984](https://gitlab.haskell.org//ghc/ghc/issues/2984) & **Recycling**
– status: partly implemented, but still needs serious work

- To use the special representation of task **Replicate** most effectively, we would *again* need different views on arrays together with a cost function and optimisation rules taking the cost function into account.  That requires a lot of work!
- We decided that, for the moment, Roman will first try to integrate the replication representation directly and see how far that gets us.  Maybe it helps at least with some examples and gives us something somewhat usable more quickly.
- However, before any further major changes to the library, Roman needs to first re-arrange things such that the library boilerplate is generated, instead of being hardcode; otherwise, changes require a lot of tiresome code editing.  

</td></tr></table>

<table><tr><th>*Simon*</th>
<td>**Code blow up**
– status: unknown
</td></tr></table>

<table><tr><th>*Gabi*</th>
<td>**Hierarchical matrix representation** & **Benchmark status**
– status: just started
</td></tr></table>

<table><tr><th>*Manuel*</th>
<td>**Desugaring comprehensions**
– status: not started
</td></tr></table>

### Open tasks


Category: *Bugs*

- [\#2984](https://gitlab.haskell.org//ghc/ghc/issues/2984)


Category: *Efficiency* (improve scalability and/or baseline performance of generated code):

- **Replicate:** Implement an extended array representation that uses an optimised representation for arrays that are the result of common forms of replication (i.e., due to free variables in lifted expressions).  The optimised representation stores the data to be replicated and the replication count(s) instead of actually replicating the data.  This also requires all functions consuming arrays to be adapted.

- **Recycling:** Use Roman's recycling optimisation (PADL'09) to avoid copying in `joinD`.

- **Scaling:** Investigate the scaling problems that we are seeing with vectorised code at the moment.  (**Replicate** and **Recycling** play a role here, but it is unclear whether that's all.)

- **Test new inliner:** Retest package dph with new inliner and the simplifier changes and try to simplify the library on the basis of these new phases.

- **Desugaring comprehensions:** The current desugaring of array comprehensions produces very inefficient code.  This needs to be improved.  In particular, the `map/crossMap` base case in `dotP` for `x<-xs` and use `zipWith` instead of `map/zip`.


Category:  *Compile time* (improve compile times):

- **Code blow up:** GHC generates a lot of intermediate code when vectorisation is enabled, leading to excessive compilation times.  Find out whether the new inliner helped here and what else can be done to improve this situation.


Category: *Ease of use* (make the system easier or more convenient to use for end users):

- **Conversion of vectorised representations:** We need other than just identity conversions between vanilla and vectorised data representations, especially `[:a:] <-> PArray a`.  This will make the system more convenient to use.

- **Selective vectorisation:** The scheme from our DAMP'08 paper that enables mixing vectorised and unvectorised code in a single module.

- **Unboxed values:** Extend vectorisation to handle unboxed values.

- **Prelude:** Extend vectorisation to the point, where it can compile the relevant pieces of the standard Prelude, so that we can remove the DPH-specific mini-Prelude.  (Requires: **Unboxed values**)


Category: *Case studies* (benchmarks and example applications):

- **Hierarchical matrix representation:** Sparse matrices can be space-efficiently represented by recursively decomposing them into four quadrants.  Decomposition stops if a quadrant is smaller than a threshold or contains only zeros.  Multiplication of such matrices is straight forward using Strassen's divide-and-conquer scheme, which is popular for parallel implementations.  Other operations, such as transposing a matrix, can also be efficiently implemented.  The plan is to experiment with the implementation of some BLAS routines using this representation.

- **Benchmark status:** Update and complete [DataParallel/BenchmarkStatus](data-parallel/benchmark-status).

- **N-body:** Get a fully vectorised n-body code to run and scale well on LimitingFactor.


Category: *Infrastructure* (fiddling with GHC's build system and similar infrastructure):

- **Template Haskell:** Arrange for package DPH to be build in stage2, so that we can use TH to generate library boilerplate & rewrite the library to generate the boilerplate that's currently hardcoded.

---

### Done

- **New build system:** Evaluate whether the preview of the new build system looks like it is what we want.  \[The new build system seems fine and we should have no problem building package dph in stage2 either.\]

- **CoreToStg**: Compiling package dph with the HEAD currently results in `ASSERT failed! file stgSyn/CoreToStg.lhs line 239` (with a DEBUG compiler).
