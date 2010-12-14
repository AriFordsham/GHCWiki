## Plan for the December 2010 Release

### To be released components

- GHC 7.0.2
- DPH packages
- Repa packages


(No release of vector should be necessary, as 0.7.0.1 should work fine.)

### Before the release we must achieve the following


Documentation:

- Haddock documentation of Data.Array.Parallel **[???](data-parallel/dec2010-release?)**
- HowTo and examples on HaWiki [ http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)**\[MANUEL\]**
- Replace `-XPArr` by `-XParallelArrays` in the Users Guide, also add `-fvectorise`


API adaptation: APIs of vector, Repa, and Accelerate should be unified as far as possible

- Repa's current 'replicate' should be renamed and a new 'replicate' that turns a scalar into an array should be introduced **[???](data-parallel/dec2010-release?)**
- Check similarity of singleton/unit between all three libraries **[???](data-parallel/dec2010-release?)**


Changes:

- -fdph-par should be the default (and sensible error message if the dph package is not available) (done, but need to still sort out a build problem) **\[MANUEL\]**
- -Odph should be equivalent to '-O2 -fsimplifier-phases=3 -fsimplifier-iterations=20' (done, not pushed yet) **\[MANUEL\]**
- Move GHC.PArr into  the DPH libs.  (Needed for Haddock.) **\[MANUEL\]**
- Find out if we still need the `NoSpecConstr` annotation and remove it if not **\[ROMAN\]**


Bug fixes:

- Vectoriser needs to be adapted to Simon's recursive superclasses patch **\[ROMAN\]**
- The combination '-fvectorise -O0' should work **\[ROMAN\]**
- Trying to vectorise the `DotP` example from the tutorial on the Haskell Wiki, `-fdph-seq` fails with (`-fdph-par` works fine)

  ```wiki
  *** Vectorisation error ***
      Tycon not vectorised:  Data.Array.Parallel.Lifted.PArray.PArray
  ```
- LLVM back end not working with DPH (held up due to LLVM backend problems in the HEAD [\#4438](https://gitlab.haskell.org//ghc/ghc/issues/4438)) **\[BEN\]**
- Repa edge-detection is deadlocking with more than 2 threads \[DONE\]
- Fix the BH seg fault in DPH. Roman has found the problem \[DONE\]


Performance goals:

- Vector works fast, sequentially, compared to C, Haskell mutable-array version \[FINE\]

  - Benchmarks: NoSlow, vector versions of Repa benchmarks
- Repa works fast in parallel

  - MMult \[OK, but about 20% slower than in 6.13; try with LLVM and w/o bounds checks\] \[BEN\]
  - Laplace **\[SLOW & DOESN'T SCALE\]****\[BEN\]**
  - Blur \[OK\]
  - EdgeDetect \[OK\]
  - FFT \[OK\]
- Statically-nested DPH programs should work fast, in parallel

  - SumSquares \[FINE\]
  - Dot product \[FINE\]
  - Evens \[OK (but more than 3 times slower than C; any improvement since [\#4830](https://gitlab.haskell.org//ghc/ghc/issues/4830) was fixed?)\]

    - rl reckons this is due to GHC compiling modulo of powers of two inefficiently; c.f., [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065) (in `packByTags`)
  - SMVM (blocked on optimisation of lifted indexing) **\[BROKEN\]****\[BEN & ROMAN\]**
- Dynamically-nested DPH programs without user-defined datatypes should run correctly, but not necessarily fast

  - Quicksort **\[BROKEN ([SpecConstr](spec-constr)) & SLOW\]****\[SIMON & BEN\]**
  - Quickhull **\[OK, but has a [SpecConstr](spec-constr) problem that we want to fix\]****\[ROMAN\]**

    - Probably affected by the same optimisation issue with the compilation of modulo operations as Evens
    - [\#4830](https://gitlab.haskell.org//ghc/ghc/issues/4830): this fix wasn't sufficient, still doesn't optimise properly
- Dynamically-nested DPH programs with user-defined datatypes should run correctly, but not necessarily fast

  - Words **\[BROKEN ([SpecConstr](spec-constr) when using `-dph-seq`)\]****\[ROMAN & SIMON\]**

    - [\#4831](https://gitlab.haskell.org//ghc/ghc/issues/4831)
  - BarnesHut \[OK\]


Legend

<table><tr><th>\[FINE\]</th>
<td>
Works well
</td></tr>
<tr><th>\[OK\]</th>
<td>
Fine for the release, but could be better
</td></tr>
<tr><th>\[SLOW\]</th>
<td>
Not usable
</td></tr></table>


Tags in **bold** require attention before the release.


More benchmarks details at [DataParallel/BenchmarkStatus](data-parallel/benchmark-status)