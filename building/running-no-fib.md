# The NoFib Benchmark Suite


The NoFib benchmark suite is a collection of (mostly old) Haskell programs that we use for benchmarking GHC.  The NoFib suite is kept in a separate darcs repository (see [DarcsRepositories](darcs-repositories)), and it should be checked out at the top level of a GHC source tree, i.e. at the same level as `compiler` and `libraries`.


To run the tests:

```wiki
  $ cd nofib
  $ make clean
  $ make boot
  $ make 2>&1 | tee nofib-log
```


will put the results in the file `nofib-log`.


To compare the results of multiple runs, use the program
[nofib/nofib-analyse/nofib-analyse](/trac/ghc/browser/ghc/nofib/nofib-analyse/nofib-analyse).  Something like this:

```wiki
  $ nofib-analyse nofib-log-6.4.2 nofib-log-6.6
```


to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`.  When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you *wanted* to change.  There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more.  To be on the safe
side, make both runs on the same unloaded machine.


To get measurements for simulated instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of 
[ Valgrind](http://valgrind.org). You can run nofib under valgrind like this:

```wiki
  $ make SRC_RUNTEST_OPTS=-cachegrind
```

## Tweaking things


To tweak things, add settings to your `mk/build.mk` (see [Commentary/SourceTree](commentary/source-tree)).

- Each benchmark is run in each "way" in `NoFibWays`.  By default `NoFibWays` is initialised to `GhcLibWays`, but you can override that in `mk/build.mk`. Typically, to just use the vanilla way, set `NoFibWays` to empty:

  ```wiki
  NoFibWays =
  ```

- By default nofib uses the stage-2 compiler from your build tree.  To tell nofib to use a different compiler, set `WithNofibHc`.  For example:

  ```wiki
  WithNofibHc = /home/simonpj/builds/HEAD/inplace/bin/ghc-stage1
  ```