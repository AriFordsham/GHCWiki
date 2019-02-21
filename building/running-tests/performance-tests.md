# Running Performance Tests


The test suit contains a number of performance tests included as part of a normal run of the test suit. Performance tests measure some metric (e.g. number of bytes allocated) of ghc or generated code, then compares that metric to some baseline value. Tests will fail if the measured metric is not within some tolerance percentage of the baseline value. If you'd like to add your own test, then see [adding performance Tests](building/running-tests/adding#performance-tests). To simply run the test suit see [Running the Testsuite](building/running-tests/running) and specifically [here](building/running-tests/running#performance-test-baselines) for establishing performance tests baselines.

## Performance Metrics are Logged


Whenever a performance test is run, the resulting metrics are stored using [ git notes](https://git-scm.com/docs/git-notes) under the "perf" [ ref space](https://git-scm.com/docs/git-notes#git-notes---refltrefgt) on the current commit. Git notes are generally stored locally, and not shared between git repositories (e.g. when pushing/pulling branches). This is desirable as performance is largely influenced by the machine on which they were measured. Each metric saved has the following data:

- **Environment** usually just 'local' unless run on CI.
- **Test** the name of the test.
- **Way** the way used to run the test.
- **Metric** what metric was recorded e.g. max bytes allocated.
- **Value** the observed value of the metric.


While you're free to delete notes manually, the test runner will never delete results, so multiple values for the same test may be recorded.

### CI Performance Metrics


Gitlab CI is setup to collect performance metrics and push them (as git notes) to a separate repo: [ https://gitlab.haskell.org/ghc/ghc-performance-notes.git](https://gitlab.haskell.org/ghc/ghc-performance-notes.git). You can fetch these results to the "ci/perf" ref space using the following command:

```wiki
$ git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf
```


This allows the test runner to use CI results to derive baselines where local results are not available.

## How Baslines are Calculated

TODO How baslines are calculated. How to make usre of CI results.

## Comparing Commits


There exists a comparison tool in perf_notes.py (located in the ghc/testsuite/driver/ directory) designed to help analyze the performance of the compiler across commits. When the testsuite is run, the performance metrics of the performance tests are saved automatically in a local git note that will be attached to the commit. To compare across multiple commits, execute the python file with the appropriate commandline arguments. One example is:

```wiki
$ python3 perf_notes.py HEAD 'HEAD~1' 'HEAD~5'
```


which will compare the HEAD's performance metrics with your previous commit and the 5th prior commit. The way the performance metrics are stored in git notes remains strictly local to the machine so performance metrics will not exist for a commit until you checkout that commit and run the testsuite (or test). 
