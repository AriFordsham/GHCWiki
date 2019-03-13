# Performance Tests


The test suit contains a number of performance tests included as part of a normal run of the test suit. Performance tests measure some metric (e.g. number of bytes allocated) of ghc or generated code, then compares that metric to some baseline value. Tests will fail if the measured metric is not within some tolerance percentage of the baseline value. If you'd like to add your own test, then see [adding performance Tests](building/running-tests/adding#performance-tests). To simply run the test suit see [Running the Testsuite](building/running-tests/running) and specifically [here](building/running-tests/running#performance-test-baselines) for establishing performance test baselines.

## Performance Metrics are Logged


Whenever a performance test is run, the resulting metrics are stored using [git notes](https://git-scm.com/docs/git-notes) under the "perf" [ref space](https://git-scm.com/docs/git-notes#git-notes---refltrefgt) on the current commit. Git notes are generally stored locally, and not shared between git repositories (e.g. when pushing/pulling branches). This is desirable as performance is largely dependent on the machine on which the tests are run. Each metric saved has the following data:

- **Environment** usually just 'local' unless run on CI.
- **Test** the name of the test.
- **Way** the way used to run the test.
- **Metric** what metric was recorded e.g. max bytes allocated.
- **Value** the observed value of the metric.


While you're free to delete notes manually via the [git notes --ref=perf](https://git-scm.com/docs/git-notes) command, the test runner will never delete results, so multiple values for the same test may be recorded.

### CI Performance Metrics


Gitlab CI is setup to collect performance metrics and push them (again as git notes) to a separate repo: [https://gitlab.haskell.org/ghc/ghc-performance-notes.git](https://gitlab.haskell.org/ghc/ghc-performance-notes.git). You can fetch these results to the "ci/perf" ref space using the following command:

```wiki
$ git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf
```


This allows the test runner to use CI results to derive baselines where local results are not available.

## How Baselines are Calculated


While a tolerance percentage is specified manually in `*.T`, the baseline (i.e. expected) value of performance tests are recovered from previous runs of the performance tests (logged in git notes). Results [fetched from CI](building/running-tests/performance-tests#) may also be used, in which case a CI [environment](building/running-tests/performance-tests#) is chosen based on the local machine architecture and os. Baselines are derived per `(Test, Way, Metric)` independently, by searching git notes as follows:

1. If the current commit message specifies an [expected change](building/running-tests/performance-tests#expected-performance-changes) for the metric, then stop. The Baseline is undefined.
1. Move to the parent commit.
1. If metrics for the given `(Test, Way, Metric)` exist in a git note for this commit (in ref space `perf`), then the baseline is the average value of those metrics.
1. Else if metrics for the given `(Chosen CI Environment, Test, Way, Metric)` exist in a git note for this commit (in ref space `ci/perf`), then the baseline is the average value of those metrics.
1. If the maximum search depth is reached then stop, the baseline is undefined. Else continue to step 1.

## Expected Performance Changes


In many cases, a new commit has expected performance changes. In order to allow the test suit to pass, these changes must be documented in the commit message in the format

```wiki
Metric (In|De)crease <metric(s)> <options>:
    <tests>
```


where metrics and options are optional and allow you to specify a metric or list of metrics, the way, and test environment. Here are some examples:

```wiki
Metric Increase ['bytes allocated', 'peak_megabytes_allocated'] (test_env='linux_x86', way='default'):
    Test012
    Test345
Metric Decrease 'bytes allocated':
    Test678
Metric Increase:
    Test711
```


Upon failing some performance tests, the test runner will output  the string required to accept all changes. First double check that you really do expect those changes! If so, you can simply copy the text into the commit message and rerun the tests to ensure they pass.


CAUTION: make sure you maintain the correct expected changes in your commit messages when squashing commits.

## Comparing Commits


There exists a comparison tool in perf_notes.py (located in the ghc/testsuite/driver/ directory) designed to help analyze the performance of the compiler across commits. When the testsuite is run, the performance metrics of the performance tests are saved automatically in a local git note that will be attached to the commit. To compare across multiple commits, execute the python file with the appropriate commandline arguments. One example is:

```wiki
$ python3 perf_notes.py HEAD 'HEAD~1' 'HEAD~5'
```


which will compare the HEAD's performance metrics with your previous commit and the 5th prior commit. The way the performance metrics are stored in git notes remains strictly local to the machine so performance metrics will not exist for a commit until you checkout that commit and run the testsuite (or test). 