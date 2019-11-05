# Performance Tests

The test suite contains a number of performance tests included as part of a normal run of the test suite. Performance tests measure some metric (e.g. number of bytes allocated) of ghc or generated code, then compares that metric to some baseline value. Tests will fail if the measured metric is not within some tolerance percentage of the baseline value. If you'd like to add your own test, then see [adding performance Tests](building/running-tests/adding#performance-tests). To simply run the test suite, see [Running the Testsuite](building/running-tests/running) and specifically [here](building/running-tests/running#performance-test-baselines) for establishing performance test baselines.

See [here](/performance/tests) for notes on the original proposal.

## Performance Metrics are Logged


Whenever a performance test is run, the resulting metrics are stored using [git notes](https://git-scm.com/docs/git-notes) under the "perf" [ref space](https://git-scm.com/docs/git-notes#git-notes---refltrefgt) on the current commit. Git notes are generally stored locally, and not shared between git repositories (e.g. when pushing/pulling branches). This is desirable as performance is largely dependent on the machine on which the tests are run. Each metric saved has the following data:

- **Environment** usually just 'local' unless run on CI.
- **Test** the name of the test.
- **Way** the way used to run the test.
- **Metric** what metric was recorded e.g. max bytes allocated.
- **Value** the observed value of the metric.


While you're free to delete notes manually via the [git notes --ref=perf](https://git-scm.com/docs/git-notes) command, the test runner will never delete results, so multiple values for the same test may be recorded.

### CI Performance Metrics


Gitlab CI is setup to collect performance metrics and push them (again as git notes) to a separate repo: [https://gitlab.haskell.org/ghc/ghc-performance-notes.git](https://gitlab.haskell.org/ghc/ghc-performance-notes.git). This will only contain performance metrics for commits on master, and CI jobs that pass successfully (keep this in mind when analyzing CI results).

You can fetch these results to the "ci/perf" ref space using the following command:

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


In many cases, a new commit has expected performance changes. In order to allow the test suite to pass, these changes must be documented in the commit message in the format

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

There exists a comparison tool located at `testsuite/driver/perf_notes.py` to help analyze the performance metrics commits. Run the commandline `testsuite/driver/perf_notes.py --help` to see the available options. E.g. to see a chart of the last 100 commits as a standalone html file (omit `--chart` to get simple text output to stdout):

```wiki
$ python3 testsuite/driver/perf_notes.py --chart HEAD~100..HEAD
$ firefox ./PerformanceChart.html
```

This will show results of your *local* runs of performance tests (see [above](#performance-metrics-are-logged)). You can also view results from CI using the `--ci` option. Make sure to [fetch CI results](#ci-performance-metrics) first. There are a lot of results, so you'll likely want to filter for a specific test/environment e.g.:

![Screenshot_from_2019-05-29_11-45-34](uploads/3a2783b354df3cdea54bf2c0c1575aff/Screenshot_from_2019-05-29_11-45-34.png)
[PerformanceChart.html](uploads/a385555a89124fa049310f3d812febf7/PerformanceChart.html)

```
$ git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/ci/perf
$ python3 testsuite/driver/perf_notes.py --chart --ci --test-name "T9630" --test-env x86_64-linux-deb9  master~100..master
$ firefox ./PerformanceChart.html
```