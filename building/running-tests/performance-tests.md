# Running Performance Tests


The test suit contains a number of performance tests included as part of a normal run of the test suit. Performance tests measure some metric (e.g. number of bytes allocated) of ghc or generated code, then compares that metric to some baseline value. Tests will fail if the measured metric is not within some tolerance percentage of the baseline value. If you'd like to add your own test, then see [adding performance Tests](building/running-tests/adding#performance-tests).

## Running via Make


In the root directory of the ghc repository or source distribution, run either `make fasttest`, `make test` (which uses the normal speed settings) or `make slowtest` (called `fulltest` in GHC \<= 7.10).


You should expect that there are no test case failures for the "normal" mode as that is a quality level that all GHC developers are expected to maintain when they check in code. There will usually be some test case failures for the full testsuite run though, and it takes a lot longer to run.


The run time of the testsuite can be reduced by running it with multiple threads. For instance if your machine has 8 cores, the following is probably appropriate:

```wiki
$ make test THREADS=8
```

## Performance Metrics are Logged

TODO How performance metrics are stored in git notes.

## Comparing Commits


There exists a comparison tool in perf_notes.py (located in the ghc/testsuite/driver/ directory) designed to help analyze the performance of the compiler across commits. When the testsuite is run, the performance metrics of the performance tests are saved automatically in a local git note that will be attached to the commit. To compare across multiple commits, execute the python file with the appropriate commandline arguments. One example is:

```wiki
$ python3 perf_notes.py HEAD 'HEAD~1' 'HEAD~5'
```


which will compare the HEAD's performance metrics with your previous commit and the 5th prior commit. The way the performance metrics are stored in git notes remains strictly local to the machine so performance metrics will not exist for a commit until you checkout that commit and run the testsuite (or test). 

## Baslines

TODO How baslines are calculated. How to make usre of CI results.
