# GHC Test framework



GHC includes a comprehensive testsuite for catching any regressions.


## Using the Testsuite as a developer


- [Running the testsuite](building/running-tests/running)
- [Testsuite Settings and WAYS](building/running-tests/settings)
- [Updating test case results](building/running-tests/updating)
- [Adding new test cases](building/running-tests/adding)
- [Testsuite implementation details](building/running-tests/details)
- [Details about performance tests](building/running-tests/performance-tests)

## Using the Testsuite as a user


In the root directory of the ghc repository or source distribution, run either `make fasttest`, `make test` (which uses the normal speed settings) or `make slowtest` (called `fulltest` in GHC \<= 7.10).


You should expect that there are no test case failures for the "normal" mode as that is a quality level that all GHC developers are expected to maintain when they check in code. There will usually be some test case failures for the full testsuite run though, and it takes a lot longer to run.



There exists a comparison tool in perf_notes.py (located in the ghc/testsuite/driver/ directory) designed to help analyze the performance of the compiler across commits. When the testsuite is ran, the performance metrics of the performance tests are saved automatically in a local git note that will be attached to the commit. To compare across multiple commits, execute the python file with the appropriate commandline arguments. One example is:


```wiki
$ python3 perf_notes.py HEAD 'HEAD~1' 'HEAD~5'
```


which will compare the HEAD's performance metrics with your previous commit and the 5th prior commit. The way the performance metrics are stored in git notes remains strictly local to the machine so performance metrics will not exist for a commit until you checkout that commit and run the testsuite (or test). 



The run time of the testsuite can be reduced by running it with multiple threads. For instance if your machine has 8 cores, the following is probably appropriate:


```wiki
$ make test THREADS=6
```