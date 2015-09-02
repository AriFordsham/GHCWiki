# GHC Test framework


GHC includes a comprehensive testsuite for catching any regressions.

## Using the Testsuite as a developer

- [Running the testsuite](building/running-tests/running)
- [Testsuite Settings and WAYS](building/running-tests/settings)
- [Updating test case results](building/running-tests/updating)
- [Adding new test cases](building/running-tests/adding)
- [Testsuite implementation details](building/running-tests/details)

## Using the Testsuite as a user


In the root directory of the ghc repository or source distribution, run either `make fasttest`, `make test` (which uses the normal speed settings) or `make slowtest` (called `fulltest` in GHC \<= 7.10).


You should expect that there are no test case failures for the "normal" mode as that is a quality level that all GHC developers are expected to maintain when they check in code. There will usually be some test case failures for the full testsuite run though, and it takes a lot longer to run.


The run time of the testsuite can be reduced by running it with multiple threads. For instance if your machine has 8 cores, the following is probably appropriate:

```wiki
$ make test THREADS=6
```