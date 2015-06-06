# GHC Test framework


GHC includes a comprehensive testsuite for catching any regressions.

## Using the Testsuite as a developer

- [Running the testsuite](building/running-tests/running)
- [Testsuite Settings and WAYS](building/running-tests/settings)
- [Updating test case results](building/running-tests/updating)
- [Adding new test cases](building/running-tests/adding)
- [Testsuite implementation details](building/running-tests/details)

## Using the Testsuite as a user


If you just want to run the whole testsuite, then in the root of the GHC tree, typing:

```wiki
$ make fasttest # (GHC <= 7.10: make test)
```


will do a run in "fast" mode (which gives an idea whether there are major problems). On the other hand, typing:

```wiki
$ make fulltest
```


will do a full testsuite run (more thorough, but takes a lot longer). You should expect that there are no test case failures for the "fast" mode as that is a quality level that all GHC developers are expected to maintain when they check in code. There will usually be some test case failures for the full testsuite run though.


The run time of the testsuite can be reduced by running it with multiple threads. For instance if your machine has 8 cores, the following is probably appropriate:

```wiki
$ make fulltest THREADS=6
```