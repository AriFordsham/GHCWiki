# GHC Test framework


GHC includes a comprehensive testsuite for catching any regressions.


The testsuite relies primarily on **GNU Make** and **Python**. Any version \>= 2.5.2 will do although avoid Python 2.6.1 as the testsuite tickles a bug in one of the included libraries.


If you just want to run the whole testsuite, then in the root of the GHC tree, typing:

```wiki
$ make test
```


will do a run in "fast" mode (which gives an idea whether there are major problems), or

```wiki
$ make fulltest
```


will do a full testsuite run (more thorough, but takes a lot longer). You should expect that there are no test case failures for the "fast" mode as that is a quality level that all GHC developers are expected to maintain when they check in code. There will usually be some test case failures for the full testsuite run though.

## Using the Testsuite

- [Running the testsuite](building/running-tests/running)
- [Testsuite Settings and WAYS](building/running-tests/settings)
- [Updating test case results](building/running-tests/updating)
- [Adding new test cases](building/running-tests/adding)
- [Testsuite implementation details](building/running-tests/details)