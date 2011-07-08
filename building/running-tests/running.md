# Running the Testsuite


This section gives information on how to use the testsuite.


Firstly, change from the root of the GHC source tree to:

```wiki
$ cd testsuite/tests/ghc-regress
```


This is the root of the actual tests cases in the GHC testsuite. The directories above this one hold the actual testsuite framework for running the tests. All the commands below assume you are in this directory before running them.


To run the testsuite against a GHC build in the same source tree:

```wiki
$ make
```


This is equivalent to running `make fulltest` in the root of the GHC tree as detailed above. 


To run a fast version of the testsuite, which should complete in under 10 minutes on a fast machine with an optimised GHC build:

```wiki
$ make fast
```


By default the testsuite uses the stage2 compiler. If you want to use another stage then:

```wiki
$ make stage=1
```


To run the testsuite against a different GHC, say ghc-5.04 (this assumes the name specified corresponds to an executable on your path):

```wiki
$ make TEST_HC=ghc-5.04
```


To run an individual test or tests (eg. tc054):

```wiki
$ make TEST=tc054
```


To make this even faster, you can also go straight to the subdirectory containing the test (e.g ./typecheck/should_compile/) and say 'make TEST=tc054' from there, which will save some time as the testsuite framework won't need to search as long to find the test you are referring to.


To run several tests, you just space separate them:

```wiki
$ make TEST="tc053 tc054"
```


You can also run a whole group of related tests by changing to a subdirectory in the test cases tree:

```wiki
$ cd ./array
$ make
```


To run the tests one particular way only (eg. GHCi):

```wiki
$ make WAY=ghci
```


To add specific options to the compiler:

```wiki
$ make EXTRA_HC_OPTS='+RTS -K32M -RTS' 
```


To save disk space you can have temporary files deleted after each test:

```wiki
$ make CLEANUP=1
```


If you have python 2.5.2 or later then you can ```run the testsuite in parallel```:

```wiki
$ make THREADS=4
```


This can be a huge time saver these days with the number of cores most people have.

## Problems running the testsuite


If the testsuite fails mysteriously, make sure that the timeout utility is working properly. This Haskell utility is compiled with the stage 1 compiler and invoked by the python driver, which does not print a nice error report if the utility fails. This can happen if, for example, the compiler produces bogus binaries. A workaround is to compile timeout with a stable ghc.
