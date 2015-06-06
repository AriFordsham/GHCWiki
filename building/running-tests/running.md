# Running the Testsuite


This section gives information on how to use the testsuite.


The following commands can all be executed simply from the `testsuite` directory.


To run the full testsuite (slow) against a GHC build in the same source tree:

```wiki
$ make
```


If you have python 2.5.2 or later then you can run the testsuite in parallel to speed it up:

```wiki
$ make THREADS=4
```


To run a reduced (fast) version of the testsuite:

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


To make this even faster, you can also go straight to the subdirectory containing the test (e.g ./tests/typecheck/should_compile/) and say 'make TEST=tc054' from there, which will save some time as the testsuite framework won't need to search as long to find the test you are referring to.


To run several tests, you just space separate them:

```wiki
$ make TEST="tc053 tc054"
```


You can also run a whole group of related tests by changing to a subdirectory in the test cases tree:

```wiki
$ cd ./tests/array
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


To just clean all the tests, without running any:

```wiki
$ make CLEANUP=1 CLEAN_ONLY=YES
```

## Additional Packages


Some of the tests in the testsuite rely on packages that aren't part of the standard libraries included with GHC. These tests will be skipped then unless you [install](debugging/installing-packages-inplace) the required packages for them. Since we don't run these tests very often, some might be currently broken.


The extra packages are:

- hmatrix
- mtl
- parallel
- parsec
- primitive
- QuickCheck
- random
- regex-compat
- syb
- utf8-string
- vector

## Problems running the testsuite


If the testsuite fails mysteriously, make sure that the timeout utility is working properly. This Haskell utility is compiled with the stage 1 compiler and invoked by the python driver, which does not print a nice error report if the utility fails. This can happen if, for example, the compiler produces bogus binaries. A workaround is to compile timeout with a stable ghc.
