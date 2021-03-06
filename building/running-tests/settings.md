# Testsuite Variables / Settings and WAYs


The following variables may be set on the command line when calling make:

```wiki
TEST                 -- specific tests to run
TEST_HC              -- compiler to use
EXTRA_HC_OPTS        -- extra flags to send to the Haskell compiler
EXTRA_RUNTEST_OPTS   -- extra flags to give the test driver
CONFIG               -- use a different configuration file
COMPILER             -- stem of a different configuration file from the config directory [default: ghc]
WAY                  -- just this way
THREADS              -- number of threads to use when running the testsuite
VERBOSE              -- verbosity
SKIP_PERF_TESTS=YES  -- skip tests which check the performance of ghc or the resulting binaries
ONLY_PERF_TESTS=YES  -- run only the tests which check the performance of ghc or the resulting binaries
```


For example, to run the `tc053` test for only the `optasm` way we would use:

```wiki
$ make test WAY=optasm TEST=tc053
```


The testsuite also has a concept called, *ways*. These refer to different settings in which a test case can be compiled and/or run. They correspond to things such as checking a test passes both when the native code generator is used and when the LLVM code generator is used.


The following ways are defined (see the file [testsuite/config/ghc](https://gitlab.haskell.org/ghc/ghc/blob/master/testsuite/config/ghc)
for the complete list):

```wiki
normal               -- no special options
llvm                 -- -fllvm
optasm               -- -O -fasm
optllvm              -- -O -fllvm
profasm              -- -O -prof -auto-all -fasm
ghci                 -- (run only, not compile) run test under GHCi
extcore              -- -fext-core (removed in ghc-7.10+)
optextcore           -- -O -fext-core  (removed in ghc-7.10+)
threaded1            -- -threaded -debug
threaded2            -- -threaded -O, and +RTS -N2 at run-time
profthreaded         -- -O -prof -auto-all -threaded
hpc                  -- -O -fhpc
static               -- -O -static
dyn                  -- -O -dynamic
dynllvm              -- -fllvm -dyn
```


By default, the 'normal' and 'hpc' ways are enabled. In addition,
certain ways are enabled automatically if the GHC build in the local
tree supports them.  Ways that are enabled this way are `optasm`,
`optllvm`, `profasm`, `threaded1`, `threaded2`, `profthreaded`, `ghci`,
and whichever of `static`/`dyn` is not GHC's default mode.
See also: [testsuite/mk/test.mk](https://gitlab.haskell.org/ghc/ghc/blob/master/testsuite/mk/test.mk).


These values are supported for `VERBOSE=n`; the default is `VERBOSE=3`:

```wiki
     n=0: No per-test output
     n=1: Only failing test results
     n=2: As above, plus progress information (names of all tests)
     n=3: As above, plus commands called.
     n=4: As above, plus performance numbers even for succeeding test cases
```