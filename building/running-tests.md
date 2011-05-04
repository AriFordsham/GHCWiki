# GHC Test framework


NOTE: you need GNU make and Python (any version \>= 1.5 will probably do) in order
to use the testsuite. If you want to run the testsuite in parallel then you need Python 2.5.2 or later.
(Avoid Python 2.6.1 as the testsuite tickles a bug in one of the included libraries)


If you have not checked out the test suite, first run:

```wiki
./sync-all --testsuite get
```


If you just want to run the whole test suite, then in the root of the tree running

```wiki
make test
```


will do a run in "fast" mode (which gives an idea whether there are major problems), or

```wiki
make fulltest
```


will do a full testsuite run (more thorough, but takes a lot longer).


Below we will explain how to get finer control of the test suite.

## Detail


To run the test suite against a GHC build in the same source tree:

```wiki
        cd testsuite/tests/ghc-regress
        make
```


(from now on, we'll assume that you're in the tests/ghc-regress
directory).


To run a fast version of the testsuite, which should complete in under
5 minutes on a fast machine with an optimised GHC build:

```wiki
        make fast
```


By default the testsuite uses the stage2 compiler. If you want to use another stage
(e.g. because your stage2 compiler doesn't work) then:

```wiki
        make stage=1
```


To run the test suite against a different GHC, say ghc-5.04:

```wiki
        make TEST_HC=ghc-5.04
```


To run an individual test or tests (eg. tc054):

```wiki
        make TEST=tc054
```


(you can also go straight to the directory containing the test and say
'make TEST=tc054' from there, which will save some time).


To run several tests, you just space separate them:

```wiki
        make TEST="tc054 tc053"
```


To run the tests one particular way only (eg. GHCi):

```wiki
        make WAY=ghci
```


To add specific options to the compiler:

```wiki
        make EXTRA_HC_OPTS='+RTS -K32M -RTS' 
```


To save disk space you can have temporary files deleted after each test:

```wiki
        make CLEANUP=1
```


If you have python 2.5.2 or later then you can run the testsuite in parallel:

```wiki
        make THREADS=2
```


For more details, see below.

# Running the testsuite with a compiler other than GHC


This doesn't work at the moment, but if it did then it would probably involve something like:

```wiki
        cd testsuite
        make TEST_HC=nhc98 COMPILER=nhc98
```

# Running individual tests or subdirectories of the testsuite


Most of the subdirectories in the testsuite have a Makefile.  In these
subdirectories you can use 'make' to run the test driver in two
ways:

```wiki
        make            -- run all the tests in the current directory
        make accept     -- run the tests, accepting the current output
```


The following variables may be set on the make command line:

```wiki
        TESTS                   -- specific tests to run
        TEST_HC                 -- compiler to use
        EXTRA_HC_OPTS           -- extra flags to send to the Haskell compiler
        EXTRA_RUNTEST_OPTS      -- extra flags to give the test driver
        CONFIG                  -- use a different configuration file
        COMPILER                -- stem of a different configuration file
                                -- from the config directory [default: ghc]
        WAY                     -- just this way
```


The following ways are defined (for GHC, see the file config/ghc for the complete list):

```wiki
        normal                  -- no special options
        llvm                    -- -fllvm
        optc                    -- -O -fvia-C
        optasm                  -- -O -fasm
        optllvm                 -- -O -fllvm
        profc                   -- -O -prof -auto-all -fvia-C
        profasm                 -- -O -prof -auto-all -fasm
        ghci                    -- (run only, not compile) run test under GHCi
        extcore                 -- -fext-core
        optextcore              -- -O -fext-core
        threaded1               -- -threaded -debug
        threaded2               -- -threaded -O, and +RTS -N2 at run-time
        hpc                     -- -fhpc
        dyn                     -- -O -dynamic
```


certain ways are enabled automatically if the GHC build in the local
tree supports them.  Ways that are enabled this way are optasm, profc,
profasm, threaded1, threaded2, and ghci.

# Updating tests when the output changes


If the output of a test has changed, but the new output is still
correct, you can automatically update the sample output to match the
new output like so:

```wiki
        make accept TEST=<test-name>
```


where \<test-name\> is the name of the test.  In a directory which
contains a single test, or if you want to update \*all\* the tests in
the current directory, just omit the 'TEST=\<test-name\>' part.

# Adding a new test


For a test which can be encapsulated in a single source file, follow
these steps:

1. Find the appropriate place for the test.  The GHC regression suite
  is generally organised in a "white-box" manner: a regression which
  originally illustrated a bug in a particular part of the compiler
  is placed in the directory for that part.  For example, typechecker
  regression tests go in the typechecker/ directory, parser tests
  go in parser/, and so on.  

>
> It's not always possible to find a single best place for a test;
> in those cases just pick one which seems reasonable.

>
> Under each main directory may be up to three subdirectories:
>
> > **should_compile**:    
> >
> > >
> > > tests which need to compile only
> >
> > **should_fail**:    
> >
> > >
> > > tests which should fail to compile and generate a particular error message
> >
> > **should_run**:
> >
> > >
> > > tests which should compile, run with some specific input, and generate a particular output.

>
> We don't always divide the tests up like this, and it's not
> essential to do so (the directory names have no meaning as
> far as the test driver is concerned).        

1. Having found a suitable place for the test, give the test a name.
  For regression tests, we often just name the test after the bug number (e.g. T2047).
  Alternatively, follow the convention for the directory in which you place the
  test: for example, in typecheck/should_compile, tests are named
  tc001, tc002, and so on.  Suppose you name your test T, then
  you'll have the following files:

> >
> > T.hs
> >
> > >
> > > The source file containing the test

> >
> > T.stdin   (for tests that run, and optional)
> >
> > >
> > > A file to feed the test as standard input when it
> > > runs.

> >
> > T.stdout  (for tests that run, and optional)
> >
> > >
> > > For tests that run, this file is compared against
> > > the standard output generated by the program.  If 
> > > T.stdout does not exist, then the program must not
> > > generate anything on stdout.

> >
> > T.stderr  (optional)
> >
> > >
> > > For tests that run, this file is compared
> > > against the standard error generated by the program.

> > >
> > > For tests that compile only, this file is compared
> > > against the standard error output of the compiler,
> > > which is normalised to eliminate bogus differences
> > > (eg. absolute pathnames are removed, whitespace
> > > differences are ignored, etc.)

1. Edit all.T in the relevant directory and add a line for the test.  The line is always of the form

  ```wiki
        test(<name>, <setup>, <test-fn>, <args>)
  ```

  The format of these fields is described in the [next section](building/running-tests#format-of-the-test-entries).


A multi-module test is straightforward.  It usually goes in a
directory of its own (although this isn't essential), and the source
files can be named anything you like.  The test must have a name, in
the same way as a single-module test; and the stdin/stdout/stderr
files follow the name of the test as before.  In the same directory,
place a file 'test.T' containing a line like

```wiki
   test(multimod001, normal, multimod_compile_and_run, \
                 [ 'Main', '-fglasgow-exts', '', 0 ])
```


as described above.


For some examples, take a look in tests/ghc-regress/programs.

# Format of the test entries


Each test in a `test.T` file is specified by a line the form

```wiki
      test(<name>, <setup>, <test-fn>, <args>)
```

## The \<name\> field

*\<name\>* is the name of the test, in quotes (' or ").

## The \<setup\> field

*\<setup\>*  is a function (i.e. any callable object in Python)
which allows the options for this test to be changed.
There are many pre-defined functions which can be
used in this field:

- **normal**                don't change any options from the defaults
- **skip**                  skip this test
- **skip_if_no_ghci**       skip unless GHCi is available

- **skip_if_fast**          skip if "fast" is enabled

- **omit_ways(ways)**       skip this test for certain ways

- **only_ways(ways)**       do this test certain ways only

- **extra_ways(ways)**      add some ways which would normally be disabled

- **omit_compiler_types(compilers)**                           skip this test for certain compilers

- **only_compiler_types(compilers)**       do this test for certain compilers only

- **expect_broken(bug)** this test is a expected not to work due to the indicated trac bug number

- **expect_broken_for(bug, ways)** as expect_broken, but only for the indicated ways

- **if_compiler_type(compiler_type, f)** Do `f`, but only for the given compiler type

- **if_platform(plat, f)**  Do `f`, but only if we are on the specific platform given

- **if_tag(tag, f)**        do `f` if the compiler has a given tag

- **unless_tag(tag, f)**    do `f` unless the compiler has a given tag

- **set_stdin(file)**       use a different file for stdin

- **no_stdin**              use no stdin at all (otherwise use `/dev/null`)

- **exit_code(n)**          expect an exit code of 'n' from the prog

- **extra_run_opts(opts)**  pass some extra opts to the prog

- **no_clean**              don't clean up after this test

- **extra_clean(files)**    extra files to clean after the test has completed

- **reqlib(P)**             requires package P

- **req_profiling**         requires profiling

- **ignore_output**         don't try to compare output

- **alone**                 don't run this test in parallel with anything else

- **literate**              look for a `.lhs` file instead of a `.hs` file

- **c_src**                 look for a `.c` file

- **cmd_prefix(string)**    prefix this string to the command when run

- **normalise_slashes**     convert backslashes to forward slashes before comparing the output


The following should normally not be used; instead, use the `expect_broken*`
functions above so that the problem doesn't get forgotten about, and when we
come back to look at the test later we know whether current behaviour is why
we marked it as expected to fail:

- **expect_fail**           this test is an expected failure, i.e. there is a known bug in the compiler, but we don't want to fix it.

- **expect_fail_for(ways)** expect failure for certain ways 


To use more than one modifier on a test, just put them in a list.
For example, to expect an exit code of 3 and omit way 'opt', we could use

```wiki
      [ omit_ways(['opt']), exit_code(3) ]
```


as the `<setup>` argument.

## The \<test-fn\> field

*\<test-fn\>*
is a function which describes how the test should be
run, and determines the form of \<args\>.  The possible
values are:

- **compile**  Just compile the program, the compilation should succeed.

- **compile_fail**
  Just compile the program, the
  compilation should fail (error
  messages will be in T.stderr).
  This kind of failure is mandated by the language definition - it does **not** indicate any bug in the compiler.

- **compile_and_run**
  Compile the program and run it,
  comparing the output against the 
  relevant files.

- **multimod_compile**
  Compile a multi-module program
  (more about multi-module programs
  below).

- **multimod_compile_fail**
  Compile a multi-module program,
  and expect the compilation to fail
  with error messages in T.stderr.  This kind of failure does **not** indicate a bug in the compiler.

- **multimod_compile_and_run**
  Compile and run a multi-module
  program.

- **compile_and_run_with_prefix**
  Same as compile_and_run, but with command to use to run the execution of the result binary.

- **multimod_compile_and_run_with_prefix**
  Same as multimod_compile_and_run, but with command to use to run the execution of the result binary.

- **run_command**
  Just run an arbitrary command.  The output is checked
  against `T.stdout` and `T.stderr` (unless `ignore_output`
  is used), and the stdin and expected exit code can be
  changed in the same way as for compile_and_run.  NB: run_command only works 
  in the **normal** way, so don't use **only_ways** with it.

- **ghci_script**
  Runs the current compiler, passing
  --interactive and using the specified
  script as standard input.

## The \<args\> field

*\<args\>* is a list of arguments to be passed to \<test-fn\>.


For compile, compile_fail and compile_and_run, \<args\>
is a list with a single string which contains extra
compiler options with which to run the test.  eg.

```wiki
    test('tc001', normal, compile, ['-fglasgow-exts'])
```


would pass the flag -fglasgow-exts to the compiler
when compiling tc001.


The multimod_ versions of compile and compile_and_run
expect an extra argument on the front of the list: the
name of the top module in the program to be compiled
(usually this will be 'Main').

# Sample output files


Normally, the sample `stdout` and `stderr` for a test T go in the
files `T.stdout` and `T.stderr` respectively.  However, sometimes a
test may generate different output depending on the platform,
compiler, compiler version, or word-size.  For this reason the test
driver looks for sample output files using this pattern:

```wiki
 T.stdout[-<compiler>][-<version>][-ws-<wordsize>][-<platform>]
```


Any combination of the optional extensions may be given, but they must
be in the order specified.  The most specific output file that matches
the current configuration will be selected; for example if the
platform is `i386-unknown-mingw32` then `T.stderr-i386-unknown-mingw32`
will be picked in preference to `T.stderr`.


Another common example is to give different sample output for an older
compiler version.  For example, the sample `stderr` for GHC 6.8.x would go in the file
`T.stderr-ghc-6.8`.

# The details


The test suite driver is just a set of Python scripts, as are all of
the .T files in the test suite.  The driver (driver/runtests.py) first
searches for all the .T files it can find, and then proceeds to
execute each one, keeping a track of the number of tests run, and
which ones succeeded and failed.


The script runtests.py takes several options:

>
> --config \<file\>

> >
> > \<file\> is just a file containing Python code which is 
> > executed.   The purpose of this option is so that a file
> > containing settings for the configuration options can
> > be specified on the command line.  Multiple --config options
> > may be given.

>
> --rootdir \<dir\>

> >
> > \<dir\> is the directory below which to search for .T files
> > to run.

>
> --output-summary \<file\>

> >
> > In addition to dumping the test summary to stdout, also
> > put it in \<file\>.  (stdout also gets a lot of other output
> > when running a series of tests, so redirecting it isn't  
> > always the right thing).

>
> --only \<test\>

> >
> > Only run tests named \<test\> (multiple --only options can
> > be given).  Useful for running a single test from a .T file
> > containing multiple tests.

>
> -e \<stmt\>

> >
> > executes the Python statement \<stmt\> before running any tests.
> > The main purpose of this option is to allow certain
> > configuration options to be tweaked from the command line; for
> > example, the build system adds '-e config.accept=1' to the
> > command line when 'make accept' is invoked.


Most of the code for running tests is located in driver/testlib.py.
Take a look.


There is a single Python class (TestConfig) containing the global
configuration for the test suite.  It contains information such as the
kind of compiler being used, which flags to give it, which platform
we're running on, and so on.  The idea is that each platform and
compiler would have its own file containing assignments for elements
of the configuration, which are sourced by passing the appropriate
--config options to the test driver.  For example, the GHC
configuration is contained in the file config/ghc.


A .T file can obviously contain arbitrary Python code, but the general
idea is that it contains a sequence of calls to the function test(),
which resides in testlib.py.  As described above, test() takes four
arguments:

>
> test(\<name\>, \<opt-fn\>, \<test-fn\>, \<args\>)


The function \<opt-fn\> is allowed to be any Python callable object,
which takes a single argument of type TestOptions.  TestOptions is a
class containing options which affect the way that the current test is
run: whether to skip it, whether to expect failure, extra options to
pass to the compiler, etc. (see testlib.py for the definition of the
TestOptions class).  The idea is that the \<opt-fn\> function modifies
the TestOptions object that it is passed.  For example, to expect
failure for a test, we might do this in the .T file:

```wiki
   def fn(opts):
      opts.expect = 'fail'

   test(test001, fn, compile, [''])
```


so when fn is called, it sets the instance variable "expect" in the
instance of TestOptions passed as an argument, to the value 'fail'.
This indicates to the test driver that the current test is expected to
fail.


Some of these functions, such as the one above, are common, so rather
than forcing every .T file to redefine them, we provide canned
versions.  For example, the provided function expect_fail does the
same as fn in the example above.  See testlib.py for all the canned
functions we provide for \<opt-fn\>.


The argument \<test-fn\> is a function which performs the test.  It
takes three or more arguments:

>
> \<test-fn\>( \<name\>, \<way\>, ... )


where \<name\> is the name of the test, \<way\> is the way in which it is
to be run (eg. opt, optasm, prof, etc.), and the rest of the arguments
are constructed from the list \<args\> in the original call to test().
The following \<test-fn\>s are provided at the moment:

>
> compile
> compile_fail
> compile_and_run
> multimod_compile
> multimod_compile_fail
> multimod_compile_and_run
> run_command
> run_command_ignore_output
> ghci_script


and obviously others can be defined.  The function should return
either 'pass' or 'fail' indicating that the test passed or failed
respectively.

# Problems running the testsuite

1. If the test suite fails mysteriously, make sure that the `timeout` utility is working properly. This Haskell utility is compiled with the stage 1 compiler and invoked by the python driver, which does not print a nice error report if the utility fails. This can happen if, for example, the compiler produces bogus binaries. A workaround is to compile `timeout` with a stable `ghc`.

# The testsuite and branches


It is not clear what to do with the testsuite when branching a compiler; should the testsuite also be branched?


If it is not branched then we have the problem that, given a set of tests

```wiki
test(tc1, ...)
test(tc2, ...)
test(tc3, ...)
```


if we add first one test, and then another to the HEAD

```wiki
test(tc1, ...)
test(tc2, ...)
test(tc3, ...)
test(tc4, ...)
test(tc5, ...)
```


and we want to merge `tc5` but not `tc4` to the branch then the merge has to be done by hand,
as the patch for tc5 depends on the patch for tc4, although most of the files in the patches (`tc5.hs` etc) are disjoint.


On the other hand, if it is not branched then any changes in test output mean we need to add extra logic to the test definitions, e.g.

```wiki
test(tc5, namebase_if_compiler_lt('ghc','6.9', 'tc5-6.8'), ...)
```