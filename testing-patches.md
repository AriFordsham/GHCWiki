# Validating Patches


We have a strict policy for validating patches before pushing them. First make sure you read either [How to fix a bug](working-conventions/fixing-bugs) or [How to add a feature](working-conventions/adding-features), and the page about [How to add a test](building/running-tests/adding).

## How to validate patches


There are three ways to validate your patches.

### Phabricator


Any patch submitted to [Phabricator](phabricator) is automatically validated.

### Travis


Travis is a free-for-open-source continuous integration service. When your patch is not quite ready yet for review on Phabricator, you can let Travis validate it in the privacy of your own Github fork.

- Fork the GHC repository on [ Github](https://github.com/ghc/ghc).
- Sign up for [ Travis](https://travis-ci.org/).
- During the signup process, flick your ghc repository fork switch on (it will be clear when you sign up).
- Push your changes to your Github fork (any branch will do).
- You (and only you) will get an email if your patch failed to build.
- Settings are in the file `.travis.yml`, though you shouldn't have to change anything. See [Travis](travis) for details.

### Locally

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes.
- In the root directory of the tree, run

  ```wiki
  ./validate
  ```

  This will do a "quick" build and then run the testsuite in "fast" mode.

- Depending on the nature of the changes, more testing might be sensible. e.g. if possible, build system changes should be tested on Linux, Mac OS X and Windows machines.  Look at the full documentation for the [test suite](building/running-tests).


The validate script should take around 40mins on a fast, dual core machine.  If you have more cores, you can use them like this:

```wiki
CPUS=8 sh validate
```

## More details on the validate script

### Configuration files

`validate` usually starts by `make maintainer-clean` to make sure that everything builds from scratch.  Furthermore, it ignores the build settings you have put in `mk/build.mk`, and instead uses those in `mk/validate-settings.mk`.


You may want to validate a different configuration, e.g. with `GhcLibWays = p`. To do that, create a new file `mk/validate.mk` and put those settings in there.


After you run `validate` your tree will continue to use the same settings. The way to get back to using your own `build.mk` is to run `make distclean`.  Less brutally, simply remove the file `mk/are-validating.mk`.

### Flags


In order to save time while debugging problems revealed by validate, the validate script understands several flags. These flags, and others understood by `validate` are documented in the `validate` script itself.

- **`--no-clean`**:  validate will not clean the tree before starting, so it will continue a previous build. 
- **`--fast`**:  validate skips the build-a-distribution step, does not build dynamic libraries, and all but the first `way` are skipped. 
- **`--slow`**: turns on `-DDEBUG` for the stage2 compiler, and runs more tests (except those that call `compiler_stats_num_fields`, those are skipped when debugging is on)
- **`--testsuite-only`**: then validate will not build the tree at all, but only run the testsuite. This is useful if the problems validate found were only due to the testsuite falling out of sync with the code. 

### Validate has failing tests without any local patches; what do I do?


The best thing to do is to fix them! This will help make the world a better place, and gain you the admiration and thanks of your colleagues.


Fixing them could mean one of two things: Fix a bug in GHC (or the libraries) that the test is reporting, or fixing a broken test to not report a failure when nothing is actually going wrong.

#### I tried to fix them, but I got stuck


If you can't fix them yourself, then first file a ticket for the problem so that it doesn't get forgotten about. First run the testsuite for just that test (e.g. `make fast TEST=thefailingtest`), and include the full testsuite output in the ticket description. If you found any information out that might be useful to someone later fixing the bug, add that too. Also, add the name of the test in the `Test Case` field.


Then add `expect_broken(123)` to the test options for that test, where `123` is the number of the ticket you just opened. If the problem only applies in certain circumstances, then only mark it broken for those circumstances, e.g. `when(opsys('mingw32'), expect_broken(123))`.
