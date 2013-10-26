# Validating Patches


Nowadays there are many people working on GHC, which is a Good Thing, but it does mean that many people are inconvenienced if a patch is pushed which makes the HEAD unbuildable. To try to reduce the number of times this happens, we now have a stricter policy for testing patches before committing them.


In order to test your patches:

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes. Depending on what you are doing, your working repository might be appropriate; otherwise you might prefer to keep a separate repository just for patch testing (see \[Git\#Workflowwithvalidate\] for a suggested git workflow with a separate repository).

- In the root directory of the tree, run

  ```wiki
  sh validate
  ```

  This will do a "quick" build and then run the testsuite in "fast" mode. Do not push if any of the tests give unexpected results!

- Depending on the nature of the changes, more testing might be sensible. e.g. if possible, build system changes should be tested on Linux, Mac OS X and Windows machines.  Look at the full documentation for the [test suite](building/running-tests).


The validate script should take around 40mins on a fast, dual core machine.  If you have more cores, you can use them like this:

```wiki
CPUS=8 sh validate
```


Assuming all is well, go ahead and commit your changes! If you have commit access then just push as normal. If not, use "git send-email" and add a note to say what testing you have done, and on which operating system/architecture. Or send a normal email with a pull request to the ghc mailing list.

## More details on validation

`validate` usually starts by `make distclean` to make sure that everything builds from scratch.  Furthermore, it ignores the build settings you have put in `mk/build.mk`, and instead uses those in `mk/validate-settings.mk`.  (It does not mess up your `mk/build.mk` file of course.)


After you run `validate` your tree will continue to use the same settings. The way to get back to using your own `build.mk` is to run `make distclean`.  Less brutally, simply remove the file `mk/are-validating.mk`.


In order to save time while debugging problems revealed by validate, the validate script understands several flags. These flags, and others understood by `validate` are documented in the `validate` script itself.

- **`--no-clean`**:  validate will not clean the tree before starting, so it will continue a previous build. 
- **`--fast`**:  validate skips the build-a-distribution step, and does not build dynamic libraries. 
- **`--slow`**: turns on `-DDEBUG` for the stage2 compiler, and runs more tests
- **`--testsuite-only`**: then validate will not build the tree at all, but only run the testsuite. This is useful if the problems validate found were only due to the testsuite falling out of sync with the code. 

**Important:** When using either of these flags, be careful that you don't end up pushing patches that have not been properly validated!

## Testing different configurations


You may want to validate a different configuration, e.g. with `GhcLibWays = p`. When validating, `mk/build.mk`, where you would normally put this, is ignored; use `mk/validate.mk` instead.

## Finding unrecorded or unpushed patches


In order to find unrecorded changes run:

```wiki
./sync-all status
```


from the root of your tree. To find unpushed patches, run:

```wiki
./sync-all new
```


also from the root of your tree.

## Validate has failing tests without any local patches; what do I do?


The best thing to do is to fix them! This will help make the world a better place, and gain you the admiration and thanks of your colleagues.


Fixing them could mean one of two things: Fix a bug in GHC (or the libraries) that the test is reporting, or fixing a broken test to not report a failure when nothing is actually going wrong.

### I tried to fix them, but I got stuck


If you can't fix them yourself, then first file a ticket for the problem so that it doesn't get forgotten about. First run the testsuite for just that test (e.g. `make fast TEST=thefailingtest`), and include the full testsuite output in the ticket description. If you found any information out that might be useful to someone later fixing the bug, add that too. Also, add the name of the test in the `Test Case` field.

**If several tests are failing**, then file a ticket for each one *individually*, unless it is clear that a single problem is causing all the failures.


Then add `expect_broken(123)` to the test options for that test, where `123` is the number of the ticket you just opened. If the problem only appplies in certain circumstances, then only mark it broken for those circumstances, e.g. `when(opsys('mingw32'), expect_broken(123))`.
