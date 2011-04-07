# Validating Patches


Nowadays there are many people working on GHC, which is a Good Thing, but it does mean that many people are inconvenienced if a patch is pushed which makes the HEAD unbuildable. To try to reduce the number of times this happens, we now have a stricter policy for testing patches before committing them.


In order to test your patches:

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes. Depending on what you are doing, your working repository might be appropriate; otherwise you might prefer to keep a separate repository just for patch testing.

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


Assuming all is well, go ahead and commit your changes! If you have commit access then just push as normal. If not, use "darcs send --edit-description" and add a note to say what testing you have done, and on which operating system/architecture.

## More details on validation

`validate` usually starts by `make distclean` to make sure that everything builds from scratch.  Furthermore, it ignores the build settings you have put in `mk/build.mk`, and instead uses those in `mk/validate-settings.mk`.  (It does not mess up your `mk/build.mk` file of course.)


After you run `validate` your tree will continue to use the same settings. The way to get back to using your own `build.mk` is to run `make distclean`.  Less brutally, simply remove the file `mk/are-validating.mk`.


In order to save time while debugging problems revealed by validate, the validate script understands several flags. These flags, and others understood by `validate` are documented in the `validate` script itself.

- **`--no-clean`**:  validate will not clean the tree before starting, so it will continue a previous build. 
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
