# Testing Patches


Nowadays there are many people working on GHC, which is a Good Thing, but it does mean that many people are inconvenienced if a patch is pushed which makes the HEAD unbuildable. To try to reduce the number of times this happens, we now have a stricter policy for testing patches before committing them.


In order to test your patches:

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes. Depending on what you are doing, your working repository might be appropriate; otherwise you might prefer to keep a separate repository just for patch testing.
- In the root directory of the tree, run `sh validate`. This will do a "quick" build and then run the testsuite in "fast" mode. Do not push if any of the tests give unexpected results!
- Depending on the nature of the changes, more testing might be sensible. e.g. if possible, build system changes should be tested on Linux, Mac OS X and Windows machines.  Look at the full documentation for the [test suite](building/running-tests).


The validate script should take around 20mins on a fast, dual core machine.


Assuming all is well, go ahead and commit your changes! If you have commit access then just push as normal. If not, use "darcs send --edit-description" and add a note to say what testing you have done, and on which operating system/architecture.


In order to save time while debugging problems revealed by validate, the validate script understands a couple of flags. If you run `sh validate --no-clean` then validate will not clean the tree before starting, so it will continue a previous build. If you run `sh validate --testsuite-only` then validate will not build the tree at all, but only run the testsuite. This is useful if the problems validate found were only due to the testsuite falling out of sync with the code. **Important:** When using either of these flags, be careful that you don't end up pushing patches that have not been properly validated!

## Testing different configurations


You may want to validate a different configuration, e.g. with `GhcLibWays = p`. When validating, `mk/build.mk`, where you would normally put this, is ignored; use `mk/validate.mk` instead.

## Finding unrecorded or unpushed patches


In order to find unrecorded changes run:

```wiki
./darcs-all what -l
```


from the root of your tree. To find unpushed patches, run:

```wiki
./darcs-all send --dry-run
```


also from the root of your tree.
