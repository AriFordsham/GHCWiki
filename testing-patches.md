# Validating Patches


Nowadays there are many people working on GHC, which is a Good Thing, but it does mean that many people are inconvenienced if a patch is pushed which makes the HEAD unbuildable. To try to reduce the number of times this happens, we now have a stricter policy for testing patches before committing them.


In order to test your patches:

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes. Depending on what you are doing, your working repository might be appropriate; otherwise you might prefer to keep a separate repository just for patch testing - see below ("Workflow with validate").

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

`validate` usually starts by `make maintainer-clean` to make sure that everything builds from scratch.  Furthermore, it ignores the build settings you have put in `mk/build.mk`, and instead uses those in `mk/validate-settings.mk`.  (It does not mess up your `mk/build.mk` file of course.)


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


Then add `expect_broken(123)` to the test options for that test, where `123` is the number of the ticket you just opened. If the problem only applies in certain circumstances, then only mark it broken for those circumstances, e.g. `when(opsys('mingw32'), expect_broken(123))`.

## Workflow with validate


All changes to GHC and the libraries need to be [validated](testing-patches) before they can be pushed to the main repositories.  Validation can take a while - 30 minutes on a 4-core machine is typical - so ideally you want to be validating changes while you are working in a separate tree.  In fact, there are other compelling reasons to have two trees in your development workflow, one for working in and one for validation:

- Validation uses build settings that are different to the ones you would normally use while developing: it adds more libraries (DPH), builds extra ways (dynamic libraries), and builds all the documentation, so you don't want to use the same build for validation and ordinary development.  In the development tree we use build settings optimised for development: `-O0 -DDEBUG` for the compiler, minimal libraries and ways so that rebuilding is fast.

- Having two trees eliminates a common source of breakage in the main repository: with one tree it is easy to add new files but forget to commit them.  Your tests will work, but the build will be broken for others.  If you have to pull your changes into a separate tree for testing, you'll notice the missing files before you push.


The typical workflow is to work in the development tree, pull into the validate tree, validate, and then push from the validate tree.  But what if validate fails?  There are two options:

1. discard the patch in the validate tree (using some instance of `git reset`) and go back to the working tree to fix it
1. or, add a new patch in the validate tree to fix the problem and re-validate


(1) is more for "back to the drawing board" kinds of failure, whereas (2) is for cases where you just need to fix a warning or some other minor error exposed by validate.

### Setting up the trees


Let's call the two trees `ghc-working` and `ghc-validate`.


Set up your repos like this:

```wiki
$ git clone git://git.haskell.org/ghc.git ghc-working
$ cd ghc-working
$ ./sync-all --no-dph get
$ cd ..
$ git clone ghc-working ghc-validate
$ cd ghc-validate
$ ./sync-all --no-dph get

$ ./sync-all -r git://git.haskell.org remote set-url origin
  # Get the dph libraries too
$ ./sync-all get
$ ./sync-all -r `pwd`/../ghc-working remote add working
$ ./sync-all -r ssh://git@git.haskell.org remote set-url --push origin
```


(omit the last step if you don't have an account for GHC's Git repositories, you can still submit patches via the mailing list (using `git format-patch` will help you with this) or send a pull request to get your changes in GHC).


Now you have `ghc-working` and `ghc-validate` repos, and additionally the `ghc-validate` repo tree is set up with a remote `working` pointing to the `ghc-working` tree, and pushing from `ghc-validate` will push changes via SSH to `git.haskell.org`.

### The rebase workflow


How do we move patches from `ghc-working` and `ghc-validate`?  There are several options here.  One is to just use `sync-all pull working master` and do merging as usual.  This works fine, but results in extra "merge commits" that aren't particularly helpful and clutter the commit logs and the mailing list.  A better approach is to rebase patches before committing.  This is done as follows:

1. Pull from `ghc-working` into `ghc-validate`: `./sync-all pull working master`
1. Rebase onto origin/master: `./sync-all pull --rebase`.  You may encounter conflicts, in which case git will tell you what to do (usually fix the conflict and then `git rebase --continue` in the appropriate repository), then you can resume with `./sync-all --resume pull --rebase` at the top.
1. Check what you have relative to origin: `./sync-all new`
1. `./validate`
1. if validate went through, `./sync-all push` (you might like to check once more what will be pushed: `./sync-all new`).


If push fails because patches have been pushed by someone else while you were validating, it is acceptable to `git pull --rebase` in that repository and push if there are no conflicts (no need to validate again).


Now, the patches pushed this way are different (have different hashes) from the patches that you originally committed in `ghc-working`, and if you try to pull these patches in `ghc-working` again, confusion and conflicts will ensue.  Fortunately there's an easy solution: just rebase again in `ghc-working`, and git will notice that your patches are already upstream and will discard the old versions.  It's as simple as

```wiki
 $ cd ghc-working
 $ ./sync-all pull --rebase
```


If rebase encounters a conflict at any point, it will tell you what to do.  After fixing the conflict and completing the rebase manually, you can then resume the pull with `./sync-all --resume pull --rebase`.


There is a slight tweak to this workflow that you might find more convenient: do a `./sync-all pull --rebase` in the `ghc-working` tree prior to pulling into `ghc-validate`.  This lets you fix conflicts in `ghc-working` rather than in `ghc-validate`, and test the resolution before validating.  The downside is that you might now have to do a lot of rebuilding in your `ghc-working` tree if there are a lot of changes to pull.
