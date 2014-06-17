# How to contribute a bug fix to GHC


Please help us fix bugs in GHC!  GHC is so large, and is used in such diverse ways by so many people, that we really need your help in fixing bugs, especially those that show up in specialised situations.  


Here's how to go about helping with a bug.

1. **Make sure the bug is in Trac**.  Usually it is (that's why you are working on it), but if it's a bug you have found yourself, add it to Trac before you start work. It's important to have a ticket, because it makes sure that the bug report, discussion about the fix, the regression test that checks it, and the eventual conclusion, are all recorded together.  Comments in the code can refer to the ticket (e.g. `See Trac #2382 for an example`). And so on.  If there's no ticket, there is every chance that it'll get lost.
1. **Take ownership of the bug in Trac**, by using the "Action" chunk at the bottom of the Trac bug display (you need to be logged in to see it), and putting your name in the "reassign to" box.  This makes sure that two people don't work on the same bug.  If you later decide you can't do it after all, remove your name.
1. **Add a test case** in the [regression-test suite](building/running-tests) that shows up the bug.

  - Name the test after the bug number - this is helpful for finding the bug again in the future.  
  - Put the name of the test in the "Test Case" field of the Trac report.
1. **Fix the bug**!

  - If your proposed fix has non-local consequences, please consult us (on ghc-devs@…) before investing too much of your time.
  - Please follow our [coding conventions](working-conventions#)
  - Comment your fix in the source code, and include a reference to the bug ticket number, e.g. "`#1466`" (this helps when grepping for the fix later).  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.
1. **Record a patch** that embodies your fix.  Please follow our convention for naming patches: [WorkingConventions/Git](working-conventions/git#commit-messages).
1. **Test your patch** using the [validation script](testing-patches), before you submit it.  (If you have write permission to the repository, then you **must** validate before pushing the patch.)
1. **Submit your patch**.  If you don't have commit permission for the repository, 

  - Use 'git format-patch' to create a patch bundle:

    1. Make a commit or commits as per usual
    1. Run `git format-patch -n`, where *`n`* is the number of commits
  - Attach the patch bundle to the Trac bug report
  - Add a comment to the Trac bug report to say what you've done
  - In the 'Action' part of the Trac ticket, select "Please review".  This shifts responsibility to [the GHC team](team-ghc) to review and commit your patch.

>
> If you do have commit permission, then commit, update the Trac report, and close the bug.


Then have a beer on us.  We are truly grateful.
