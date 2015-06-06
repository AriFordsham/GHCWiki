# How to contribute a bug fix to GHC


Here's how to go about helping with a bug. For [adding features](working-conventions/adding-features), there are a few extra steps to follow.

1. **Make sure the bug is in Trac**.  Usually it is (that's why you are working on it), but if it's a bug you have found yourself, add it to Trac before you start work. It's important to have a ticket, because it makes sure that the bug report, discussion about the fix, the regression test that checks it, and the eventual conclusion, are all recorded together.  Comments in the code can refer to the ticket (e.g. `See Trac #2382 for an example`). And so on.  If there's no ticket, there is every chance that it'll get lost.
1. **Take ownership of the bug in Trac**, by using the "Action" chunk at the bottom of the Trac bug display (you need to be logged in to see it), and putting your name in the "reassign to" box.  This makes sure that two people don't work on the same bug.  You can skip this step if you've submitted your patch to Phabricator already (see below). If you later decide you can't do it after all, remove your name. 
1. **Add a test case** in the [regression-test suite](building/running-tests/adding) that shows up the bug.

  - Name the test after the bug number - this is helpful for finding the bug again in the future.  
  - Put the name of the test in the "Test Case" field of the Trac report.
1. **Fix the bug**!

  - If your proposed fix has non-local consequences, please consult us before investing too much of your time.
  - Please follow our [coding conventions](commentary/coding-style).
  - Comment your fix in the source code, and include a reference to the bug ticket number, e.g. "`#1466`" (this helps when grepping for the fix later).  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.
1. **Make a commit** that embodies your fix.  Please follow our convention for naming commits: [WorkingConventions/Git](working-conventions/git#commit-messages).
1. **Test your commit** using the [validation script or Travis](testing-patches). Alternatively, you can let [Harbormaster](phabricator/harbormaster) build and validate your commit, see next step. (If you have write permission to the repository, then you **must** validate before pushing.)
1. **Submit a code review** to [Phabricator](phabricator). Wait for [Phabricator/Harbormaster](phabricator/harbormaster) to build and validate your commit (\~35 minutes, September 2014).
1. **Update the Trac ticket.**

  - Fill out the field called "[Differential Revisions](phabricator#linking-reviews-to-trac-tickets-and-vice-versa)". Syntax: `Phab:D123`.
  - Optional: if you did not manage to submit a code review to Phabricator for whatever reason, attach a patch bundle (run `git format-patch -n`, where *`n`* is the number of commits). When you do this, it is vital you set 'Action' to "Please review", or at least leave a comment, otherwise we aren't [ notified](http://trac.edgewall.org/ticket/2259) and your patch will get lost.


Then have a beer on us.  We are truly grateful.
