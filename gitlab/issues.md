# Issue conventions

A few notes about GHC's issue tracker:

 * The **weight** field is a 10-point scale of issue severity.

## Triage protocol

When a reporter opens a new issue they will generally only provide an issue description using one of the [provided templates](https://gitlab.haskell.org/ghc/ghc/tree/master/.gitlab/issue_templates). The triage process involves ensuring that the issue report is complete and filling in any missing metadata. To ensure that new issues are triaged, the issue templates apply the ~"triage needed" label to all new issues.

Triaging a new issue typically proceeds as follows:

1. Check that the ticket is really **supposed to be filed against GHC** and not another project (e.g. `haskeline` or `Cabal`). If it should be filed against another project then kindly direct the reporter to the appropriate issue tracker.

1. Apply the appropriate **labels** to the ticket. See the [labels list](labels) for the full listing of valid labels. Generally a ticket should always have the following labels:

   * one of ~bug, ~"feature request", or ~"task"; generally this will be taken care of by the template.
   * if a ~bug, one of the [bug types labels](gitlab/labels#types-of-bugs).
   * if the bug looks to be operating-system-dependent, one of the [operating system labels](gitlab/labels#operating-systems). If no operating system label is present the default is assumed to be Linux.
   * if the bug looks to be architecture-dependent, one of the [architecture labels](gitlab/labels#architecture). If no architecture label is present the default is assumed to be x86-64.
   * any appropriate [language extension](gitlab/labels#language-extensions) or [compiler subsystem](gitlab/labels#subsystems) labels
   * if the bug looks to be an appropriate task for a newcomer, apply ~newcomer
   * if any of the [miscellaneous labels](gitlab/labels#miscellaneous) are appropriate, apply then

1. Check that the ticket includes **sufficient detail** to be reproducible. If something is missing then kindly the reporter for clarification and apply the ~"info needed" label (don't forget to remove this when the reporter responds).

1. Set the **weight** field appropriately for the issue's severity. Recall that GHC uses a 10-point weight scale:

   * if the user's program crashes (~"runtime crash") or results in incorrect evaluation (~"incorrect runtime result"), set weight to 7 or higher
   * if the issue not a crash or incorrect result and is unlikely to affect a large number of users, set weight to 3 or lower.

1. Set the **milestone** field appropriately:

   * if the issue is a regression from the previous major release, set milestone to the next minor release
   * if the issue is of high severity, set milestone to the next minor release
   * if the issue is not important (e.g. weight of 3 or less) and unlikely to see attention in the next six months, set milestone to %"⊥"
   * otherwise set milestone to the next major release

1. Remove the ~"needs triage" label


## Marking an issue as resolved

When a fix for an open issue is landed, it is good practice to look over the ticket before closing it. Some questions to ask yourself:

 * Is the issue **fully resolved**? If there are still parts outstanding then it may be best to open a new ticket to track them.

 * Is the **milestone appropriate**? If the patch is low-risk or the issue severe, perhaps it should be backported to the stable branch. In this case set the milestone to the next stable release and apply the ~"backport needed" label.

   You might also considering submitting a backport merge request. See the [merge request conventions](gitlab/merge-requests) for guidance on how to do this.

 * Was a **testcase added** to the testsuite? If not then do so if possible. It is customary to name tests after their issue (e.g. #1234 would have an testcase named `T1234`). See the [testsuite documentation](building/running-tests/adding) for details.

   GHC uses a formal convention to note the addition of tests on an issue. When adding a test for an issue also leave a comment of the following form on the ticket:
   
   > This is a comment. Blah blah blah. More things to say. Lorem ipsum.
   > 
   > Tests: T1234
   
 * Are the **issue and resolving merge request linked**? A link will be created automatically if the MR description mentions the ticket. Otherwise leave a comment on the ticket pointing to the MR.
