# Working conventions for working on GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better.
This page has pointers to information you'll need.


Especially, please read:

- [How to fix a bug in GHC](working-conventions/fixing-bugs)
- [How to add a new feature to GHC](working-conventions/adding-features)


There are separate guidelines for proposing changes to standard libraries; see [ Library Submissions](http://haskell.org/haskellwiki/Library_submissions).

## Coding conventions


When you are editing GHC's source code, please follow our coding guidelines:

- [Coding style in the compiler](commentary/coding-style)
- [Coding style in the runtime system](commentary/rts/conventions)

## Using Darcs


Our conventions and some useful tips for using darcs are here: [WorkingConventions/Darcs](working-conventions/darcs).

## The Bug Tracker


We organise our work (both bug fixing and feature requests) using the Trac bug tracker.   There are links to the bug tracker in the sidebar under "View tickets" ad "Create ticket". 


The following are GHC-specific policies for using the Trac bug tracking system. (See also [the bug reporting guidelines](report-a-bug).)

- When a bug is fixed, but the patch or patches still need to be merged to other branches, then
  don't close the bug, just change its type from "bug" or "task" to "merge".  Also add a list of
  patches to be merged, and which branch to merge to, as a comment.

- **Milestones**: we have milestones for each release, and three special milestones:

  - An empty milestone field means the bug has not been triaged yet.  We don't yet know if the
    ticket is a real, unique, issue.  Once this has been established, the ticket will be given
    a milestone.
  - **Not GHC** is for tickets that are not tied to a GHC release, because they are in libraries
    or other software that is not released with GHC.  Bugs in the "extra libraries" typically fall
    into this category.
  - **_\|_** is for tickets that have been triaged, but we don't plan to fix them for a particular
    release.  This might be because the bug is low priority, or is simply too hard to fix right now.

- **Severity**: this is set by the submitter of the ticket, and indicates how important the issue is to
  them, i.e. is it preventing them from doing something altogether, or just a minor annoyance.  The
  severity might be reduced if we discover a workaround.

- **Priority**: this field is for the GHC development team to help us prioritise what we work on.  Bugs
  that have a high severity will tend to be prioritised higher, as will bugs that are regressions from
  a previous release.

- **Test Case**: fill in this field with the name of the test in the test suite.  Typically every bug
  closed should have an appropriate test case added to the test suite.
