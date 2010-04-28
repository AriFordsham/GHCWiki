# Working conventions for working on GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better.
This page has pointers to information you'll need.


First, make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code.


These pages guide you step-by-step through making a contribution:

- [How to fix a bug in GHC](working-conventions/fixing-bugs)
- [How to add a new feature to GHC](working-conventions/adding-features)
- [ How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Coding conventions


When you are editing GHC's source code, please follow our coding guidelines:

- [Coding style in the compiler](commentary/coding-style)
- [Coding style in the runtime system](commentary/rts/conventions)

## Validating patches and the test suite


Before you commit a patch you want to be reasonably sure you haven't broken anything.  So before you commit, you must **validate** your changes, using the **regression test suite**:

- The policy on validating patches, and how to perform validation, is at: [TestingPatches](testing-patches).
- Details about the regression test suite, and how to use it are at: [Building/RunningTests](building/running-tests)

## Using Darcs


Our conventions and some useful tips for using darcs are here: [WorkingConventions/Darcs](working-conventions/darcs).

---

## The Bug Tracker


We organise our work (both bug fixing and feature requests) using the Trac bug tracker.   There are links to the bug tracker in the sidebar under "View tickets" and "Create ticket". See also:

- [the bug reporting guidelines](report-a-bug)
- [How to fix a bug in GHC](working-conventions/fixing-bugs)

### Type and status


Every ticket has a **status** and a **type**, which appear in the title of the ticket.  Thus "Ticket [\#2762](https://gitlab.haskell.org//ghc/ghc/issues/2762) (new bug)" means status=new, and type=bug.  Here's what they mean:

- **Type** is one of `bug`, `feature request`, `task`, or `proposal`. The `proposal` type is only for [ library submission](http://www.haskell.org/haskellwiki/Library_submissions) proposals.

- **Status** says what state the ticket is in.  It is one of these:

  - **New** is for open tickets that need to be triaged or fixed.
  - **Infoneeded** means that the ticket is stalled awaiting information from the submitter (or anyone else).
  - **Closed** means what it says.
  - **Merge** means that a fix has been committed to the HEAD, but should be propagated to the current release branch.
  - **Patch** means that the ticket includes a patch for review.  We love patches!  So we try hard to review patches promptly and either commit them, or start a conversation with the author.


The intention is that tickets do not live in the Merge or Patch state for long.


You change the status of a ticket using the Action box at the bottom.

### Other Trac ticket fields


Each ticket has a bunch of other fields too:

- **Milestone**: this field is for the GHC development team to indicate by when we intend to fix the bug.  We have a milestone for each planned release (e.g. "6.12.3"), and three special milestones:

  - An empty milestone field means the bug has not been triaged yet.  We don't yet know if the
    ticket is a real, unique, issue.  Once this has been established, the ticket will be given
    a milestone.
  - **Not GHC** is for tickets that are not tied to a GHC release.
  - **_\|_** is for tickets that have been triaged, but we don't plan to fix them for a particular
    release.  This might be because the bug is low priority, or is simply too hard to fix right now.

- **Severity**: this is set by the submitter of the ticket, and indicates how important the issue is to
  them, i.e. is it preventing them from doing something altogether, or just a minor annoyance.  The
  severity might be reduced if we discover a workaround.

- **Priority**: this field is for the GHC development team to help us prioritise what we work on. On a release milestone, the highest priority tickets are blockers for that release, and the high priority tickets are those that we also plan to fix before releasing. We will also try to fix as many of the normal and lower priority tickets as possible.

- **Test Case**: fill in this field with the name of the test in the test suite.  Typically every bug
  closed should have an appropriate test case added to the test suite.

- **cc**: we pay more attention to tickets with a long cc list, so add yourself to the cc list if you care about the ticket.  Better still, add a comment to explain why you care.

- **Owned by** says who is committed to taking the ticket forward.  We typically leave this field blank until we actively start working on it, lest others feel unable to to work on a ticket because it is apparently owned, even though nothing is happening.

### Releases


When a release is made, any open tickets on that release's milestone will be moved to the next release's milestone. However, if they are more than 1 major release old (e.g. opened on the 6.10 branch, and about to be moved to the 6.14.1 milestone), and there is not a reason to keep them in the release milestone (e.g. a patch attached for review, or significant support in the CC field), then they will be moved to the `_|_` milestone instead.

### Workflow


The ticket workflow is illustrated in the following image. Most tickets will start in state "new" and, once fixed, possibly go via state "merge" if they are suitable for merging to the stable branch, before moving to state "closed". They may also go via state "infoneeded" if more information is needed from the submitter, or "patch" if a patch that needs review has been attached to the ticket.

not handled: Image