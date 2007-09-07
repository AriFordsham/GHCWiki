# Working conventions for working on GHC

## Coding conventions


There are some documents on coding style:

- [Coding style in the compiler](commentary/coding-style)
- [Coding style in the runtime system](commentary/rts/conventions)

## Testing patches


Important! See [TestingPatches](testing-patches).

## Using Darcs


Our conventions and some useful tips for using darcs are here: [WorkingConventions/Darcs](working-conventions/darcs).

## Submissions


Guidelines for submitting patches to GHC are here: [WorkingConventions/Submissions](working-conventions/submissions).

## Guidelines for people with commit permissions

- Try to make small patches (i.e. work in consistent increments).

- Separate changes that affect functionality from those that just affect
  code layout, indendation, whitespace, filenames etc.  This means that
  when looking at patches later, we don't have to wade through loads of
  non-functional changes to get to the important parts of the patch.   

- If possible, push often.  This helps to avoid conflicts.

- Rather than push conflicting patches followed by conflict resolutions, use
  amend-record (or unrecord/edit/record) to make a single patch.  Darcs currently
  doesn't handle conflicts
  well, so we are trying to keep the HEAD clean of conflicts for now.  It doesn't
  matter so much on the stable branches though.

- Every patch must pass at least minimal validation: see [TestingPatches](testing-patches).

- Discuss anything you think might be controversial before pushing it.

## The Bug Tracker


The following are GHC-specific policies for using the Trac bug tracking system.

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
