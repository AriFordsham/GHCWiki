# Working conventions for working on GHC

## Coding conventions


There are some documents on coding style:

- [Coding style in the compiler](commentary/coding-style)
- [Coding style in the runtime system](commentary/rts/conventions)

## Submitting patches


To submit patches to the GHC developers, please use `darcs send` to `cvs-ghc@haskell.org`.  You don't need any special permission to do this.  


Broadly speaking there are two sorts of patches: **bug fixes**, and **new features**.  We treat them differently.


We have separate guidelines for proposing changes to standard libraries; see [ Library Submissions](http://haskell.org/haskellwiki/Library_submissions).

### How to submit a bug fix


Bug fixes always extremely welcome.  GHC is so large, and is used in such diverse ways by so many people, that we really need your help in fixing bugs, especially those that show up in specialised situations.

- In the darcs commit message, please say which Trac bug is being fixed, if any

- Comment your fix in the source code.  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.

- Please ensure that there is a test case in the regression-test suite that shows up the bug, and which is fixed by your patch.  This test case should be identified in the "Test Case" field of the Trac report.

### How to submit a patch for a new feature


We welcome your involvement in making GHC able to do more.  That said, we think twice before committing new features. Here are some things to bear in mind:
 

- Your patch does not need to be incorporated in the main GHC repository to be useful.  The joy of Darcs is that you can send it to anyone, and they can use it quite independently.

- It may seem that running 'darcs apply' is practically free for us; you have done all the hard work.  But it isn't:

  - It effectively commits us to maintaining it indefinitely, and worrying about its interactions with existing features
  - We try hard to keep GHC's language design somewhat coherent.  GHC deliberately tries to host a variety of ideas, not all of which may be good, but we try to keep it under control.
  - We have to do quality-control on your patch; we don't want to commit un-maintainable code... or even well-written code that we can't understand.
  - If your patch breaks something else, we'll get blamed regardless.  
  - Subsequent new features have to take account of interactions with your feature.

- New features should be switchable with their own flag, by default off.  We used to have just one flag `-fglasgow-exts` but nowadays we try to be much more selective.

- A new feature should come with 

  - A **commit message** that (a) explains what the patch does, and (b) sketches how it works.
  - A **patch to the user manual** that documents it (part of the main source-code patch)
  - A **(separate) patch to the testsuite repository** that gives a reasonable collection of tests for the new feature.  This has to be a separate patch, because the testsuite is a separate repository.

- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your patch the attention it deserves. However, if you hear nothing for a couple of weeks then please feel free to ping us, in case your patch has slipped through the cracks.


If you are working on a feature that you think you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.

## Committing changes


If you have permission to push patches directly to the repository (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push`:

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!

## The stable branch


The preferred way to make a change to the stable branch is to first record it on the HEAD,
with a note in the commit message that it should be merged. You can then also push it to the
stable branch if you wish, or you can leave it for the stable branch maintainer to do.


Sometimes it is not possible to push the HEAD patch to the stable branch, as it may depend on
other patches that are meant only for the HEAD. In this case the changes will need to be remade
in the stable branch. The commit message for this new change should include the entire commit
message of the original change, prefaced with `"MERGED: "`.
The same patch should be pushed to both branches whenever possible, though, as it makes it
easier to compare the two branches.


If you first fix a problem in the stable branch then please do apply it only to that branch
rather than leaving it lying around for when you have time to make the HEAD patch.

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

- Try not to break anything.  At the minimum, the tree should build on your system with
  the patch, better still [test your changes](building/running-tests) before
  pushing.  The nightly builds will show up any breakage on other platforms.

  If you do end up breaking the build then it's not the end of the world,
  so don't sweat about it too much. History shows that even people
  called Simon are not immune from doing so!

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
