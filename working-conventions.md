# Working conventions for working on GHC

## Coding conventions


There are some documents on coding style:

- [Coding style in the compiler](commentary/coding-style)
- [Coding style in the runtime system](commentary/rts/conventions)

## Submitting patches


To submit patches to the GHC developers, please use `darcs send` to `cvs-ghc@haskell.org`.  You don't need any special permission to do this.  


Broadly speaking there are two sorts of patches: **bug fixes**, and **new features**.  We treat them differently.


We have separate guidelines for proposing changes to standard libraries; see [ Library Submissions](http://haskell.org/haskellwiki/Library_submissions).

### Bug fixes


Bug fixes always extremely welcome.  GHC is so large, and is used in such diverse ways by so many people, that we really need your help in fixing bugs, especially those that show up in specialised situations.

- In the darcs commit message, please say which Trac bug is being fixed, if any

- Comment your fix in the source code.  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.

- Please ensure that there is a test case in the regression-test suite that shows up the bug, and which is fixed by your patch.  This test case should be identified in the "Test Case" field of the Trac report.

### New features


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

  - A commit message that (a) explains what the patch does, and (b) sketches how it works.
  - A patch to the user manual that documents it (part of the main source-code patch)
  - A (separate) patch to the testsuite repository that gives a reasonable collection of tests for the new feature.  This has to be a separate patch, because the testsuite is a separate repository.

- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your patch the attention it deserves.


If you are working on a feature that you think you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.

## Committing changes


If you have permission to push patches directly to the repository (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push`:

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!

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
