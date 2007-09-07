# Submitting patches


To submit patches to the GHC developers, please use `darcs send` to `cvs-ghc@haskell.org`.  You don't need any special permission to do this.  


Broadly speaking there are two sorts of patches: **bug fixes**, and **new features**.  We treat them differently.


We have separate guidelines for proposing changes to standard libraries; see [ Library Submissions](http://haskell.org/haskellwiki/Library_submissions).

## How to submit a bug fix


Bug fixes always extremely welcome.  GHC is so large, and is used in such diverse ways by so many people, that we really need your help in fixing bugs, especially those that show up in specialised situations.

- Follow our convention for naming patches: [WorkingConventions/Darcs](working-conventions/darcs#).

- Comment your fix in the source code.  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.

- Please ensure that there is a test case in the regression-test suite that shows up the bug, and which is fixed by your patch.  This test case should be identified in the "Test Case" field of the Trac report.

## How to submit a patch for a new feature


We welcome your involvement in making GHC able to do more.  That said, we think twice before committing new features. Here are some things to bear in mind:
 

- Your patch does not need to be incorporated in the main GHC repository to be useful.  The joy of Darcs is that you can send it to anyone, and they can use it quite independently.

- It may seem that running 'darcs apply' is practically free for us; you have done all the hard work.  But it isn't:

  - It effectively commits us to maintaining it indefinitely, and worrying about its interactions with existing features
  - We try hard to keep GHC's language design somewhat coherent.  GHC deliberately tries to host a variety of ideas, not all of which may be good, but we try to keep it under control.
  - We have to do quality-control on your patch; we don't want to commit un-maintainable code... or even well-written code that we can't understand.
  - If your patch breaks something else, we'll get blamed regardless.  
  - Subsequent new features have to take account of interactions with your feature.

- New features should be switchable with their own flag, by default off.  We used to have just one flag `-fglasgow-exts` but nowadays we try to be much more selective.

- We are much happier about features whose implementation is:   

  - **Localised**: only a handful of places in the code are changed.
  - **Orthogonal**: no new invariants or constraints are added that subsequent changes must bear in mind. This is really important; any change that imposes costs on later, apparently unrelated, changes is much more costly. 

- A new feature should come with 

  - A **patch name** and **description** that (a) explains what the patch does, and (b) sketches how it works.  See "Patch naming" below for conventions concerning the patch name.
  - A **patch to the user manual** that documents it (part of the main source-code patch)
  - A **(separate) patch to the testsuite repository** that gives a reasonable collection of tests for the new feature.  This has to be a separate patch, because the testsuite is a separate repository.

- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your patch the attention it deserves. However, if you hear nothing for a couple of weeks then please feel free to ping us, in case your patch has slipped through the cracks.


If you are working on a feature that you think you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.
