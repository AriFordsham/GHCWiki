# How to contribute a new feature to GHC


We welcome your involvement in making GHC able to do more.  Here's how to do it

1. **Add a Trac feature request** for your proposed new feature (unless it's there already).  Use "feature request" in the "Type" field. 
1. **Take ownership of the ticket in Trac**, by using the "Action" chunk at the bottom of the Trac bug display (you need to be logged in to see it), and putting your name in the "reassign to" box.  This makes sure that two people don't work on the same feature.  If you later decide you can't do it after all, remove your name.
1. **Write down the specification of the feature** and either add it directly to Trac, or put it on a web page somewhere else and link to it.  Specifying before implementing is obviously a good idea; and it makes it possible for others to comment on your design before you invest heavily in building the feature.
1. **Get feedback** by emailing a suitable list (`ghc-devs` for nitty-gritty GHC internals, `glasgow-haskell-users` for user-visible features).  Often you'll get useful ideas.
1. **Implement the feature**!

  - Please follow our [coding conventions](working-conventions#)
  - Comment your fix in the source code, and include a reference to the bug ticket number, e.g. "`#1466`" (this helps when grepping for the fix later).  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.
1. **Test the feature** by creating a reasonable collection of tests that exercise the new capabilities, and incorporating the new tests in the main testsuite.  The building guide contains a [description of how to add tests](building/running-tests).
1. **Record a patch** that embodies your feature.  

  - Please follow our convention for **naming** patches: [WorkingConventions/Darcs](working-conventions/darcs#). 
  - Please give the patch a **description** that (a) explains what the patch does, and (b) sketches how it works. 
  - Include updates to the **user manual** that documents your feature.  The user manual is in the same repository as the main compiler source code, so the user-manual patch can form part of the main source-code patch.
  - Make a **(separate) patch to the testsuite repository** that embodies your new tests. This has to be a separate patch, because the testsuite is a separate repository.
1. **Test your patch** using the [validation script](testing-patches), before you submit it.
1. **Submit your patch**.  If you don't have write permission for the repository, 

  - Use 'darcs send' to create a patch bundle
  - Attach the patch bundle to the Trac report
  - Add a comment to the Trac report to say what you've done
  - Set the ticket status to 'patch', and if you are listed as owner then 'unassign' the ticket.
    This shifts responsibility to the GHC team to review and commit your patch.

> >
> > If you do have write permission, then commit, update the Trac report, and close the ticket.   **You must validate before pushing the patch**.


Then have a beer on us.  We are truly grateful.

## Things to bear in mind


While we love new features, we think twice before incorporating them into GHC's main code base. Here are some things to bear in mind:
 

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

- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your patch the attention it deserves. However, if you hear nothing for a couple of weeks then please feel free to ping us, in case your patch has slipped through the cracks.


If you are working on a feature that you think you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.
