# How to contribute a new feature to GHC


We welcome your involvement in making GHC able to do more.  Here's how to do it.

1. **Add a Trac feature request** for your proposed new feature (unless it's there already).  Use "feature request" in the "Type" field. 
1. **Take ownership of the ticket in Trac**, by using the "Action" chunk at the bottom of the Trac bug display (you need to be logged in to see it), and putting your name in the "reassign to" box.  This makes sure that two people don't work on the same feature.  If you later decide you can't do it after all, remove your name.
1. **Write down the specification of the feature** and create a Wiki page for it, or put it on a web page somewhere else and link to it.  Specifying before implementing is obviously a good idea; and it makes it possible for others to comment on your design before you invest heavily in building the feature.
1. **Get feedback** by emailing a suitable list (`ghc-devs` for nitty-gritty GHC internals, `glasgow-haskell-users` for user-visible features).  Often you'll get useful ideas.
1. **Implement the feature**!

  - Please follow our [coding conventions](working-conventions#).
  - Comment your fix in the source code, and include a reference to the bug ticket number, e.g. "`#1466`" (this helps when grepping for the fix later).  It is often helpful to give a small example code fragment that demonstrates the need for your fix.  This isn't always relevant; sometimes you are fixing a plain error, but often it's more subtle than that.
1. **Test the feature** by creating a reasonable collection of tests that exercise the new capabilities, and incorporating the new tests in the main testsuite.  The building guide contains a [description of how to add tests](building/running-tests).
1. **Make a commit** that embodies your feature.  

  - Please follow our convention for **naming** commits: [WorkingConventions/Git](working-conventions/git#commit-messages). 
  - Please give the commit a **description** that (a) explains what the commit does, and (b) sketches how it works. 
  - Include updates to the **user manual** that documents your feature.  The user manual is in the same repository as the main compiler source code, so the user-manual commit can form part of the main source-code commit.
1. **Test your commit** using the [validation script or Travis](testing-patches). Alternatively, you can let [Harbormaster](phabricator/harbormaster) build and validate your commit, see next step. (If you have write permission to the repository, then you **must** validate before pushing.)
1. **Submit a code review** to [Phabricator](phabricator). Wait for [Phabricator/Harbormaster](phabricator/harbormaster) to build and validate your commit (\~35 minutes, September 2014).
1. **Update the Trac ticket.**

  - Add a comment to say what you've done.
  - Fill out the field called "[Differential Revisions](phabricator#linking-reviews-to-trac-tickets-and-vice-versa)". Syntax: `Phab:D123`.
  - In the 'Action' part of the Trac ticket, select "Please review". This shifts responsibility to [the GHC team](team-ghc) to review and push your commit if it is accepted.
  - Optional: if you did not manage to submit a code review to Phabricator for whatever reason, attach a patch bundle (run `git format-patch -n`, where *`n`* is the number of commits). When you do this, it is vital you set 'Action' to "Please review", or at least leave a comment, otherwise we aren't [ notified](http://trac.edgewall.org/ticket/2259) and your patch will get lost.

## Things to bear in mind


While we love new features, we think twice before incorporating them into GHC's main code base. Here are some things to bear in mind:
 

- Your proposal does not need to be incorporated in the main GHC repository to be useful. The joy of Git is that you can send it to anyone, and they can use it quite independently.

- It may seem that running 'git push' is practically free for us; you have done all the hard work.  But it isn't:

  - It effectively commits us to maintaining it indefinitely, and worrying about its interactions with existing features
  - We try hard to keep GHC's language design somewhat coherent.  GHC deliberately tries to host a variety of ideas, not all of which may be good, but we try to keep it under control.
  - We have to do quality-control on your proposal; we don't want to push un-maintainable code... or even well-written code that we can't understand.
  - If your proposal breaks something else, we'll get blamed regardless.  
  - Subsequent new features have to take account of interactions with your feature.

- New features should be switchable with their own flag, by default off.  We used to have just one flag `-fglasgow-exts` but nowadays we try to be much more selective.

- We are much happier about features whose implementation is:   

  - **Localised**: only a handful of places in the code are changed.
  - **Orthogonal**: no new invariants or constraints are added that subsequent changes must bear in mind. This is really important; any change that imposes costs on later, apparently unrelated, changes is much more costly. 

- New features should work in a way that is consistent, as far as possible, with the way that other
  existing GHC features work.  Adopting a variety of different styles leads to a
  system that is hard to learn, and complaints of the form "why doesn't it work like X?
  I'm familiar with X!".

- Remember that GHC HQ is not heavily staffed!  It may take us a while to give your proposal the attention it deserves. However, if you hear nothing for a couple of weeks then please feel free to ping us, in case your proposal has slipped through the cracks.


If you are working on a feature that you think you think is a candidate for including in GHC's main repository, you may want to talk to us while you are developing it.  We may well, for example, have advice about how to structure the change in a way that we're happy to incorporate in our code base.
