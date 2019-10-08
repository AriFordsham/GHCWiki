This is a proposal about the workflow for getting merge requests to land in
GHC’s master branch. While it is the result of some discussion at GHC HQ, this
is currently just a proposal: We would love to have your feedback.


## Background

In the past years GHC has grown to be relied on by more and more users,
especially in commercial applications. Consequently, it is now more
important than ever that we ensure the reliability and maintainability of
the project.

For most of its history GHC has allowed a relatively large group of committers
direct commit access to the tree. With the move to GitLab we have had a
slightly more formal approval process, but nevertheless a rather large group of
contributors held the ability to add merge requests to the merge queue that
would land in the master branch.

Furthermore, many merge requests languish in code review for extended periods
of time; while this is sometimes due to the unavoidable difficulty of finding
reviewers, there are often delays due to MRs falling through the procedural
cracks. We think that a more systematic way of tracking the state of merge
requests will help avoid these preventable stalls.


## Dramatis personae

- **Author:** This is the person who contributes the patch

- **GHC contributors:** One of GHC’s greatest strengths is its large group of
  willing developers. Almost all developers are volunteers. We want to do
  everything possible to make it as frictionless as possible for GHC contributors
  to help make GHC great.

- **GHC developers** are contributors who have a “Developer role” in GitLab.
  The difference is minor: being a developer is just a permissions thing; you can
    
  - change the label on issues and MRs  
  - push a branch to the main GHC repo (but not the master or stable branches!)
    

  Any contributor who is making a MR should be a developer. (Note: in GitLab,
  only developers can edit a MR once it has been created; so to add an
  approver you must be a developer.)

- **GHC maintainers:** As GHC has become more mission critical to more
  companies, we have slowly built a small team of maintainers, whose actual day
  job is to look after GHC, its developers, and its ecosystem.
    

## The proposed new life cycle of an MR

We propose the following life cycle for an MR. A label for each MR signals
which step it is on; each MR will have exactly one such label. Each label has a
designated *owner* who is responsible for taking the next step on the MR.

  
| Stage            | Label                    | Owner                | Reasonable latency expectation        |
|------------------|--------------------------|----------------------|---------------------------------------|
| Work in progress | No label                 | Author               | N/A                                   |
| Under review     | ~"MR::1-under review"    | Author               | A week to a few months |
| Ready to merge   | ~"MR::2-ready for merge" | Maintainers          | One working day |
| In merge queue   | ~"MR::3-in merge queue"  | CI infrastructure    | One working day |
| Merged           | ~"MR::4-merged"          | Author (post-merge clean-up) | |
| Backporting      | ~"backport needed"       | Maintainer or author | A few weeks |

The steps involved are as follows:


### 1. Post a WIP Merge Request

 - **Label:** No label
 - **Owner:** Author  

To start the process off, the author creates a merge request (MR). They can do
so as early as they desire; contributors are encouraged to create MRs early to
serve as a collecting-point for early feedback. Such work-in-progress MRs
should be marked with the `WIP:` prefix in the merge request title.


### 2. Technical review

 - **Label:** ~"MR::1-under review"
 - **Owner**: Author  

When the the author feels that the patch is ready it is their responsibility to
identify an appropriate set of approvers; this is the group of
people who will be notified of the new merge request with the intention that
they will contribute to the code review performed in Step (2).
   
Reviewing is a task carried out by volunteers in their own time; they are doing
GHC and the author a favour. If you are asking for review you are strongly
encouraged to also contribute review for others. If, as an author, you are
finding it difficult to identify suitable approvers please ask for help (on the
MR's discussion thread or, that failing, the
[ghc-devs](https://mail.haskell.org/mailman/listinfo/ghc-devs) mailing list).

Anyone can contribute to the review stage! There are typically two sorts of
contributions to the review process

- Reviewers familiar with the affected areas of GHC will evaluate the concept
  and implementation of the patch, add comments, and work with the contributor to
  iterate as necessary.
    
- Reviewers less familiar with the details can still contribute by reviewing
  the checklist used in the next stage (final review). For example, is there a
  test case? Does the patch make a user-visible change?

Reviewers can use the "Approve" button to indicate they are happy with the MR.
In a long thread, this is a useful way for a well-informed reviewer to indicate
"this is good to go". Additionally, developers are encouraged to edit the MR to
add additional approvers that they think could offer input.
   
When contributor believes that the MR is ready to go -- has had an appropriate
level of review and approvers -- the contributor will add the
~"MR::2-ready for merge" label, moving the MR to the next phase of review.
(N.B. Step 3b, below, affirms that the maintainer agrees with the contributor’s
judgment).

If more than two weeks pass without comment from an MR’s approvers the author
is encouraged to leave a comment reminding the reviewers of the pending review.  
  

### 3. **Final sanity check**
 
 - **Label:** ~"MR::2-ready for merge"
 - **Person responsible:** GHC Maintainers  

Here a maintainer will have a final look at the MR:

1. If it makes a user-facing change to GHC, has a [GHC
   Proposal](https://gitlab.haskell.org/ghc/homepage/tree/master/blog) been
   accepted? If it makes a change that would affect downstream tooling, has that
   been discussed with the affected downstream users?
    
2. Has appropriate code review taken place, with appropriate reviewers?
   ("Appropriate" because fixing a spelling error in a comment requires no review
   and no approvers, whereas a pervasive change to the type system would deserve
   a lot of attention, and perhaps several approvals).
    
3. Has the documentation (GHC user manual) and core libraries’ changelogs been
   updated appropriately?
    
4. Have the milestone and labels been set appropriately?
    
5. Has the MR been squashed into a single commit (or, if appropriate, a
   sequence of independent commits), with a decent commit message? The goal is
   that the commit log that ends up in master makes sense, rather than reflecting
   the history of the MR’s development.
    
6. Does the MR regress performance (of the compiler, or of compiled code)
   significantly?
    
7. Does it pass the Final MR Sanity Check? See the eponymous section below.

The final review is *not a complete code review*: it’s a quick process review.
Think of it as a human version of the CI testing that every MR must pass.  It
should take the maintainer no more than 10 or 15 minutes and should very rarely
be a show-stopper. See the "Final MR Sanity check details" section at the end
of this document for details on what this review entails.

The final review should not impose significant delay: within one
working day a maintainer should either put it in the merge queue, or else push
it back into the "Techincal Review" stage with a comment.  Such a push-back
does not imply that the MR is bad; only that for some reason it is not quite
ready, and inviting the author to address the concern.

Finally the maintainer will add the MR to the merge queue (currently Marge) and
add the ~"MR::3-in merge queue" label.


### 4. **In merge queue**

 - **Label:** ~"MR::3-in merge queue"
 - **Person responsible:** @marge-bot, or maintainers if things get stuck  

After being merged the MR will be assigned the ~"MR::4-merged" label. Ideally,
adding this label will be part of Marge’s remit. (After Marge is retired
in favor of GitLab’s official solution, update accordingly.)  


### 5. **Post-merge cleanup**

 - **Label:** ~"MR::4-merged"
 - **Person responsible:** Author  

After the MR has been merged, it is the author’s responsibility to have a final
look over the MR to deal with any related issues. For example

1. If MR !R is a fix for issue #N, then when !R is merged, the author should go
   back to #N (which should by now be decorated with R's commit message), open MRs
   adding any additional regression tests, and close.

2. Or, perhaps when closing #N, that unblock other tickets, which the author
   might want to look at.
    
3. Propose the MR for backporting to the stable branch by leaving a comment
   mentioning the `@ghc/release` group and assigning the MR to that group.
    

When this step is done, change the label to ~"MR:5-completed" and close the
MR.


### 6. **Backporting**

 - **Label:** ~"backport needed"
 - **Person responsible:** Maintainer or contributor

If desired, the contributor can request that the release manager backports a
patch to the stable branch. If the release manager agrees that backporting is
appropriate then the ~"backport needed" label will be applied until a
backporting merge request is opened (which may be done by either the maintainer
or the patch contributor). Once such an MR is created the ~"backport needed"
label can be removed.


## FAQ

- **How can an author know when his/her MR moves to "Post merge cleanup"?**

  Easy: here is a URL that searches for MRs that have the "post-merge cleanup"
  label and "Fred" as the author: show url
    
- **Do I need to change Assignee as well as the Label on a MR?**

  No: changing the label is enough.
    
- **What about back-porting to the most recent release branch. Who decides? Who
  is responsible?**

  The author is responsible for raising their patch with the release manager
  (`@ghc/release`)  
    

## Conclusion

The new process isn’t so different from the status quo. In practice small
patches will likely have all three stages of review performed by the same
maintainer. However, the explicit nature of the process makes it easier to
ensure that reviews don’t fall through the cracks, as has happened in the past.
In particular, the ~"MR::2-ready for merge" label make it clear what things
are waiting for intervention from maintainers. Moreover, the final review phase
ensures that we have a clearly identified group responsible for overseeing what
is merged to the tree.

Cheers,

> Ben


# Appendix

## Final MR sanity check details

Every MR that lands in GHC adds a permanent joy or burden to subsequent
developers. Moreover, for very good reasons, contributors come and go, so it’s
essential that every MR is comprehensible and maintainable to people other than
its author, far into the future.

In the end, the maintainers get to pick up the pieces, so it makes sense to get
them to do a final sanity check. Otherwise it would be possible for a couple of
enthusiastic and well-meaning developers to get stuff into GHC, perhaps barely
noticed by others, that gives us later pain. The criteria here are deliberately
vague. The final-review step just give the maintainers an opportunity to start
a conversation about any aspect of a MR that gives them cause for concern.

The final review checks will include:

 * Are the commits logically structure? Are their commit messages descriptive?
 * Are ticket numbers referenced as appropriate?
 * Is a GHC release notes entry included (e.g. `docs/users_guide/*-notes.rst`)?
 * Have changelog entries been added to any changed packages (e.g.
   `libraries/*/changelog.md`)?
 * Has a test case been added?
 * Milestone and labels set as appropriate
 * Does the patch add a significant new user-facing feature to GHC? If so
   perhaps a GHC proposal is in order.
 * Does the patch change GHC's core libraries (e.g. `base`, `template-haskell`,
   `ghc-prim`)? If so:
   * Has the core libraries committee consented?
   * Has the user-facing label been applied?
   * Has the `head.hackage` job been run to characterise the effect of the
     change on user code?
   * Changelog and release notes entries are mandatory
   * Have package versions been bumped as appropriate?
   * Has an entry been added to the next release's migration guide? If not, ask
     the author to do so.

