# Subject: Commit access and MR approval policies

  

Hello GHCers,

This is a proposal about the workflow for getting merge requests to land in GHC’s master branch. While it is the result of some discussion at GHC HQ, this is currently just a proposal: We would love to have your feedback.

## Background

In the past years GHC has grown to be relied on by more and more users, especially in commercial applications. Consequently, it is now more important than ever that we ensure the reliability and maintainability of the project.

  

For most of its history GHC has allowed a relatively large group of committers direct commit access to the tree. With the move to GitLab we have had a slightly more formal approval process, but nevertheless a rather large group of contributors held the ability to add merge requests to the merge queue that would land in the master branch.

  

Furthermore, many merge requests languish in code review for extended periods of time; while this is sometimes due to the unavoidable difficulty of finding reviewers, there are often delays due to MRs falling through the procedural cracks. We think that a more systematic way of tracking the state of merge requests will help avoid these preventable stalls.

## Dramatis personae

- **Author:** This is the person who contributes the patch

- **GHC contributors:** One of GHC’s greatest strengths is its large group of willing developers. Almost all developers are volunteers. We want to do everything possible to make it as frictionless as possible for GHC contributors to help make GHC great.

- **GHC developers** are contributors who have a “Developer role” in GitLab. The difference is minor: being a developer is just a permissions thing; you can
    
  - change the label on issues and MRs  
  - push a branch to the main GHC repo (but not the master or stable branches!)
    

  Any contributor who is making a MR should be a developer. (Note: in GitLab, only developers can edit a MR once it has been created; so to add an approver you must be a developer.)

- **GHC maintainers:** As GHC has become more mission critical to more companies, we have slowly built a small team of maintainers, whose actual day job is to look after GHC, its developers, and its ecosystem.
    

## The proposed new life cycle of an MR

We propose the following life cycle for an MR.. A label for each MR signals which step it is on; each MR will have exactly one such label. Each label has a designated *owner* who is responsible for taking the next step on the MR.

  
|Stage label       | Owner       | Reasonable latency expectation        |
|------------------|-------------|---------------------------------------|
| Work in progress | Author      | N/A                                   |
| Under review     | Author      | < Two weeks until beginning of review |
| Ready to merge   | Maintainers | One working day |
| In merge queue   | CI infrastructure | One working day |
| Merged           | Author (post-merge clean-up) | |


  

  

The steps involved are as follows:

## 1. Post a Merge Request
    
**Owner:** Author  

To start the process off, the author creates a merge request. When doing so, the author chooses appropriate approvers; this is the group of people who will be notified of the new merge request with the intention that they will contribute to the code review performed in Step (2).
   
It’s primarily up to the author to identify such approvers/reviewers. Reviewing is a task carried out by volunteers in their own time; they are doing GHC and the author a favour. If you are asking for review you are strongly encouraged to also contribute review for others. If, as an author, you are finding it difficult to identify suitable approvers please ask for help (on the MR’s discussion thread or, that failing, the ghc-devs mailing list).

Authors are free to open merge requests for in-progress work which is not yet ready for review; however, such merge requests should be marked with the `WIP:` prefix in the merge request title.

## 2. Technical review
**Label:** `~”MR::1-under review”`
**Owner**: Author  

Anyone can contribute to the review stage! There are typically two sorts of contributions

-   Reviewers familiar with the affected areas of GHC will evaluate the concept and implementation of the patch, add comments, and work with the contributor to iterate as necessary.
    
-   Reviewers less familiar with the details can still contribute by reviewing the checklist used in the next stage (final review). For example, is there a test case? Does the patch make a user-visible change?

Reviewers can use the “Approve” button to indicate they are happy with the MR. In a long thread, this is a useful way for a well-informed reviewer to indicate “this is good to go”. Additionally, developers are encouraged to edit the MR to add additional approvers that they think could offer input.
   
When contributor believes that the MR is ready to go -- has had an appropriate level of review and approvers -- the contributor will add the ~”MR::2-ready for merge” label, moving the MR to the next phase of review. (N.B. Step 3b, below, affirms that the maintainer agrees with the contributor’s judgment.)

If more than two weeks pass without comment from an MR’s approvers the author is encouraged to leave a comment reminding the reviewers of the pending review.  
  

## 3.  **Final review**
 
**Label:** `~”MR::2-ready for merge”`
**Person responsible:** GHC Maintainers  

Here a maintainer will have a final look at the MR:

1.  If it makes a user-facing change to GHC, has a [GHC Proposal](https://gitlab.haskell.org/ghc/homepage/tree/master/blog) been accepted? If it makes a change that would affect downstream tooling, has that been discussed with the affected downstream users?
    
2.  Has appropriate code review taken place, with appropriate reviewers? (“Appropriate” because fixing a spelling error in a comment requires no review and no approvers, whereas a pervasive change to the type system would deserve a lot of attention, and perhaps several approvals).
    
3.  Has the documentation (GHC user manual) and core libraries’ changelogs been updated appropriately?
    
4.  Have the milestone and labels been set appropriately?
    
5.  Has the MR been squashed into a single commit (or, if appropriate, a sequence of independent commits), with a decent commit message? The goal is that the commit log that ends up in master makes sense, rather than reflecting the history of the MR’s development.
    
6.  Does the MR regress performance (of the compiler, or of compiled code) significantly?
    
7.  Does it pass the Final MR Sanity Check? See the eponymous section below.

The final review is not a complete code review: it’s a quick process review. Think of it as a human version of the CI testing that every MR must pass. Moreover, it should not impose significant delay: within one working day a maintainer should either put it in the merge queue, or else push back explicitly with a comment.

Finally the maintainer will add the MR to the merge queue (currently Marge) and add the ~”MR::3-in merge queue” label.

## 4.  In merge queue.  

**Label:** `~”MR::3-in merge queue”`
**Person responsible:** Marge Bot, or maintainers if things get stuck  

After being merged the MR will be assigned the `~”MR::4-merged”` label. Ideally, adding this label will be part of Marge’s remit. (After Marge is retired in favor of GitLab’s official solution, update accordingly.)  

## 5.  Post-merge cleanup:  

**Label:** `~”MR::4-merged”`
**Person responsible:** Author  

After the MR has been merged, it is the author’s responsibility to have a final look over the MR to deal with any related issues. For example
    

1.  If MR !R is a fix for issue #N, then when !R is merged, the author should go back to #N (which should by now be decorated with R’s commit message), open MRs adding any additional regression tests, and close.
    
2.  Or, perhaps when closing #N, that unblock other tickets, which the author might want to look at.
    
3.  Propose the MR for backporting to the stable branch by leaving a comment mentioning the `@ghc/release`.
    

When this step is done, change the label to `~”MR:5-completed”` and close the MR.

  

6.  Backporting:
    

Label: `~”backport needed”`

Person responsible: Maintainer or contributor

If desired, the contributor can request that the release manager backports a patch to the stable branch. If the release manager agrees that backporting is appropriate then the `~”backport needed”` label will be applied until a backporting merge request is opened (which may be done by either the maintainer or the patch contributor). Once such an MR is created the `~”backport needed”` label can be removed.

## The Final MR Sanity Check

Every MR that lands in GHC adds a permanent joy or burden to subsequent developers. Moreover, for very good reasons, contributors come and go, so it’s essential that every MR is comprehensible and maintainable to people other than its author, far into the future.

In the end, the maintainers get to pick up the pieces, so it makes sense to get them to do a final sanity check. Otherwise it would be possible for a couple of enthusiastic and well-meaning developers to get stuff into GHC, perhaps barely noticed by others, that gives us later pain. The criteria here are deliberately vague. The final-review step just give the maintainers an opportunity to start a conversation about any aspect of a MR that gives them cause for concern.

It would be very unusual for the final sanity check to prove to be a show stopper.

## FAQ

- **How can an author know when his/her MR moves to “Post merge cleanup”?**
  Easy: here is a URL that searches for MRs that have the “post-merge cleanup” label and “Fred” as the author: show url
    
- **Do I need to change Assignee as well as the Label on a MR?**
 No: changing the label is enough.
    
- **What about back-porting to the most recent release branch. Who decides? Who is responsible?**
  The author is responsible for raising their patch with the release manager (`@ghc/release`)  
    

## Conclusion

The new process isn’t so different from the status quo. In practice small patches will likely have all three stages of review performed by the same maintainer. However, the explicit nature of the process makes it easier to ensure that reviews don’t fall through the cracks, as has happened in the past. In particular, the `~”MR::2-ready for merge”` label make it clear what things are waiting for intervention from maintainers. Moreover, the final review phase ensures that we have a clearly identified group responsible for overseeing what is merged to the tree.

Cheers,
-   Ben



## Maintainer responsibilities

-   Once a day check for issues with the `~”needs triage”` label. For each issue follow the [ticket triage protocol](https://gitlab.haskell.org/ghc/ghc/wikis/gitlab/issues#triage-protocol)
    
-   Once a day check for merge requests in `~“MR::1-under review”` state but lacking reviewers.
    
-   Once a day check for merge requests with the `~”MR::2-ready for merge”` label: For each MR follow the final review protocol

### Final review protocol

Final review is to be performed by a maintainer and represents the last set of checks which a merge request will be subject to before it is merged. This includes:

1.  Verify that the MR’s milestone is set to the release in which the change will first appear
2.  Verify that the MR’s labels are set appropriately
3.  Verify that the change is adequately documented in the users’ guide
4.  Verify that the change is mentioned in the target release’s release notes, if appropriate
5.  Verify that the changelogs of any affected core libraries mention the change

Finally, the MR can be added to the merge queue by assigning to `@marge-bot`

## Release manager responsibilities

 - Respond to backport requests from contributors by:
   - If backporting is appropriate then set the backported MR's milestone appropriately and apply the `~"backport needed"` label
   - If backporting is not appropriate then explain this to the author.




