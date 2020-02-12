# GHC working conventions

These pages document [GHC-specific working conventions](https://gitlab.haskell.org/ghc/ghc/wikis/contributing)

* [GHCI wiki index](https://gitlab.haskell.org/ghc/ghc/wikis/index)
* [GHC label summary](https://gitlab.haskell.org/ghc/ghc/wikis/gitlab/labels)
* [GHC issue conventions](https://gitlab.haskell.org/ghc/ghc/wikis/gitlab/issues)
* [GHC merge request conventions](https://gitlab.haskell.org/ghc/ghc/wikis/gitlab/merge-requests)
* [Simon's GitLabl wish-list](https://docs.google.com/document/d/1sdGlDJSTiBZSH6kBU5pyn1HASE9XFqQhzILcYcH1QJQ/edit?usp=sharing)

# GitLab's model

Here's a summary of the things in GitLab's universe, what they mean, and how they relate to each other.

- **Wiki**. GHC's wiki is [rooted here](https://gitlab.haskell.org/ghc/ghc/wikis), and we mainain [an index of the wiki structure](https://gitlab.haskell.org/ghc/ghc/wikis/index).

- **Groups.**  Example, the [GHC group](https://gitlab.haskell.org/ghc)

  - A group contains:

    - *Members*, each of whom is a *user*.
    - *Projects*
    - *Sub-groups*
  - As "sub-groups" suggests, groups can nest.   For example [GHC packages group](https://gitlab.haskell.org/ghc/packages) is a sub-group of the [GHC group](https://gitlab.haskell.org/ghc)

- **Projects.** Example: the  [GHC project](https://gitlab.haskell.org/ghc/ghc).  A *project* has

  - A *git repository*
  - A *wiki*
  - An *issue tracker*
  - An *owner*, who is either a group or a user. **Question**: exactly one owner?
    A project can accept merge requests.

- **Users.**

  - A user can be a *member* of (a) groups and (b) projects.
  - There are a few flavours of membership, called "roles".  These include "reporter", "developer", "maintainer", and "owner"; which all imply different sets of permissions, [documented here](https://docs.gitlab.com/ee/user/permissions.html). 

- A **merge request** (MR) is a set of one or more commits that you want to have merged into master.

  - Every MR has an associated **git branch**.  It is this branch that will be ultimately merged into master.
  - The branch may have multiple commits; these are all merged individually onto master.

- A **review** is a set of comments on a MR.  Always comment on MRs using a review, else the recipients get one email per comment, which is terrible.

- An **issue** is GitLab's name for a Trac "ticket".

- A **label** is GitLab's name for a Trac "keyword".  Here is [GHC's list of labels](https://gitlab.haskell.org/ghc/ghc/wikis/GitLab-Labels/).

# Workflows

## Making a change to the master repository


You never commit directly to HEAD.  Rather, follow this workflow.

- Create a branch, e.g. `wip/spj-wibbles` or `wip/T16224`:

  ```wiki
  $ git checkout -b wip/spj-wibbles
  ```

- Push it to the main repo (or your private repo)

  ```wiki
  $ git push -u origin wip/spj-wibbles
  ```

  The `-u` flag arranges that your private branch will now track the remote branch.

- Create a merge request.  [More details on this process are here](https://gitlab.haskell.org/ghc/ghc/wikis/home#opening-a-merge-request).

- Working with your merge request: [details here](https://gitlab.haskell.org/ghc/ghc/wikis/home#working-with-your-merge-request).

- As you review it, you can add further patches to the branch, and push them. The MR as seen in GitLab is always the tip of the branch.

- You can also rebase the patch on HEAD; then you need to force-push the branch

  ```wiki
  $ git push --force origin wip/spj-wibbles
  ```

- Before trying to commit to master, you should tidy up the MR by squashing it into one (or more in unusual cases) patch, with a good commit message.

- When you are ready to commit to master, you do this by **assigning to Marge**.  Go to "Edit" the MR, and set the "Assignee" to "Marge".   That's all you have to do.  Marge will rebase you patch on master, validate, and commit.

  In particular, **do not click the "Merge if passes tests" button**. That bypasses Marge and confuses her.

# GitLab notes

## Issues

- **To see what merge request is open for this issue** look down the status panels to one that says "Related Merge Request(s)" ([example](https://gitlab.haskell.org/ghc/ghc/issues/16344)).  This list should include the MR that fixes the issue, if indeed there is such a MR.

## Merge requests

- **To see all merge requests**, click on "Merge requests" *in the left-hand navigation panel*.  The similar icon in the black menu bar at the top doesn't seem to do anything useful.

- **The title and description of a MR** do not form part of the Git repo's history.  Only the commit messages in the patches do.  So make sure that each patch has a good commit message!  The title and description of the MR signpost the readers through the review process.

- **The status of a MR** is in the panels near the top.  

  * *Relevant issue*. To see what ticket this MR is for, look down the status boxes to find one with "Mentions" in it (the panel is probably about the merge status of the MR).  The parent ticket should appear in that list.

  * If it says *"Fast-forward merge is not possible. To merge this request, first rebase locally."* then Marge can't rebase your commits on top of master because of a conflict.  It's up to you to rebase and force-push the branch, after which Marge will have another go.  (NB: It's up to you *even though the "assignee" is still Marge -- this is a bug.)

  * But you should ignore *"Fast-forward merge is not possible. Rebase the source branch onto the target branch or merge target branch into source branch to allow this merge request to be merged."*  (Note the subtle difference in wording.)  In this case Marge is still in control and will do the rebase for you.

- **Work in progress Merge Requests**.  A MR can be a "work in progress" (WIP) MR.

  - WIP MRs are identified simply by having a title beginning "WIP:" or "\[WIP\]".
  - The "merge" button is disabled for WIP MRs, so they can't be accidentally merged.
    Details [here](https://docs.gitlab.com/ee/user/project/merge_requests/work_in_progress_merge_requests.html)

- **Approvers**. A MR requires at least one approver to press the "Approve" button before a MR will be merged.   The approvers for a MR are drawn from three sources:

  - A small per-project list of super-developers.
  - The [code owners file](https://gitlab.haskell.org/ghc/ghc/blob/master/CODEOWNERS): if your patch touches code listed in this file, the corresponding users become approvers for that MR.
  - Per-MR approvers, which you as MR author can add.  You can add or remove approvers by clicking on the "Edit" button of the MR.  (You can't do this via the sidebar on the right.  And you need to be a GitLab "developer", which anyone GHC contributor can ask to be.)

## Gitlab tips

- **Search** is rather confusing. If you type a search into the prominent "Search of jump to.." box in the black menu bar at the top, you'll see a list of results, but it's easy to miss the fact that **there are a bunch of separate non-prominent tabs**, one for code, one for issues, one for comments etc.

  If you are search issues you must look _both_ under "Issues" _and_ under "Comments".

  There does not seem to be any way to change the sort order of the results. I don't know what the sort order actually is.

- **Markup**. In GitLab markup:

  - `#3553` links to issue 3553

  - `!883` links to merge request 883

  - "@fred" mentions Fred.  You don't need to remember their user-id; you can just start typing their real name and auto-complete stuff will pop up.  The main reason to mention Fred is to ensure that he sees your comment.  For example you might asking him for a review.

  - In bulleted or itemized lists, if you want a new paragraph, indent two spaces.  

    Less obviously, if you want to put a code block _indent the triple-backticks two spaces_.  If you don't indent the triple-backticks, you terminate the current list.

  The [full markdown guide](https://about.gitlab.com/handbook/product/technical-writing/markdown-guide/).

- **Zooming**.  If you zoom your browser too much, stuff starts disappearing from the top menu bar; e.g. your picture and the settings linked to it. You have to zoom out.  This happens less if you switch to "fluid" layout in [your preferences](https://gitlab.haskell.org/profile/preferences).

- **Notifications**.  
  * For projects you care about (e.g. [https://gitlab.haskell.org/ghc/ghc](https://gitlab.haskell.org/ghc/ghc)), click on "Watch" (there's a little bell icon) about four rows down from the top. Then you'll get notifications of what changes.
  * By default GitLab does not send you mail about your own activity, e.g. contributions to the Discussion of an issue.  I'm an email-driven person, so I find that confusing. You can switch this on in your settings: click on the down-arrow next to your picture in the top right-hand corner, and then pick "Notifications" in the left-hand column.

- **From branch name to merge request**.  Say you are on branch `wip/T16185`, and you know you (or someone) has submitted a MR based on that branch. How can you find the MR?

  - Go to the [GHC project page](https://gitlab.haskell.org/ghc/ghc)
  - Select the branch (drop-down menu box near the top left; initially labelled *master*).
  - In the left-hand-column menu bar, from "Repository" select "Commits"
  - Click on the title of the top-most commit

    You should then see something like "1 merge request!128 WIP: Add an AnonArgFlag to FunTy" near the top, if there is an active MR from this branch.

>
> Actually, git reports this when you push the branch too, if you remember to look.  E.g. 
>
> ```wiki
> bash$ git push --force
> Counting objects: 80, done.
> Delta compression using up to 32 threads.
> Compressing objects: 100% (80/80), done.
> Writing objects: 100% (80/80), 25.29 KiB | 0 bytes/s, done.
> Total 80 (delta 75), reused 0 (delta 0)
> remote: 
> remote: View merge request for wip/T15952-2:     <-- Here --------
> remote:   https://gitlab.haskell.org/ghc/ghc/merge_requests/206        
> remote: 
> To git@gitlab.haskell.org:ghc/ghc
>  + 5f84b0c...5c1f268 HEAD -> wip/T15952-2 (forced update)
> ```

## Reviewing a MR

- Click on the Changes tab in the MR

- I recommend clicking on "Side-by-side" to get a better rendering.

- You also may want to go to your preferences page (accessible via the Settings button that appears under your picture at the very top-right of the screen -- Preferences is fourth from the bottom in the vertical navbar to the left of your settings screen) and choose a Fluid layout width.

- Back on the "Changes" tab of the MR above, you'll see a button labeled "0/49 discussions resolved". There is a small button with an arrow in it to the right of that. It warps you to the first unresolved discussion (i.e. comment I've made during my review)

- To see more code surrounding a diff, there are some light grey "..." icons at the top and bottom of the line-number column. Click to show more.

- Comment if you like. Then, at the bottom-right of the windowette that contains the discussion, you'll see another button with a right-pointing arrow inside a speech bubble. Click that to go to the next unresolved discussion. In this way, the unresolved discussions can be navigated like a singly linked list.

- The table-of-contents on the left is useful, but it takes up a lot of visual space. To hide it, press the button with the three horizontal lines to the left of the words "Changes between", right below the tabs where you choose between, e.g., Discussion and Changes.

- When commenting on a MR, use "Start review", not "Add comment".  The latter sends an email for each comment.  But a "review" is a set of comments, and the recipients get one email for the whole thing.

## GitLab "quick actions"


Quick actions are textual shortcuts for common actions on issues, epics, merge requests, and commits that are usually done by clicking buttons or dropdowns in GitLab's UI.

- You can enter these commands while creating a new issue or merge request, or in comments of issues, epics, merge requests, and commits.
- Each command should be on a separate line in order to be properly detected and executed.
- Once executed, the commands are removed from the text body and not visible to anyone else.


There's a [full list here](https://docs.gitlab.com/ee/user/project/quick_actions.html).

## Transitioning an old repo

```wiki
$ git remote set-url origin https://gitlab.haskell.org/ghc/ghc.git
$ git remote set-url --push origin git@gitlab.haskell.org:ghc/ghc
$ git submodule sync
```
