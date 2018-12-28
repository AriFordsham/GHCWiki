This is GHC's GitLab instance. GitLab will replace Trac and Phabricator as the center of GHC's development community.

# Logging in

If you had a Trac account then you likely already have a GitLab account as a result of the Trac import. This account will have a username of the form `trac-$TRAC_USERNAME`. Contact @bgamari with your email address to have a password reset email sent. Once you have gained access to this account you may change your username to remove the `trac-` prefix

If you do not have a Trac account (or would prefer to start from scratch with a new account) you can [log in](https://gitlab.staging.haskell.org/users/auth/github) using your GitHub account or [create](https://gitlab.staging.haskell.org/users/sign_in) a new GitLab account.

# Merge requests

Submitting a patch for incorporation into the tree is done by creating a *merge request*. The merge request serves as a place to conduct code review, collect continuous integration logs, and 

## Opening a merge request

To open a merge request:

1. Push your branch. You may push either to your fork or the primary `ghc/ghc` project.
2. Starting from the [GHC GitLab project](https://gitlab.staging.haskell.org/ghc/ghc) click on the *Merge Requests* link in the left navigational bar.
3. Click on the green *New Merge Request* button on the top right corner of the Merge Requests page
4. In the left drop-down of the *Source branch* pane select the project to which you pushed your branch.
5. In the right drop-down of the *Source branch* pane select the name of your branch.
6. Click on the green *Compare branches and continue* button.
7. Give your merge request a title
8. Write a description of your change. This should be ideally at very least a few sentences to help reviewers understand what you have done.
9. Click on the green *Submit merge request* button.

## Working with your merge request

Your merge request shows you several panes information:

Top-most is the request's title and description. These can be edited by pressing the yellow `Edit` button on the top-right corner of the page.

The next pane summarises your merge request, showing the the source and target branches.

![request-summary](uploads/a6b259498530ea48964a11c2539447d2/request-summary.png)

The next pane shows the status of the merge request's continuous integration builds. If the build has failed you can click on the red *X* icon to see the build log.

![pipeline](uploads/3e1bf21a870eef5608665f9d6e6a48de/pipeline.png)

At the bottom of the page is the code review interface, consisting of several tabs:

![code-review](uploads/f90d8a2f0a3716669546ae1fd43480c1/code-review.png)

The *Discussion* tab shows a summary of the comments left on the change. This includes comments left in-line in the code, as well as those independent of the code. 

The *Commits* tab lists the commits being proposed for merge. These may be clicked upon to restrict the diff to only changes made by that commit. 

The *Pipelines* tab provides a more detailed overview on the state of the various continuous integration jobs associated with the merge request.

Finally, the *Changes* tab shows the patch itself. This view may be restricted to changes made by a single commit by selecting the commit in the *Commits* tab. Moreover, one may view previous iterations of the merge request using the two drop-down menus at the top of the tab. To leave an inline comment click on a line in the patch.

# Tickets

## Migration from Trac

All tickets, their comments, and attachments previously present on [Trac](https://ghc.haskell.org/trac/ghc/) have been migrated to GitLab. During this import authorship, formatting, and insofar as is possible, metadata was preserved. Some Trac metadata fields have no analogue in GitLab. Changes in these fields are represented in GitLab by collapsed-by-default *Trac metadata* blocks.

In particular, fields have been translated as follows:

| Trac field       | GitLab representation |
| :--------------- | :-------------------- |
| Title            | Ticket title          |
| Description      | Ticket description    |
| Reported by      | Ticket author         |
| Owned by         | Ticket owner          |
| Priority         | Ticket weight         |
| Milestone        | [Milestone](https://gitlab.staging.haskell.org/ghc/ghc/milestones) |
| Component        | Metadata block        |
| Version          | Metadata block        |
| Keywords         | Tags (selectively)    |
| Cc               | Metadata block        |
| Operating System | Metadata block        |
| Architecture     | Metadata block        |
| Type of failure  | Metadata block        |
| Test Case        | Metadata block        |
| Blocked by       | Metadata block        |
| Blocking         | Metadata block        |
| Related tickets  | Related tickets       |
| Differential Revs | Metadata block       |
| Wiki page         | Metadata block       |
| Attachments       | Either attachments or [Snippets](https://gitlab.staging.haskell.org/ghc/ghc/snippets) |
| State             | Ticket state and tags |
