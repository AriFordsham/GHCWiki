# The big picture

- A **merge request** (MR) a set of changes that you want to have merged into HEAD.

- Every MR has an associated **GIT branch**.  It is this branch that will be ultimately merged into HEAD.

- An **issue** is GitLab's name for a Trac ticket.

- A **review** is a set of comments on a MR.  Always comment on MRs using a review, else the recipients get one email per comment, which is terrible.

# Workflows

## Making a change to HEAD


You never commit directly to HEAD.  Rather, follow this workflow.

- Create a branch, e.g. `wip/spj-wibbles` or `wip/T16224`:

  ```wiki
  $ git checkout -b wip/spj-wibbles
  ```

- Push it to the main repo (or your private repo)

  ```wiki
  $ git push origin wip/spj-wibbles
  ```

- Create a merge request.  [ More details on this process are here](https://gitlab.haskell.org/ghc/ghc/wikis/home#opening-a-merge-request).

- Working with your merge request: [ details here](https://gitlab.haskell.org/ghc/ghc/wikis/home#working-with-your-merge-request).

- As you review it, you can add further patches to the branch, and push them. The MR as seen in GitLab is always the tip of the branch.

- You can also rebase the patch on HEAD; then you need to force-push the branch

  ```wiki
  $ git push --force origin wip/spj-wibbles
  ```

- Before trying to commit to HEAD, you should tidy up the MR by squashing it into one (or more in unusual cases) patch, with a good commit message.

- Someone with commit right can then click "merge" on that MR.  Even then it has to pass CI before it lands in HEAD.

# GitLab model


This section summarises what I understand about GitLab's semantics: what things there are and how they relate to each other.

- **Groups.**  Example, the [ GHC group](https://gitlab.haskell.org/ghc)

  - A group contains:

    - *Members*, each of whom is a *user*.
    - *Projects*
    - *Sub-groups*
  - As "sub-groups" suggests, groups can nest.   For example [ GHC packages group](https://gitlab.haskell.org/ghc/packages) is a sub-group of the [ GHC group](https://gitlab.haskell.org/ghc)

- **Projects.** Example: the  [ GHC project](https://gitlab.haskell.org/ghc/ghc).  A *project* has

  - A *git repository*
  - A *wiki*
  - An *issue tracker*
  - An *owner*, who is either a group or a user. **Question**: exactly one owner?
    A project can accept merge requests.

- **Users.**

  - A user can be a *member* of (a) groups and (b) projects.
  - There are a few flavours of membership ("reporter", "developer", "maintainer", and "owner") which all imply different sets of permissions, [ documented here](https://docs.gitlab.com/ee/user/permissions.html). 

# GitLab notes

## Gitlab tips

- In GitLab markup:

  - `#3553` links to issue 3553
  - `!883` links to merge request 883

- If you zoom your browser too much, stuff starts disappearing from the top menu bar; e.g. your picture and the settings linked to it. You have to zoom out.

- For projects you care about (e.g. [ https://gitlab.haskell.org/ghc/ghc](https://gitlab.haskell.org/ghc/ghc)), click on "Watch" (there's a little bell icon) about four rows down from the top. Then you'll get notifications of what changes.

## Merge requests

- The title and description of a MR do not form part of the Git repo's history.  Only the commit messages in the patches do.  So make sure that each patch has a good commit message!  The title and description of the MR signpost the readers through the review process.

- When commenting on a MR, use "Start review", not "Add comment".  The latter sends an email for each comment.  But a "review" is a set of comments, and the recipients get one email for the whole thing.

- To see all merge requests, click on "Merge requests" *in the left-hand nav column*.  The similar icon in the black menu bar at the top doesn't seem to do anything useful.

- To see more code surrounding a diff, there are some light grey "..." icons at the top and bottom of the line-number column. Click to show more.

## Reviewing comments

- Click on the Changes tab in the MR

- I recommend clicking on "Side-by-side" to get a better rendering.

- You also may want to go to your preferences page (accessible via the Settings button that appears under your picture at the very top-right of the screen -- Preferences is fourth from the bottom in the vertical navbar to the left of your settings screen) and choose a Fluid layout width.

- Back on the "Changes" tab of the MR above, you'll see a button labeled "0/49 discussions resolved". There is a small button with an arrow in it to the right of that. It warps you to the first unresolved discussion (i.e. comment I've made during my review)

- Comment if you like. Then, at the bottom-right of the windowette that contains the discussion, you'll see another button with a right-pointing arrow inside a speech bubble. Click that to go to the next unresolved discussion. In this way, the unresolved discussions can be navigated like a singly linked list.

- The table-of-contents on the left is useful, but it takes up a lot of visual space. To hide it, press the button with the three horizontal lines to the left of the words "Changes between", right below the tabs where you choose between, e.g., Discussion and Changes.

## Transitioning an old repo

```wiki
$ git remote set-url origin https://gitlab.haskell.org/ghc/ghc.git
$ git remote set-url --push origin git@gitlab.haskell.org:ghc/ghc
$ git submodule sync
```