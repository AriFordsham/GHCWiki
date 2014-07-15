# Using Phabricator for GHC development

[ Phabricator](http://phabricator.org) (AKA "Phab") is a suite of tools for software development, initially developed by Facebook. It consists of a suite of applications which are useful for the entire software development lifecycle, but for GHC specifically, **Phabricator is a code review tool**, and we use it to read and accept patches from contributors - both new ones, and existing ones.

**Note**: Phabricator is *not* required for GHC committers. If you have commit access, you're free to push patches directly. But do feel free to post reviews for things you're working on - it generally will only help your work. Benefits include:

- Better tools: side-by-side diffs, inline comments, blocking states, command line access
- Better notifications: use "Herald" to control emails very closely, or commits or reviews you are interested in.
- Increased visibility for code review and feedback.
- **NEW**: Automatic `./validate` of all your diffs when using Arcanist! (see "Harbormaster" below)
- Meme support (optional)
- Other features you might like

## Signing up


First off, sign up by going to the Phab instance at Haskell.org: [ https://phabricator.haskell.org/](https://phabricator.haskell.org/)


Next, click the power button in the top right corner. You'll be taken to a login screen where you can register a new account. You have several options, including a username/password, or using OAuth to login with GitHub, Facebook, or more.


Afterwords Phab will send you a verification email, which you'll need to open and read - so use a correct email address!

## Understanding the interface


Phabricator is actually a suite of applications - there are a few that are the most important for GHC development, but many auxiliary ones in use by other projects. In general, **the most important tool is Differential, located on the top of the left-side panel**. The other tools you may want are **Herald**, **Audit**, and **Diffusion**.


Finally, you'll also *really* want the command line tool, **Arcanist**. It's not strictly necessary, but makes code review and many other tasks significantly faster and easier.

## The CLI: Arcanist


The most important tool you're going to use with Phab is called **arcanist**, which is the command line interface to Phabricator. Arcanist is a tool written in PHP, and comes from a git repository - so you'll need PHP installed on your machine, as well as the source checked out.


Getting the source is very easy, just check out two git repositories next to each other, and put the binaries in your `$PATH`:

```wiki
$ git clone https://github.com/phacility/libphutil.git
$ git clone https://github.com/phacility/arcanist.git
$ export PATH="$(pwd)/arcanist/bin:$PATH"
```


That's it! **This will even work on Windows, as long as you have PHP available in your shell**.


Next, you're going to need to install a user certificate, so that `arc` can authenticate as you. First, go to your GHC repository, then run `arc install-certificate`. You need to be in the GHC repository so `arc` knows what URL to authenticate against:

```wiki
$ cd ~/code/ghc-head
$ arc install-certificate
```


Follow the directions it specifies - afterwards, your arcanist tool will be properly authenticated! Now you can submit reviews, paste things, etc etc.

*Note*: once the certificate is installed, it will be written to `$HOME/.arcrc`, so it doesn't need to be installed again.

**Question**: How does `arc install-certificate` know what URL to use?
**Answer**: It's located in a file called `.arcconfig` in the GHC repository. This is why you have to `cd` there first.

## Starting off: Fixing a bug, submitting a review


(This assumes you have installed Arcanist).


First off, let's say you have a bug you want to fix. Go to the Trac page for the bug, and make yourself as the owner.


Next, checkout a branch for the bug, and work on it:

```wiki
$ cd ~/code/ghc-head
$ git checkout -b fix-trac-1234
$ emacs ...
$ git commit -asm "compiler: fix trac issue #1234"
```


Once you're ready to submit it for review, it's easy! Just run:

```wiki
$ arc diff
```

`arc` will then respond with the revision number and a URL where you can visit your change.


Next, add a link to the revision in the Trac ticket. Fill out the field called "Differential Revisions" when you modify the ticket. You can hyperlink to any Phabricator revision using the syntax `Phab:Dxx` with a specific number. For example, to link to Differential Revision D69, say [ Phab:D69](https://phabricator.haskell.org/D69). As an example, Ticket [\#8634](https://gitlab.haskell.org//ghc/ghc/issues/8634) has this set:

[](https://i.imgur.com/gYHkAhe.png)


You may also modify the commit more later, by making new commits, and running `arc diff` again. This will just update the existing review:

```wiki
$ emacs ...
$ git commit -asm "fix bug in new feature"
$ arc diff # update existing review
```

### Notes on `arc diff`


Note that when you run `arc diff`, it will drop you into an editor to summarize your commit message, add a **test plan**, and **reviewers**, and optionally any **subscribers**. Subscribers will be notified of changes to a review. Reviewers are a set of people who must look at the review. A **test plan** just specifies how you can verify the change - feel free to make it simple and just say 'validate' or otherwise something silly.

**A change cannot be merged until at least one reviewer has signed off**. All revisions must have at least one reviewer. Ideally, multiple people will review a change.

**You should always at least add `austin` to a review**. If you want wider attention, you can specify the reviewer as `#ghc` - this specifies a group of GHC developers who may come to review it as well.

**Question**: What does the `#ghc` mean? **Answer**: Essentially, in Phabricator, a Project is composed of a group of people. A project can be referred to by a hashtag, which basically incorporates everyone involved in the project.

## Automatic builds: Harbormaster


When **you use Arcanist to submit a diff**, Phabricator will automatically trigger a build rule using an application called "Harbormaster". This application causes a build machine in the background to apply your patch and run `sh ./validate`. Afterwords, your diff will be updated with:

- A status from "Harbormaster" about the build.
- A notification from a bot, "phaskell", that will contain build logs.
- If the tests fail, "phaskell" will also include the results.


The current status of the build is at the top. [ Phab:D69](https://phabricator.haskell.org/D69) is a good example:

[](https://i.imgur.com/jiHics8.png)


Note the build was failing at first, and the bot reported this:

[](https://i.imgur.com/sZ5oieH.png)


The diff was then updated, and it passed:

[](https://i.imgur.com/DJljJ4M.png)


Note that every time you run `arc diff` and update an existing review or create a new one, you'll trigger a build.

**Note**: You can use the word "nobuild" by itself in a `Summary:` when you submit a diff to skip builds. You may want to do this if you know your build will break, but you just want to post code.

## Diffusion: Browsing the GHC repository

**[ Diffusion](https://phabricator.haskell.org/diffusion)** is a simple, fast repository browser for GHC which you can use to browse the repo, audit commits, explore branches or just read code.


Note that in Phabricator, every repository has what we call a **callsign**. A callsign is a short, unique identifier for a repository. The GHC repository has the **GHC** callsign. Sometimes in the UI when referring to a repository, you must use the unambiguous name `rGHC`, signifying the repository callsign.

## Herald & Audit: post-commit review

**[ Herald](https://phabricator.haskell.org/herald)** is an application that tracks events that occur on Phabricator, and take certain actions when they happen. For example, when a commit to the GHC repository occurs, you may want to be sent an email about it so you can be aware. Or maybe you'd like to audit commits that touch certain files.

**[ Audit](https://phabricator.haskell.org/audit)** is an application that lets you keep track of commits that have gone into a repository, in case you need to review them. The most common workflow audit enables is *post-commit review*, which means you review code after it's been committed.


Normally, you'll use **Herald** to create rules which trigger an **Audit**, which will then appear in the Audit application.

### Herald: creating rules


Go to the [ Herald application](https://phabricator.haskell.org/herald/) and click *Create New Herald Rule* at the top right.


First, you have to select the event the rule will trigger on. Normally this will be *Commit* if you want to analyze commits, or *Differential Revision* if you want it to trigger on new patches for review.


Next, you have to select the type of rule. You will always want the type to be *Personal* - so it only affects you and nobody else.


Finally, you have the rules screen. It should be mostly self explanatory: set combinations of conditions, and sets of actions to take when the conditions are satisfied.


For example, I may create a **Personal Rule** for **Differential Revisions**. The rule is only triggered when the repository **is any of**`rGHC` (**NB**: the `r` is important!), and when **any changed filename** also **matches regexp**`rts/*`. Finally, the `action` to take will be to **Add me as a reviewier**.


With this rule, any commits which modify the RTS will then automatically have you added as a reviewier.

### Audit: auditing commits that go by


Once you've created a Herald rule, you may now go to the Go to the [ Audit application](https://phabricator.haskell.org/audit/) to review commits that have gone by. Auditing works mostly the same way as reviewing, only after the fact - instead of accepting or rejecting, you can **Raise Concerns** or **Accept Revision**. If you raise a concern, the author will be notified and generally required to rectify the change.

**NB**: commits you should audit, or commits of yours that have had concerns raised will appear on the homepage.


Alternatively, you may use Diffusion to [ browse the GHC repository](https://phabricator.haskell.org/diffusion/GHC), find a commit, and then audit it. For example, by viewing commit [ b6352c9912536929537dcebac9d02d4f995c1657](https://phabricator.haskell.org/rGHCb6352c9912536929537dcebac9d02d4f995c1657), we can look at the diff. Then, go to the bottom, and you can take actions like **Accept Review** or **Raise Concerns** - this is an equivalent to a regular audit for arbitrary commits.

### More reading


Be sure to also read the [ Herald user guide](https://secure.phabricator.com/book/phabricator/article/herald/), as well as the [ Audit user guide](https://secure.phabricator.com/book/phabricator/article/audit/). There's also some documentation on [ review vs audit workflows](https://secure.phabricator.com/book/phabricator/article/reviews_vs_audit/) in the Phab documentation.

## General review workflow


Once you post a review, you can keep revising it by making new commits, and running `arc diff`.


Once people begin reviewing, a few things can happen: more reviewers can be added, subscribers can join, and people can make comments. This can be done by scrolling to the bottom of the page and selecting the appropriate action.


The most important things people can do is **accept a revision** or **request changes**. The actions should be obvious. If a reviewer accepts a revision, it will hopefully be merged soon unless someone complains. But if a person requests changes, the submitter needs to respond. Alternatively, new people may commandeer revisions to take them over as well, or the owner may abandon revisions.

### Landing reviews


If you're submitting a patch to GHC and not a committer, Austin or Herbert will land your changes - and we really appreciate it!


If you *are* a committer, you can leave it to Austin or Herbert as well if you'd like, but ideally you'll do it yourself.


If you have commit access, go to your repository with the branch and your changes. Then you can do:

```wiki
$ arc land <branch name>
```


(**NB**: if your revision has been accepted, this command will appear in a notification at the top of the review page).

`arc land` is a *very* automated tool for landing reviews. What it will do is:

- Take your branch
- Squash the commits into a single commit
- Rebase it on master
- Delete the old branch
- Push the change upstream


So it's really automated! If you'd like something more manual, do this:

```wiki
$ arc patch --nobranch <differential revision>
$ git push origin master
```


This will:

- Pull the patch down, merge it as a single change
- Push it upstream - Phabricator will then close the branch for you as it will see the differential revision notifier in the commit.


You're done!

### Notes on reviews


Note that you can add inline comments to any line in a differential revision. You can also reply to inline comments. **You can have multiple inline comments per top-level comment you add**. Once you have made some inline comments, or replied to one, go to the bottom of the page and hit 'submit' to submit it. You may also add a top-level comment to go with it.

## Tips


There are some good tips for using Phabricator, including...

### Dashboards


When you login, by default you'll be greeted by a default **Dashboard**, which are Phabricator's way of having custom pages.


If you go to the [ Dashboards](https://phabricator.haskell.org/dashboard/) application, you can create a new dashboard, and then create panels to go on it. You can then move panels around on the editor to customize your home page with audits, commits, etc. Once you've created a Dashboard, you can install it as your default home one as well.


The default dashboard should be relatively well tuned for what GHC developers need, but if you need a custom one, feel free to share!

### Commandeering revisions


Occasionally, you may need to take over a patch from someone else on Phab. That can be done by **Commandeering the Revision**. To do that, go to a differential revision, go to the bottom, and use the **Commandeer** action. You will now own the review, be allowed to update it, and more.

### Keyboard shortcuts


On any page, hit the `?` key on your keyboard to bring up all the **keyboard shortcuts**. These are especially useful when reviewing code, since you can quickly navigate between changes in a review to easily navigate.

### Remarkup reference


Be sure to read about Phabricator's markup language, [ Remarkup](https://secure.phabricator.com/book/phabricator/article/remarkup/). Most importantly, make sure you know how to use those image macros.

### Applications configuration


You can configure the applications that appear on the left side panel by visiting your [ panel settings](https://phabricator.haskell.org/settings/panel/home/). You can probably safely get rid of most things except **Differential**, **Diffusion**, **Audit**, and **Herald**.

### Arcanist


Arcanist has a few useful commands - in particular, you'll probably like `arc paste`, and `arc list`.

`arc paste` will allow you to instantly upload things to Phab's [ PasteBin application](https://phabricator.haskell.org/paste/). 

`arc list` will show you all your revisions currently open, and what state they're in. For example, Austin's `arc list` may look like:

```wiki
$ arc list
* Needs Revision D4: Add support for faster copies via Intel Extended REP-MOVSB
* Accepted       D13: Make Applicative a superclass of Monad 
```

### Pastebin


To upload things to the Phabricator pastebin, cat the file into `arc paste`:

```wiki
$ cat foo.txt | arc paste
```


Grab pastes using `arc paste` as well:

```wiki
$ arc paste P23
foo and bar!
```

### Email tips


First off, note that **you can reply to an email from Phabricator** to comment on a review; inbound mail handling works fine. In the email you have handler actions, which you can use to control what action to post to a review - for example, replying to an email with `!reject` and some content will comment on the review, and reject it and ask for revisions.


Second, Phab has a very 'Getting Things Done' interface, which means it tries to only alert you as to what is relevant in a particular project or codebase. You can carefully control what emails you get using Herald particularly, but you still may want to crop things. Be sure to [ configure your mail rules](https://secure.phabricator.com/book/phabricator/article/mail_rules/)!

### External editor support


If you want, you can also configure Phab to use an external editor so you can launch things [ right from your browser](https://secure.phabricator.com/book/phabricator/article/external_editor/)!

### Review IRC logs


There are active IRC logs kept on Phabricator using the [ ChatLog](https://phabricator.haskell.org/chatlog/) application

### Multi-factor authentication


If you're paranoid, enable [ multi-factor authentication](https://secure.phabricator.com/book/phabricator/article/multi_factor_auth/) for your account.

### Play Arkanoid


If you're waiting to validate or compile, run `arc anoid` to play a game of Arkanoid.
