# GitLab Migration


In October 2018 a [ proposal](https://mail.haskell.org/pipermail/ghc-devs/2018-October/016425.html) was discussed on ghc-devs and ghc-devops suggesting that GHC migrate away from Phabricator and towards GitLab for its code review infrastructure. In addition, we are considering consolidating several additional services. Specifically:

1. Phabricator for code review
1. gitolite, for repository hosting
1. Trac, for issue tracking and the Wiki

## Code review


Nothing particular needs to happen here. We will simply phase out Phabricator
and start taking MRs via GitLab.

## Git hosting


Currently GHC uses Gitolite, Gitweb, and a pile of custom hooks to host the `ghc` repository and its mirrored core libraries.


Our gitolite configuration gives us a few things, most of which GitLab
can also provide:

- Mirroring ghc packages to GitHub (GitLab provides this)
- Mirroring core libraries from GitHub and elsewhere (GitLab can also
  do this)
- Checks for inconsistent submodule changes.

> >
> > There are a few possible ways this can be accomplished with what
> > GitLab provides:

- Assuming all code goes through MRs: as a CI pipeline job
- with a [ custom hook](https://github.com/gitlabhq/gitlabhq/blob/667c0a909bde1cf71f21d8ec9768e98b1c489030/doc/hooks/custom_hooks.md)

- Code and commit message style checks (this is probably best done as a
  CI pipeline)
- Branch protection (GitLab provides this, although with slightly reduced granularity. However, this granularity shouldn't be necessary in the case of GitLab since users will largely be working within their own forks).
- Repository browsing via gitweb (GitLab provides this)


So, on the whole migrating Git hosting entirely to GitLab seems
straightforward. It will require that we replace the forward slashes in
the core library submodules with dashes (`packages/filepath` turns into
`packages-filepath`) but this has been a long time coming (we have
struggled with this on GitHub for a long time).

## Migrating Trac


This is a more delicate situation. We certainly want to get this right
since we will only do this migration once. As I pointed out earlier, I
briefly hacked together a 75% of a migration script a few weeks ago.
Naturally this means all that remains is the last 90%. Tobias has been
working on this.


In particular, migrating user accounts is a bit tricky since GitLab
requires that all users have an associated email address yet in Trac
accounts are entirely orthogonal to mail. Consequently, I implemented
the following scheme to migrate accounts:

1. We manually build a list of users covering the current major
  contributors, mapping their Trac username(s) to their email address.
  Create these accounts.

1. For all remaining users we make up an email address (e.g.
  trac+USERNAME@…) and use a dummy `trac-USERNAME` user
  name.


Users falling into bucket (b) can either create a new account (since
their usual username will still be available) or, if they want to retain
attribution of their previous comments, email us to recover their
`trac-` prefixed account (which they can rename to remove the prefix).


In general, I running the Trac targetting a clean GitLab
instance is both easier and safer than doing so on a live instance, so I
would prefer that we move everything at once. However, this does mean
that we need to have things sorted before migrating.


After migration we intend on keeping a read-only Trac instance around for
future reference.

### Markup conversion


The import already has reasonable coverage of Trac markup. In my opinion
aiming for 100% coverage is unrealistic and unnecessary. We will aim
for the import to cover the majority of syntax in common use, and collect
syntax falling outside of this set for manual review.

### Tickets


Ticket migration absolutely must preserve ticket numbers, which the current
migration does. Trac maintains a significant amount of metadata in fields.
We want to ensure that most of this information is captured \*somehow\* in
the corresponding GitLab ticket. Towards this end, the import does the following:

- Trac milestones are represented as GitLab milestones
- Trac keywords are represented as GitLab labels; however to avoid cluttering
  the label space we will first do a bit of spring cleaning of keywords before import
- Attachments will be represented as either snippets (in the case of Haskell soource files) or uploaded files (otherwise)
- The type of failure, component, operating system and architecture fields will be represented as prefixed labels 
- The priority field will be represented as GitLab weight
- The version, test case, and wiki page fields will be represented in the ticket text
- The related, blocking, and blocked tickets fields will be represented by issue relations
- Phabricator differential references will have to be converted to plain web links (yet to be implemented); this means that we will need to commit to a phabricator base URL at migration time, which will be very difficult to change later.


Much of this logic is already implemented

### Wiki


GHC's Wiki has a wealth of information. However, much of this information is now out-of-date.


We might consider using this opportunity to clean up the wiki by migrating the Trac wiki to the `Trac` namespace within the GitLab wiki. We can then move still-relevant data out to the top-level after the migration.

## Migrating CI


A few things need to happen here:

- Add a new Appveyor project building against the GitLab project

- Write a bit of glue to incorporate CircleCI


There is the question of what to about CI during the migration process, however. My current thinking is that we simply try to keep Harbormaster limping along as best we can and keep a close eye on CircleCI.

## Timeline


The goal is to be able to start accepting MRs by December 15th.
