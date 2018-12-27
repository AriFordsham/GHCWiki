Welcome to GHC's GitLab instance. This is intended to be a short getting started guide and status update for GHC's migration to GitLab.

# Getting started

To get started on GitLab you will first want to either [create a new account](https://gitlab.haskell.org/users/sign_in)
or [login with your GitHub credentials](https://gitlab.haskell.org/users/auth/github).

Once you have an account you should [add an SSH key](https://gitlab.haskell.org/profile/keys) so that you can push
to your repositories. If you currently have commit rights to GHC notify me
(@bgamari) of your user name so I can grant you similar rights in GitLab.



# Updating your development environment

You can updated existing working directory (assuming the usual upstream
remote name of `origin`) for the new upstream repository location by
running the following:

```bash
git remote set-url origin https://gitlab.haskell.org/ghc/ghc.git
git remote set-url --push origin git@gitlab.haskell.org:ghc/ghc
```

This is all that should be necessary; a quick `git pull origin master`
should verify that everything works as expected.


# Continuous integration

Continuous integration is now provided by GitLab's native continuous
integration infrastructure. We currently test a variety of
configurations, including many that neither Phabricator nor
CircleCI/Appveyor previously tested (e.g. see this [example run](https://gitlab.haskell.org/ghc/ghc/pipelines/568)):

 * With the make build system:
    * x86_64/Linux on Fedora 27, Debian 8, and Debian 9
    * i386/Linux on Debian 9
    * aarch64/Linux on Debian 9 (currently broken due to a variety of
      issues)
    * x86_64/Windows
    * x86_64/Darwin
    * x86_64/Linux on Debian 9 in a few special configurations:
        * unregisterised (still a bit fragile due to #16085)
        * integer-simple
        * building GHC with -fllvm
 * With Hadrian:
    * x86_64/Linux on Debian 9
    * x86_64/Windows (currently broken due to #15950)

We also run a slightly larger set of jobs on a nightly basis. Note that
binary distributions are saved from most builds and are available for
download for a few weeks (we may put in place a longer retention policy
for some builds in the future).

There are admittedly a few kinks that we are still working out,
particularly in the case of Windows (specifically the long build times
seen on Windows). If you suspect you are seeing spurious build failures
do let us know.

To make the best use of our limited computational resources our CI
builds occur in three stages:

 * lint: the style and correctness checkers which would previously be
   run by `arc lint` and `git push`

 * build: Debian 9 Linux x86_64 built with make and Hadrian

 * full-build: the remaining configurations

If a build fails at an earlier phase no further phases will be run.


# Structuring your merge request

With the transition to GitLab GHC is moving to a model similar to that used by
GitHub. If you have a Differential on Phabricator we will finish review there.
However, please post new patches as merge requests on GitLab.

Note that Phabricator and GitLab have quite different models for
handling patches. Under Phabricator a Differential is a single patch
with no further structure; larger changes can be composed of multiple
dependent Differentials.

Under GitLab's model a merge request is a git branch consisting of
one or more patches. Larger changes can be handled in one of two ways:

 1. a set of dependent merge requests, each of which to be squashed when
    merged.

 2. a single branch with each atomic change made in a single, buildable
    commit

Due to the difficulty of maintaining dependent merge requests, I would
recommend that contributors making larger changes use method (2).


# Submitting your merge request for review

Depending upon whether you have push rights to the GHC repository there
are two ways to submit a merge request:

 * if you have push access you can push a branch directly to
   `git@gitlab.haskell.org:ghc/ghc.git` and open merge request.
   In this case please do follow the usual branch naming conventions:

     * prefix all branch names with `wip/`

     * if you are fixing a particular ticket consider using the name
       `wip/TNNNN`

 * if not you can [create a fork](https://gitlab.haskell.org/ghc/ghc/forks/new) using the "Fork" button on the project
   page and push your branch there

In either case after you have pushed your branch [open a merge request](https://gitlab.haskell.org/ghc/ghc/merge_requests/new)
against `ghc/ghc` [2].


# Reviewing and merging merge requests

As always, all contributors are encouraged to help review proposed
changes. If you are unfamiliar with GitLab's review interface please see
GitLab's [user documentation](https://gitlab.com/help/user/discussions/index.md#discussions). Here are a few quick highlights for
those who are familiar with GitHub but haven't yet used GitLab:

 * As with GitHub, GitLab supports both inline and out-of-line comments.

 * Comments that are actionable (known as "discussions") can be marked
   as resolved and collapsed.

 * Comments can be left on both changed and unchanged lines

 * Revisions of a merge request can be viewed and compared using the
   two drop-down menus at the top of the Changes tab

 * Merge requests can require approvals from particular users before
   considered as mergable

 * Merge requests can be placed in "merge when CI passes" state, which
   will cause merge requests to be merged as soon as they are green

From this point moving forward all changes to GHC will be merged via
GitLab's merge requests facilities and must pass CI before being merged.
To ensure that GHC's git history remains linear `ghc/ghc` will use GitLab's
"fast-forward without a merge commit" merge strategy. Consequently you
will be asked to rebase merge requests which are not fast-forward merges
before merging (a convenient "Rebase" button will appear if the rebase
can be carried out without conflicts.



# Status of the Trac migration

Tobias will be continuing work on the Trac ticket migration after a bit
of a holiday break. Hopefully by mid-January we will be able to move
forward on this part of the migration; I will share more details about
this as they develop.

In the meantime, Trac users should [check and possibly update](https://ghc.haskell.org/trac/ghc/prefs) the
email address associated with their account.  This address will be
used to correlate Trac users with their GitLab equivalents so the
correctness of this address will be important in preserving attribution
information during the Trac import.


# Next steps


We are actively working on cleaning up a few remaining issues with CI:

 * build times are still very long on Windows, despite the fact that we
   are only building the `quick` build flavour on that platform;
   consequently GitLab CI Windows builds do sometimes timeout
   when we are faced with long build queues.

 * we at times run low on disk space on our Windows builder runners,
   resulting in occasional spurious build failures

 * Appveyor builds (which are supposed to supplement the native GitLab
   builds) rarely seem to finish

GitLab upstream has been incredibly supportive of our transition effort
and has expressed interest in assisting us with issues that we encounter.
Our current requests can be found on our [migration effort's tracking ticket](https://gitlab.com/gitlab-org/gitlab-ce/issues/55039).
If you find any additional bugs or workflows that could be improved
please do let me know and I can raise the matter with GitLab.


# Acknowledgments

We would like to acknowledge several parties for their contributions to
this effort:

 * [Packet.net](https://www.packet.com/) and [Google X](https://x.company/) for their generous donation of hosting for
   continuous integration and web hosting

 * [GitLab](https://gitlab.com/) and their [Open Source program](https://gitlab.com/gitlab-com/gitlab-oss) for many productive discussions,
   their generous support, and the GitLab Ultimate license used by
   `gitlab.haskell.org`.

 * Davean Scies for his help procuring the hosting services that power
   our continuous integration.

 * Matthew Pickering, Alp Mestangullari, Tobias Dammers for their work
   in setting up the new instance, sorting out the details of the
   migration, and debugging problems when they arose

Finally, thanks to GHC's contributors for their patience during this
transition; it has been a long process which has stolen a significant
amount of attention from other matters. My apologies we have been a bit
less responsive than usual in code review and ticket triage over the
past few months. Regardless, I am hopeful that this wait will be
worthwhile.


# Final thoughts

This is not only a milestone for the GitLab migration but also for GHC
itself. For the first time GHC has fully-automated testing, proposed
patch CI, and release generation across the full range of Tier 1
configurations it supports, with passing builds in all cases.

We are very excited to begin this next chapter of GHC's development and
are looking forward to your feedback on how we can further improve our
new infrastructure. Onward and upwards!
