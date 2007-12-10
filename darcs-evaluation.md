# Darcs retrospective, and the future


GHC has been using darcs for version control since the beginning of 2006.  It has not been all plain sailing, so in this page we will record our experiences with darcs, and attempt to objectively evaluate whether we would be better off with a different version control system.  In the event that we do switch, we need to track exactly what needs to change, so this page will also list those dependencies.

## Problems we currently experience with darcs

- Conflicts and merging.  This is the biggest problem we encounter, and is also the [\#1](https://gitlab.haskell.org//ghc/ghc/issues/1) priority for
  Darcs development.  Any non-trivial branch is affected, and essentially the workaround is to discard
  the history from the branch when merging, and use ordinary diff/patch tools.  Keeping history is
  possible, but impractical for branches with more than a few patches.

- Speed.  many operations are impractical (annotate, `darcs changes <file>`), and many operations just take "too
  long" (i.e. long enough that you go and do something else rather than wait for it to finish,
  which incurs a context-switch cost).  We can't use Trac's darcs integration or darcsweb, for example,
  because both rely on invoking `darcs changes <file>`.

- bugs: we run into darcs bugs other than the conflict/merging bug on a regular basis.

- user interface issues: e.g. in a conflict there's no way to tell
  which two patches are conflicting with each other(!)

- Windows support: is quite flaky still.  (well, it's certainly better than it used to be, and
  at least some Windows users don't consider it to be bad).

## Comparison of darcs relative to other systems


ToDo.  Compare workflows using darcs with the same workflow in other systems.  Igloo suggested one basis for comparison:

```wiki
    # Make a repo with a single file with lines 1,3,5,7 in

    mkdir repo1
    cd repo1
    darcs init
    printf 'Line1\nLine3\nLine5\nLine7\n' > file
    darcs record --all --look-for-adds -m patch1 -A igloo@earth.li
    cd ..

    # Check out 2 copies of the repo

    darcs get repo1 repo2
    darcs get repo1 repo3

    # Add a patch that adds lines 2 and 6, then another that adds line 4

    cd repo1
    printf 'Line1\nLine2\nLine3\nLine5\nLine6\nLine7\n' > file
    darcs record --all -m patch2
    printf 'Line1\nLine2\nLine3\nLine4\nLine5\nLine6\nLine7\n' > file
    darcs record --all -m patch3

    # Pull the line 4 patch, but not the lines 2 and 6 patch, into the
    # other repos non-interactively and interactively

    cd ../repo2
    darcs pull --all --patches patch3
    cd ../repo3
    darcs pull 
    n
    y

    # repo2's and repo3's file now contains lines 1,3,4,5,7
```


This workflow is going to favour darcs over everything else, because of the cherrypicking.  Before you put too much faith in it, it would be good to ask how often people really cherrypick, and why they don't seem to miss it in other systems.

### Mercurial

### Git


Advantages:
  

- Speed


Disadvantages:

- Lack of good Windows support?

### Bzr


Advantages:

- Fairly fast
- Portable (as portable as python, anyhow)
- Merging works correctly based on closest-common-ancestor
- Tracking of renamed files / directories merges correctly
- Revisions form a DAG (more like a tree with merge-points) rather than patchsets
- Supports convenient "centralised-style" commit-remote-by-default as well as "distributed-style" commit-local-by-default. Just 'bind' or 'unbind' your branch whenever you want.
- Simple clear UI


Disadvantages

- Revisions form a DAG (more like a tree with merge-points) rather than patchsets (this is a subjective point, which is why it's in both lists. Which model do you believe in?)
- Cherry-picking isn't very "native" to the data model.
- UI is rather different from darcs (which current contributors are used to).

### Darcs


Advantages to staying with darcs:
  

- Community consistency: essentially the Haskell community has standardised on darcs, so it would be 
  an extra barrier for contributors if they had to learn another VC system.

- Merging, when it works, is done right in darcs.


Disadvantages to staying with darcs:

- Uncertain future: no critical mass of hackers/maintainers.  The technical basis is not well enough
  understood by enough people.

## Dependencies on darcs


The following is intended to be a complete list of the things that would need to change if we were to switch away from darcs, in addition to the conversion of the repository itself, which I am assuming can be automatically converted using available tools.


The following code/scripts would need to be adapted or replaced:

- The `darcs-all` script
- The `push-all` script
- The `aclocal.m4` code that attempts to determine the source tree date
- `.darcs-boring`
- The buildbot scripts
- checkin email script: `/home/darcs/bin/commit-messages-split.sh`
- Trac integration (the GHC Trac does not currently integrate with darcs, however)
- darcsweb (use whatever alternative is available)


The following documentation would need to change:

- `README`
- [Building/GettingTheSources](building/getting-the-sources)
- [Building/Windows](building/windows)
- [Building/QuickStart](building/quick-start)
- [Building/Rebuilding](building/rebuilding)
- [Building/RunningNoFib](building/running-no-fib)
- [DarcsRepositories](darcs-repositories) (inc. the sidebar)
- [WorkingConventions](working-conventions)
- [WorkingConventions/Darcs](working-conventions/darcs)
- [WorkingConventions/FixingBugs](working-conventions/fixing-bugs)
- [WorkingConventions/AddingFeatures](working-conventions/adding-features)
- [GettingStarted](getting-started)
- [TestingPatches](testing-patches)
- [BuildBot](build-bot)

## External references


Posts/blogs:

- [ Hans Fugal: Mercurial and Darcs](http://hans.fugal.net/blog/articles/2007/11/16/mercurial-and-darcs)
- [ Hans Fugal: Darcs and Mercurial Redux](http://hans.fugal.net/blog/articles/2007/11/20/darcs-and-mercurial-redux)
- [ iBanjo: The Risks of Distributed Version Control](http://blog.red-bean.com/sussman/?p=20)
- [ https://lists.ubuntu.com/archives/bazaar/2007q4/033256.html](https://lists.ubuntu.com/archives/bazaar/2007q4/033256.html)
- [ http://bazaar-vcs.org/BzrVsGit](http://bazaar-vcs.org/BzrVsGit)