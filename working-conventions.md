# Working conventions for working on GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better.
This page has pointers to information you'll need.


First, make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code. However, we do not ask for copyright attribution; you retain copyright on any contributions you make, so feel free to add your copyright to the top of any file in which you make non-trivial changes.


These pages guide you step-by-step through making a contribution:

- [How to fix a bug in GHC](working-conventions/fixing-bugs)
- [How to add a new feature to GHC](working-conventions/adding-features)
- [ How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Conventions

- **Using Git**: Our conventions and some useful tips for using git are here: [WorkingConventions/Git](working-conventions/git).

- OLD: **Using Darcs**: Our conventions and some useful tips for using darcs are here: [WorkingConventions/Darcs](working-conventions/darcs).

- **Using the Bug Tracker**: see [WorkingConventions/BugTracker](working-conventions/bug-tracker)

- **Coding style**: When you are editing GHC's source code, please follow our coding guidelines:

  - [Coding style in the compiler](commentary/coding-style)
  - [Coding style in the runtime system](commentary/rts/conventions)

- **Testing**: all patches that go into GHC must first pass *validation*, which ensures that a basic build works and the *regression test suite* passes.

  - The policy on validating patches, and how to perform validation, is at: [TestingPatches](testing-patches).
  - Details about the regression test suite, and how to use it are at: [Building/RunningTests](building/running-tests)