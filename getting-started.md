# Getting Started


This page tells you how to get started with hacking on GHC.

## Contributing to the Wiki

- [Register](/trac/ghc/register) an account, so that you can edit pages
- Look at the "Wiki" links in the green sidebar on the left of every page

## Getting GHC to build on your machine

- [The Building Guide](building) has all the information you need to set up your system for building GHC, and get a working development build.

## Deciding what to work on

- You may have a pet project of your own.  If you are wondering which bits of the compiler would be important for you, start by consulting the [Commentary](commentary). 
- Pick an easy bug report (Ticket query: status: new, status: assigned, status: reopened, type: bug, order: priority, group: difficulty) or task (Ticket query: status: new, status: assigned, status: reopened, type: task, order: priority, group: difficulty) to work on
- Help us with our [BugSweep](bug-sweep)

## Contributing back to GHC


The following pages describe the process of making a change to GHC and contributing it back.  For small changes you can just `darcs send`, but for larger changes it helps a great deal if the patch is in a form that we can review quickly and use without too much extra work.

- [How to fix a bug in GHC](working-conventions/fixing-bugs)
- [How to add a new feature to GHC](working-conventions/adding-features)


Note that, largely due to performance issues in darcs 1.\*, we currently avoid applying patches with conflicts to GHC, and its associated repositories. If you find that you have a conflict in a patch that you want to send or apply, please first unrecord and re-record (or amend-record) it so that the conflict is removed.

## More information

- Ask questions on [ the cvs-ghc mailing list](http://haskell.org/mailman/listinfo/cvs-ghc)
- Useful information about GHC's architecture is in the [Commentary](commentary)
- Familiarise yourself with the GHC [WorkingConventions](working-conventions).
