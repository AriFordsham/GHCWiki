# Getting Started


THis page tells you how to get started with hacking on GHC.

## Getting GHC to build on your machine

- [Grab the latest sources](building/getting-the-sources)
- [Set up your build tree](building/hacking)


Although old code in GHC is not warning-clean, we want new code to be, and we are gradually cleaning the old modules.  Here is [how to cooperate with this](commentary/coding-style#warnings).  The rest of the same page describes the coding conventions we encourage you to use.

## Deciding what to work on

- You may have a pet project of your own.  If you are wondering which bits of the compiler would be important for you, start by consulting the [Commentary](commentary). 
- Pick an easy bug report (Ticket query: status: new, status: assigned, status: reopened, type: bug, order: priority, group: difficulty) or task (Ticket query: status: new, status: assigned, status: reopened, type: task, order: priority, group: difficulty) to work on

## Contributing back to GHC

- [Validating](testing-patches) your patches
- [Submit patches back](working-conventions/submissions), using `darcs send` is the recommended way.

## More information

- Ask questions on [ the cvs-ghc mailing list](http://haskell.org/mailman/listinfo/cvs-ghc)
- Useful information about GHC's architecture is in the [Commentary](commentary)
- Familiarise yourself with the GHC [WorkingConventions](working-conventions).
