# Bug squashing at ZuriHac2014


Joachim (nomeata) wants to run a small bugsquashing sprint at [ ZuriHac 2014](http://www.haskell.org/haskellwiki/ZuriHac2014/Projects). 

## Requirements


You should bring some Haskell experience and be confident reading other people’s Haskell code. You do not need to know all the latest fancy type hackery – GHC itself is written in quite plain Haskell. Some knowledge of git is also useful.


Obviously, you need a machine to work on. The more core it has, the less you’ll have to wait.

## Setup


If you want to join in, you can come prepared:

- Read through [Newcomers](newcomers)
- Make sure that you have built GHC once yourself.
- Your changes need to be validated. So make sure you validated GHC once. I suggest to have a second working copy of GHC that you only use to validate. There is a [section](working-conventions/git#workflow-with-validate) explaining how to do this.
- Fork [ ghc on github](https://github.com/ghc/ghc/) (or otherwise publish a fork of the GHC repo) for easier collaboration during the hackathon.
- Get an account on this trac.
- Join `#ghc` on freenode.
- (optional, if you plan to stick around) Subscribe to `ghc-dev` and `ghc-tickets` mailing lists.

## Optional tips


If you have a strong remote machine with lots of cores, you can have the validate tree remotely.


For more convenient validation, especially if the validate repository is remotely, I (Joachim) have a script `ci-validate.sh` that waits for a new branch calls `validate/foo`, then validates it cleanly and either moves it to `validated/foo` or `broken/foo`. If you want to set up that as well, fetch the script from my [ ghc-devscripts repository](https://github.com/nomeata/ghc-devscripts).

## Possible tickets


This is a list of tickets that might be suitable for a hacking sprint, but feel free to look for others (click “All Bugs“ and “All Tasks” on the left). And of course, feel free to extend this list.

<table><tr><th>Ticket (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, desc: 1, order: id)</th>
<th>Summary (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: summary)</th>
<th>Owner (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: owner)</th>
<th>Type (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: type)</th>
<th>Status (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: status)</th>
<th>Priority (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: priority)</th>
<th>Milestone (Ticket query: id: 9095%2C9122%2C9127%2C9132%2C9136%2C95%2C1388%2C8959%2C9156%2C17%2C9177%2C8429%2C9178%2C4836, max: 0, order: milestone)</th></tr>
<tr><th>[\#17](https://gitlab.haskell.org//ghc/ghc/issues/17)</th>
<th>[Separate warnings for unused local and top-level bindings](https://gitlab.haskell.org//ghc/ghc/issues/17)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>[8.0.1](/trac/ghc/milestone/8.0.1)</th></tr>
<tr><th>[\#95](https://gitlab.haskell.org//ghc/ghc/issues/95)</th>
<th>[GHCi :edit command should jump to the the last error](https://gitlab.haskell.org//ghc/ghc/issues/95)</th>
<th>lortabac</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th></tr>
<tr><th>[\#1388](https://gitlab.haskell.org//ghc/ghc/issues/1388)</th>
<th>[Newbie help features](https://gitlab.haskell.org//ghc/ghc/issues/1388)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th></tr>
<tr><th>[\#4836](https://gitlab.haskell.org//ghc/ghc/issues/4836)</th>
<th>[literate markdown not handled correctly by unlit](https://gitlab.haskell.org//ghc/ghc/issues/4836)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#8429](https://gitlab.haskell.org//ghc/ghc/issues/8429)</th>
<th>[GHC.Base.{breakpoint, breakpointCond} do nothing](https://gitlab.haskell.org//ghc/ghc/issues/8429)</th>
<th>iand675</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      new
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8959](https://gitlab.haskell.org//ghc/ghc/issues/8959)</th>
<th>[GHCi should honour UnicodeSyntax](https://gitlab.haskell.org//ghc/ghc/issues/8959)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>[8.0.1](/trac/ghc/milestone/8.0.1)</th></tr>
<tr><th>[\#9095](https://gitlab.haskell.org//ghc/ghc/issues/9095)</th>
<th>[make sdist picks up test files](https://gitlab.haskell.org//ghc/ghc/issues/9095)</th>
<th>thomie</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>[8.2.1](/trac/ghc/milestone/8.2.1)</th></tr>
<tr><th>[\#9122](https://gitlab.haskell.org//ghc/ghc/issues/9122)</th>
<th>[Make Lint check for bad uses of \`unsafeCoerce\`](https://gitlab.haskell.org//ghc/ghc/issues/9122)</th>
<th>qnikst</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[8.0.1](/trac/ghc/milestone/8.0.1)</th></tr>
<tr><th>[\#9127](https://gitlab.haskell.org//ghc/ghc/issues/9127)</th>
<th>[Don't warn about pattern-bindings of the form \`let !_ = rhs\`](https://gitlab.haskell.org//ghc/ghc/issues/9127)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9132](https://gitlab.haskell.org//ghc/ghc/issues/9132)</th>
<th>[takeWhile&C. still not fusible](https://gitlab.haskell.org//ghc/ghc/issues/9132)</th>
<th>skeuchel</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[7.10.1](/trac/ghc/milestone/7.10.1)</th></tr>
<tr><th>[\#9136](https://gitlab.haskell.org//ghc/ghc/issues/9136)</th>
<th>[Constant folding in Core could be better](https://gitlab.haskell.org//ghc/ghc/issues/9136)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[8.6.1](/trac/ghc/milestone/8.6.1)</th></tr>
<tr><th>[\#9156](https://gitlab.haskell.org//ghc/ghc/issues/9156)</th>
<th>[Duplicate record field](https://gitlab.haskell.org//ghc/ghc/issues/9156)</th>
<th>gintas</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#9177](https://gitlab.haskell.org//ghc/ghc/issues/9177)</th>
<th>[Suggest Int when user uses int](https://gitlab.haskell.org//ghc/ghc/issues/9177)</th>
<th>nomeata</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[7.10.1](/trac/ghc/milestone/7.10.1)</th></tr>
<tr><th>[\#9178](https://gitlab.haskell.org//ghc/ghc/issues/9178)</th>
<th>[improve orphan instance warning](https://gitlab.haskell.org//ghc/ghc/issues/9178)</th>
<th></th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      closed
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>

## Summary

- *n* people particitpated: nomeata, *please add yourself*
- *n* Tickets worked on: [\#9177](https://gitlab.haskell.org//ghc/ghc/issues/9177), [\#8959](https://gitlab.haskell.org//ghc/ghc/issues/8959), [\#9127](https://gitlab.haskell.org//ghc/ghc/issues/9127), [\#9178](https://gitlab.haskell.org//ghc/ghc/issues/9178)