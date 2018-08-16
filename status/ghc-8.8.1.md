# GHC plans for 8.8.1


This page is our road-map for what will be in 8.8.


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Cut release branch in November 2018. Release in April 2019.

## Libraries Status


See Libraries? and [Migration/8.8](migration/8.8).

## Release highlights (planned)


Below are the major highlights of 8.8.

### Build system and miscellaneous changes

- [ Reinstallable lib:ghc](https://mail.haskell.org/pipermail/ghc-devs/2017-July/014424.html)

## Landed in `master` branch

### Library changes

### Build system and miscellaneous changes

## Tickets marked merge with no milestone

<table><tr><th>Ticket (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

## Tickets slated for 8.8.1

### merge/patch/upstream

## Status: merge (22 matches)

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#16141](https://gitlab.haskell.org//ghc/ghc/issues/16141)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[StrictData and TypeFamilies regression](https://gitlab.haskell.org//ghc/ghc/issues/16141)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/88](https://gitlab.haskell.org/ghc/ghc/merge_requests/88)</th>
<th></th></tr>
<tr><th>[\#16188](https://gitlab.haskell.org//ghc/ghc/issues/16188)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC HEAD-only panic (buildKindCoercion)](https://gitlab.haskell.org//ghc/ghc/issues/16188)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/207](https://gitlab.haskell.org/ghc/ghc/merge_requests/207)</th>
<th>goldfire</th></tr>
<tr><th>[\#16204](https://gitlab.haskell.org//ghc/ghc/issues/16204)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC HEAD-only Core Lint error (Argument value doesn't match argument type)](https://gitlab.haskell.org//ghc/ghc/issues/16204)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/207](https://gitlab.haskell.org/ghc/ghc/merge_requests/207)</th>
<th></th></tr>
<tr><th>[\#16225](https://gitlab.haskell.org//ghc/ghc/issues/16225)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC HEAD-only Core Lint error (Trans coercion mis-match)](https://gitlab.haskell.org//ghc/ghc/issues/16225)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/207](https://gitlab.haskell.org/ghc/ghc/merge_requests/207)</th>
<th></th></tr>
<tr><th>[\#16331](https://gitlab.haskell.org//ghc/ghc/issues/16331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[REGRESSION: --supported-languages lies about supported languages, again](https://gitlab.haskell.org//ghc/ghc/issues/16331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/383](https://gitlab.haskell.org/ghc/ghc/merge_requests/383)</th>
<th></th></tr>
<tr><th>[\#16195](https://gitlab.haskell.org//ghc/ghc/issues/16195)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Program with trivial polymorphism leads to out of scope dictionary](https://gitlab.haskell.org//ghc/ghc/issues/16195)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th>
<th></th></tr>
<tr><th>[\#8657](https://gitlab.haskell.org//ghc/ghc/issues/8657)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-fregs-graph still has a limit on spill slots](https://gitlab.haskell.org//ghc/ghc/issues/8657)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/219](https://gitlab.haskell.org/ghc/ghc/merge_requests/219)</th>
<th>archblob</th></tr>
<tr><th>[\#14729](https://gitlab.haskell.org//ghc/ghc/issues/14729)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[normaliseType is not well-kinded](https://gitlab.haskell.org//ghc/ghc/issues/14729)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/208](https://gitlab.haskell.org/ghc/ghc/merge_requests/208)</th>
<th></th></tr>
<tr><th>[\#15549](https://gitlab.haskell.org//ghc/ghc/issues/15549)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Core Lint error with EmptyCase](https://gitlab.haskell.org//ghc/ghc/issues/15549)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/208](https://gitlab.haskell.org/ghc/ghc/merge_requests/208)</th>
<th></th></tr>
<tr><th>[\#15733](https://gitlab.haskell.org//ghc/ghc/issues/15733)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Several links in GHC.Exts.Heap documentation are broken](https://gitlab.haskell.org//ghc/ghc/issues/15733)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      D5257
                      
                      
                    </th>
<th></th></tr>
<tr><th>[\#15849](https://gitlab.haskell.org//ghc/ghc/issues/15849)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Error message: "Perhaps you need a let in a do block", when there is no do block.](https://gitlab.haskell.org//ghc/ghc/issues/15849)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/330](https://gitlab.haskell.org/ghc/ghc/merge_requests/330)</th>
<th>nineonine</th></tr>
<tr><th>[\#15897](https://gitlab.haskell.org//ghc/ghc/issues/15897)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Negative MUT time in +RTS -s -RTS when heap profiling is enabled](https://gitlab.haskell.org//ghc/ghc/issues/15897)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr>
<tr><th>[\#16183](https://gitlab.haskell.org//ghc/ghc/issues/16183)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC HEAD regression: -ddump-splices incorrectly parenthesizes HsKindSig applications](https://gitlab.haskell.org//ghc/ghc/issues/16183)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/121](https://gitlab.haskell.org/ghc/ghc/merge_requests/121)</th>
<th></th></tr>
<tr><th>[\#16230](https://gitlab.haskell.org//ghc/ghc/issues/16230)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[API Annotations: more explicit foralls fixup](https://gitlab.haskell.org//ghc/ghc/issues/16230)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/284](https://gitlab.haskell.org/ghc/ghc/merge_requests/284)</th>
<th>alanz</th></tr>
<tr><th>[\#16236](https://gitlab.haskell.org//ghc/ghc/issues/16236)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[API Annotations: AnnAt disconnected for TYPEAPP](https://gitlab.haskell.org//ghc/ghc/issues/16236)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/284](https://gitlab.haskell.org/ghc/ghc/merge_requests/284)</th>
<th>alanz</th></tr>
<tr><th>[\#16255](https://gitlab.haskell.org//ghc/ghc/issues/16255)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Visible kind application defeats type family with higher-rank result kind](https://gitlab.haskell.org//ghc/ghc/issues/16255)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/260](https://gitlab.haskell.org/ghc/ghc/merge_requests/260)</th>
<th></th></tr>
<tr><th>[\#16265](https://gitlab.haskell.org//ghc/ghc/issues/16265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[API Annotations: parens anns discarded for \`(\*)\` operator](https://gitlab.haskell.org//ghc/ghc/issues/16265)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>alanz</th></tr>
<tr><th>[\#16279](https://gitlab.haskell.org//ghc/ghc/issues/16279)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Lexer: Alternate Layout Rule injects actual not virtual braces](https://gitlab.haskell.org//ghc/ghc/issues/16279)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/284](https://gitlab.haskell.org/ghc/ghc/merge_requests/284)</th>
<th>alanz</th></tr>
<tr><th>[\#16287](https://gitlab.haskell.org//ghc/ghc/issues/16287)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[:kind accepts bogus type](https://gitlab.haskell.org//ghc/ghc/issues/16287)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/293](https://gitlab.haskell.org/ghc/ghc/merge_requests/293)</th>
<th></th></tr>
<tr><th>[\#16303](https://gitlab.haskell.org//ghc/ghc/issues/16303)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[checkStack sanity check fails](https://gitlab.haskell.org//ghc/ghc/issues/16303)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr>
<tr><th>[\#16311](https://gitlab.haskell.org//ghc/ghc/issues/16311)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Suggest -XExistentialQuantification for 'forall' in data declarations](https://gitlab.haskell.org//ghc/ghc/issues/16311)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/363](https://gitlab.haskell.org/ghc/ghc/merge_requests/363)</th>
<th></th></tr>
<tr><th>[\#16339](https://gitlab.haskell.org//ghc/ghc/issues/16339)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cannot put (.) or (!) type operators into an export list](https://gitlab.haskell.org//ghc/ghc/issues/16339)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/403](https://gitlab.haskell.org/ghc/ghc/merge_requests/403)</th>
<th></th></tr>
<tr><th>## Status: patch (7 matches)

</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#16022](https://gitlab.haskell.org//ghc/ghc/issues/16022)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian appears to link against libffi unconditionally](https://gitlab.haskell.org//ghc/ghc/issues/16022)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ Phab:D5427](https://phabricator.haskell.org/D5427)</th>
<th></th></tr>
<tr><th>[\#16201](https://gitlab.haskell.org//ghc/ghc/issues/16201)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghci063 failing on Darwin](https://gitlab.haskell.org//ghc/ghc/issues/16201)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/440](https://gitlab.haskell.org/ghc/ghc/merge_requests/440)</th>
<th></th></tr>
<tr><th>[\#16378](https://gitlab.haskell.org//ghc/ghc/issues/16378)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[bkpcabal01 fails with recent Cabal](https://gitlab.haskell.org//ghc/ghc/issues/16378)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>[ https://gitlab.haskell.org/ghc/ghc/merge_requests/512](https://gitlab.haskell.org/ghc/ghc/merge_requests/512)</th>
<th></th></tr>
<tr><th>[\#11126](https://gitlab.haskell.org//ghc/ghc/issues/11126)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Entered absent arg in a Repa program](https://gitlab.haskell.org//ghc/ghc/issues/11126)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>[ Phab:D3221](https://phabricator.haskell.org/D3221)</th>
<th>bgamari</th></tr>
<tr><th>[\#10069](https://gitlab.haskell.org//ghc/ghc/issues/10069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[CPR related performance issue](https://gitlab.haskell.org//ghc/ghc/issues/10069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>
                      
                      
                      
                      
                      
                      
                      !401
                      
                      
                    </th>
<th></th></tr>
<tr><th>[\#15656](https://gitlab.haskell.org//ghc/ghc/issues/15656)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Extend -Wall with incomplete-uni-patterns and incomplete-record-updates](https://gitlab.haskell.org//ghc/ghc/issues/15656)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D5415](https://phabricator.haskell.org/D5415)</th>
<th>ckoparkar</th></tr>
<tr><th>[\#15896](https://gitlab.haskell.org//ghc/ghc/issues/15896)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[GHC API: add function to allow looking up Name for Located RdrName](https://gitlab.haskell.org//ghc/ghc/issues/15896)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D5330](https://phabricator.haskell.org/D5330)</th>
<th>alanz</th></tr>
<tr><th>## Status: upstream (5 matches)

</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#13897](https://gitlab.haskell.org//ghc/ghc/issues/13897)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Ship check-ppr in bindist and compile during testsuite run](https://gitlab.haskell.org//ghc/ghc/issues/13897)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>[ Phab:D4039](https://phabricator.haskell.org/D4039)</th>
<th>alanz</th></tr>
<tr><th>[\#9775](https://gitlab.haskell.org//ghc/ghc/issues/9775)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["Failed to remove" errors during Windows build from hsc2hs](https://gitlab.haskell.org//ghc/ghc/issues/9775)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr>
<tr><th>[\#10822](https://gitlab.haskell.org//ghc/ghc/issues/10822)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC inconsistently handles \\\\?\\ for long paths on Windows](https://gitlab.haskell.org//ghc/ghc/issues/10822)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D4416](https://phabricator.haskell.org/D4416)</th>
<th></th></tr>
<tr><th>[\#12965](https://gitlab.haskell.org//ghc/ghc/issues/12965)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[String merging broken on Windows](https://gitlab.haskell.org//ghc/ghc/issues/12965)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D3384](https://phabricator.haskell.org/D3384)</th>
<th></th></tr>
<tr><th>[\#15074](https://gitlab.haskell.org//ghc/ghc/issues/15074)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Possible uninitialised values in ffi64.c](https://gitlab.haskell.org//ghc/ghc/issues/15074)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr></table>

### new

## Status: new (31 matches)

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#14375](https://gitlab.haskell.org//ghc/ghc/issues/14375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Implement with\# primop](https://gitlab.haskell.org//ghc/ghc/issues/14375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>bgamari</th></tr>
<tr><th>[\#15064](https://gitlab.haskell.org//ghc/ghc/issues/15064)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[T8089 mysteriously fails when GHC is built with LLVM](https://gitlab.haskell.org//ghc/ghc/issues/15064)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>osa1</th></tr>
<tr><th>[\#15779](https://gitlab.haskell.org//ghc/ghc/issues/15779)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Follow-ups to D5169](https://gitlab.haskell.org//ghc/ghc/issues/15779)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#15919](https://gitlab.haskell.org//ghc/ghc/issues/15919)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Deprecate split objects](https://gitlab.haskell.org//ghc/ghc/issues/15919)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#15948](https://gitlab.haskell.org//ghc/ghc/issues/15948)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian build fails on Windows when invoked without --configure flag](https://gitlab.haskell.org//ghc/ghc/issues/15948)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#16051](https://gitlab.haskell.org//ghc/ghc/issues/16051)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cross compilation broken under Hadrian](https://gitlab.haskell.org//ghc/ghc/issues/16051)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>alpmestan</th></tr>
<tr><th>[\#16058](https://gitlab.haskell.org//ghc/ghc/issues/16058)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC built on macOS Mojave nondeterministically segfaults](https://gitlab.haskell.org//ghc/ghc/issues/16058)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#16398](https://gitlab.haskell.org//ghc/ghc/issues/16398)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Missing documentation in Windows bindist tarball](https://gitlab.haskell.org//ghc/ghc/issues/16398)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#8095](https://gitlab.haskell.org//ghc/ghc/issues/8095)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TypeFamilies painfully slow](https://gitlab.haskell.org//ghc/ghc/issues/8095)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#8949](https://gitlab.haskell.org//ghc/ghc/issues/8949)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[switch -msse2 to be on by default](https://gitlab.haskell.org//ghc/ghc/issues/8949)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#12758](https://gitlab.haskell.org//ghc/ghc/issues/12758)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Bring sanity to our performance testsuite](https://gitlab.haskell.org//ghc/ghc/issues/12758)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#13253](https://gitlab.haskell.org//ghc/ghc/issues/13253)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Exponential compilation time with RWST & ReaderT stack with \`-02\`](https://gitlab.haskell.org//ghc/ghc/issues/13253)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>bgamari, osa1</th></tr>
<tr><th>[\#13786](https://gitlab.haskell.org//ghc/ghc/issues/13786)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi linker is dependent upon object file order](https://gitlab.haskell.org//ghc/ghc/issues/13786)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#14069](https://gitlab.haskell.org//ghc/ghc/issues/14069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[RTS linker maps code as writable](https://gitlab.haskell.org//ghc/ghc/issues/14069)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>rockbmb</th></tr>
<tr><th>[\#14974](https://gitlab.haskell.org//ghc/ghc/issues/14974)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[2-fold memory usage regression GHC 8.2.2 -\> GHC 8.4.1 compiling \`mmark\` package](https://gitlab.haskell.org//ghc/ghc/issues/14974)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>davide</th></tr>
<tr><th>[\#15059](https://gitlab.haskell.org//ghc/ghc/issues/15059)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghcpkg05 fails](https://gitlab.haskell.org//ghc/ghc/issues/15059)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15262](https://gitlab.haskell.org//ghc/ghc/issues/15262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC and iserv cannot agree on what an Integer is; insanity ensues](https://gitlab.haskell.org//ghc/ghc/issues/15262)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15383](https://gitlab.haskell.org//ghc/ghc/issues/15383)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[T3171 doesn't terminate with Interrupted message on Darwin](https://gitlab.haskell.org//ghc/ghc/issues/15383)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15442](https://gitlab.haskell.org//ghc/ghc/issues/15442)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GhcStage3HcOpts passed to ghc-stage1](https://gitlab.haskell.org//ghc/ghc/issues/15442)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15503](https://gitlab.haskell.org//ghc/ghc/issues/15503)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[interpreter: sequence_ (replicate 100000000 (return ()))  gobbles up memory](https://gitlab.haskell.org//ghc/ghc/issues/15503)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>osa1</th></tr>
<tr><th>[\#15577](https://gitlab.haskell.org//ghc/ghc/issues/15577)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TypeApplications-related infinite loop (GHC 8.6+ only)](https://gitlab.haskell.org//ghc/ghc/issues/15577)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15703](https://gitlab.haskell.org//ghc/ghc/issues/15703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Significant compilation time blowup when refactoring singletons-heavy code](https://gitlab.haskell.org//ghc/ghc/issues/15703)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15913](https://gitlab.haskell.org//ghc/ghc/issues/15913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-8.6.2 fails to build on s390x (unregisterised): ‘stg_MUT_ARR_PTRS_FROZEN0_info’ undeclared (first use in this function); did you mean ‘stg_MUT_ARR_PTRS_FROZEN_DIRTY_info’?](https://gitlab.haskell.org//ghc/ghc/issues/15913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15971](https://gitlab.haskell.org//ghc/ghc/issues/15971)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian fails Shake's linter on Windows](https://gitlab.haskell.org//ghc/ghc/issues/15971)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15982](https://gitlab.haskell.org//ghc/ghc/issues/15982)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian's \`--configure\` flag is broken on Windows](https://gitlab.haskell.org//ghc/ghc/issues/15982)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#16037](https://gitlab.haskell.org//ghc/ghc/issues/16037)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[memcpy test inexplicably failing](https://gitlab.haskell.org//ghc/ghc/issues/16037)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#16073](https://gitlab.haskell.org//ghc/ghc/issues/16073)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian build fails on Windows](https://gitlab.haskell.org//ghc/ghc/issues/16073)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#16085](https://gitlab.haskell.org//ghc/ghc/issues/16085)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ffi018_ghci fails when unregisterised](https://gitlab.haskell.org//ghc/ghc/issues/16085)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15990](https://gitlab.haskell.org//ghc/ghc/issues/15990)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Dynamically built GHC crashes on MacOS](https://gitlab.haskell.org//ghc/ghc/issues/15990)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16165](https://gitlab.haskell.org//ghc/ghc/issues/16165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Move Hadrian (github) wiki information to in-tree docs](https://gitlab.haskell.org//ghc/ghc/issues/16165)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16217](https://gitlab.haskell.org//ghc/ghc/issues/16217)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[check-api-annotations should check that an annotation does not precede its span](https://gitlab.haskell.org//ghc/ghc/issues/16217)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>

### infoneeded

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>