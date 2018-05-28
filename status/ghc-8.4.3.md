# GHC plans for 8.4.3


This page is our road-map for what will be in 8.4.3.  


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Release in Late May 2018.

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

## Tickets slated for 8.4.3

### merge/patch/upstream

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

### new

## Status: new (5 matches)

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#15016](https://gitlab.haskell.org//ghc/ghc/issues/15016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Referencing a do-bound variable in a rec block with ApplicativeDo results in variable not in scope during type checking](https://gitlab.haskell.org//ghc/ghc/issues/15016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#14998](https://gitlab.haskell.org//ghc/ghc/issues/14998)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Sort out the strictness mess for exceptions](https://gitlab.haskell.org//ghc/ghc/issues/14998)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15003](https://gitlab.haskell.org//ghc/ghc/issues/15003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Data.List documentation should list complexities of more functions](https://gitlab.haskell.org//ghc/ghc/issues/15003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>supersven</th></tr>
<tr><th>[\#15014](https://gitlab.haskell.org//ghc/ghc/issues/15014)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Exhaustivity check should suggest when COMPLETE could be helpful](https://gitlab.haskell.org//ghc/ghc/issues/15014)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15015](https://gitlab.haskell.org//ghc/ghc/issues/15015)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Parsing of exp_doc loses section markers](https://gitlab.haskell.org//ghc/ghc/issues/15015)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>

### infoneeded

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.4.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>