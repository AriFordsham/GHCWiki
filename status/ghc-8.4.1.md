# GHC plans for 8.4.1


This page is our road-map for what will be in 8.4.  


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


TBD.

## Libraries Status


See [Libraries](status/ghc-8.4.1/libraries)

## Release highlights (planned)


Below are the major highlights of 8.4.

- Further work on **compiler performance**
- Improved support for **cross-compilation** (Moritz Angermann)
- A *more expressive Haskell AST** based on [Trees That Grow](implementing-trees-that-grow) (Shayan Najd)
  ***

### Build system and miscellaneous changes

- **New Shake-based build system, `hadrian`, will be merged.**  (Andrey Mokhov)
- **Remove dependency on Hoopl package.**  (Michal Terepeta, [ Phab:D3616](https://phabricator.haskell.org/D3616))
- Improved Windows support, including support for split sections and long file paths (Tamar Christina)

### Landed in `master` branch

- TODO

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

## Tickets slated for 8.4.1

### merge/patch/upstream

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

### new

## Status: new (13 matches)

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#11196](https://gitlab.haskell.org//ghc/ghc/issues/11196)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TypeInType performance regressions](https://gitlab.haskell.org//ghc/ghc/issues/11196)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#11198](https://gitlab.haskell.org//ghc/ghc/issues/11198)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TypeInType error message regressions](https://gitlab.haskell.org//ghc/ghc/issues/11198)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#11371](https://gitlab.haskell.org//ghc/ghc/issues/11371)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Bogus in-scope set in substitutions](https://gitlab.haskell.org//ghc/ghc/issues/11371)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>niteria</th></tr>
<tr><th>[\#11523](https://gitlab.haskell.org//ghc/ghc/issues/11523)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Infinite Loop when mixing UndecidableSuperClasses and the class/instance constraint synonym trick.](https://gitlab.haskell.org//ghc/ghc/issues/11523)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#12088](https://gitlab.haskell.org//ghc/ghc/issues/12088)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type/data family instances in kind checking](https://gitlab.haskell.org//ghc/ghc/issues/12088)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#12506](https://gitlab.haskell.org//ghc/ghc/issues/12506)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Compile time regression in GHC 8.](https://gitlab.haskell.org//ghc/ghc/issues/12506)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>bgamari</th></tr>
<tr><th>[\#12564](https://gitlab.haskell.org//ghc/ghc/issues/12564)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type family in type pattern kind](https://gitlab.haskell.org//ghc/ghc/issues/12564)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#13993](https://gitlab.haskell.org//ghc/ghc/issues/13993)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Certain inter-module specializations run out of simplifier ticks](https://gitlab.haskell.org//ghc/ghc/issues/13993)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#14253](https://gitlab.haskell.org//ghc/ghc/issues/14253)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable](https://gitlab.haskell.org//ghc/ghc/issues/14253)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#14297](https://gitlab.haskell.org//ghc/ghc/issues/14297)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[make bindist packages the wrong binaries for cross compilers](https://gitlab.haskell.org//ghc/ghc/issues/14297)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>angerman</th></tr>
<tr><th>[\#14495](https://gitlab.haskell.org//ghc/ghc/issues/14495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Relocatable GHC](https://gitlab.haskell.org//ghc/ghc/issues/14495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>angerman</th></tr>
<tr><th>[\#14856](https://gitlab.haskell.org//ghc/ghc/issues/14856)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC API: Linker failure on loading target multiple times](https://gitlab.haskell.org//ghc/ghc/issues/14856)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14858](https://gitlab.haskell.org//ghc/ghc/issues/14858)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Typed hole subtitution search fails in the REPL](https://gitlab.haskell.org//ghc/ghc/issues/14858)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>

### infoneeded

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.4.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>