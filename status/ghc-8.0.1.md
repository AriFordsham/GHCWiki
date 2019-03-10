# GHC plans for 8.0.1


This page is our road-map for what will be in 8.0.

- We hope to incorporate all the "Landed" and "In-flight" stuff under "Release highlights" below.

- We'll include (or at least review) all patches in tickets in "Status: patch" below.

- We will address all the tickets under "Status: new" below with "highest" or "high" priority.  We love help to do more, but there are far too many "normal" tickets to make any promises.


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Release candidate by* mid-December 2015**. Release in **January 2016**.
***

## Release highlights


Below are the major highlights of 8.0.

### Landed in HEAD

- Support for [implicit parameters providing callstacks/source locations](explicit-call-stack/implicit-locations), allowing you to have a light-weight means of getting a call-stack in a Haskell application. ([ Phab:D861](https://phabricator.haskell.org/D861))

- Improved optimization diagnostics. The compiler is now more liberal about issues warnings of failed specialization attempts

- Support for wildcards in data and type family instances ([ Phab:D1092](https://phabricator.haskell.org/D1092))

- Support for [Injective Type Families](injective-type-families), which allows you to specify type families which are injective, i.e. a one-to-one relationship. ([ Phab:D202](https://phabricator.haskell.org/D202))

- Support for typed holes in Template Haskell ([ Phab:D835](https://phabricator.haskell.org/D835))

- Support for [Applicative Do](applicative-do), allowing GHC to desugar do-notation to `Applicative` where possible. ([ Phab:D729](https://phabricator.haskell.org/D729))

- Support for deriving the `Lift` typeclass ([ Phab:D1168](https://phabricator.haskell.org/D1168))

- Support for [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields), allowing multiple datatypes to declare the same record field names provided they are used unambiguously ([ Phab:D761](https://phabricator.haskell.org/D761))

- A beautiful new users guide, written in reStructured Text, and significantly improved output.

- Support for [Strict Haskell](strict-pragma) including both the `StrictData` and `Strict` language extensions ([ Phab:D1142](https://phabricator.haskell.org/D1142))

- Support for record pattern synonyms ([ Phab:D1152](https://phabricator.haskell.org/D1152))

- Allow patterns synonyms to be exported in the same way as data constructors ([ Phab:D1258](https://phabricator.haskell.org/D1258))

- [Custom type errors](proposal/custom-type-errors). ([\#9637](https://gitlab.haskell.org//ghc/ghc/issues/9637), [ Phab:D1236](https://phabricator.haskell.org/D1236)).

- Implement the `MonadFail` proposal ([\#10751](https://gitlab.haskell.org//ghc/ghc/issues/10751))

### In-flight, and likely to land in time

- Visible type application

- Support for reasoning about kind equalities, which gives promotion of GADTs to kinds, kind families, heterogeneous equality (kind-indexed GADTs), and `* :: *`. There is some discussion in [DependentHaskell/Phase1](dependent-haskell/phase1), but that's very low-level. I (Richard) have no good user-oriented write-up yet, but there shouldn't be much in the way of new syntax -- just fewer type errors. ([ Phab:D808](https://phabricator.haskell.org/D808))

- Support for [OverloadedLabels](records/overloaded-record-fields/overloaded-labels), allowing a form of type-directed name resolution ([ Phab:D1033](https://phabricator.haskell.org/D1033) and [ Phab:D1331](https://phabricator.haskell.org/D1331))

- A huge improvement to pattern matching (including much better coverage of GADTs), based on the work of Simon PJ and Georgios Karachalias. For more details, see [ their paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf).

- Backpack is chugging along; we have a new user-facing syntax which allows multiple modules to be defined a single file, and are hoping to release at least the ability to publish multiple "units" in a single Cabal file.

- Improved [DWARF based debugging support](dwarf) from Peter Wortmann, Arash Rouhani, and Ben Gamari with backtraces from Haskell code.

- An [Improved LLVM Backend](improved-llvm-backend) that ships with every major Tier 1 platform.

### Possible, if the authors push forward fast enough

- Support for **Type Signature Sections**, allowing you to write `(:: ty)` as a shorthand for `(\x -> x :: ty)`.
- A `DEPRECATED` pragma for exports ([\#4879](https://gitlab.haskell.org//ghc/ghc/issues/4879))
- Complete support for [OverloadedRecordFields](records/overloaded-record-fields)
- Stack traces in GHCi ([\#11047](https://gitlab.haskell.org//ghc/ghc/issues/11047))

### Won't make it, but being actively worked on

- A new, type-indexed type representation, `data TTypeRep (a :: k)`. See [TypeableT](typeable-t).
- A (possible) overhaul of GHC's build system to use **Shake** instead of Make.

## Migration Guide to 8.0

FIXME Write the migration guide.

[ https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0](https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0)

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

## Tickets slated for 8.0.1

### merge/patch/upstream

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

### new

## Status: new (3 matches)

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#10735](https://gitlab.haskell.org//ghc/ghc/issues/10735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Smooth out the differences between \`compiler/utils/Pretty.hs\` and \`libraries/pretty\`](https://gitlab.haskell.org//ghc/ghc/issues/10735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10927](https://gitlab.haskell.org//ghc/ghc/issues/10927)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[IndexError: pop from empty list](https://gitlab.haskell.org//ghc/ghc/issues/10927)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#3351](https://gitlab.haskell.org//ghc/ghc/issues/3351)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Generated ghc man page missing xrefs](https://gitlab.haskell.org//ghc/ghc/issues/3351)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr></table>

### infoneeded

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.0.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>