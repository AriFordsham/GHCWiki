# GHC plans for 8.6.1


This page is our road-map for what will be in 8.6.  


If you believe your favorite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Cut release branch in June 2018. Release in August 2018.

## Libraries Status


See Libraries? and [Migration/8.6](migration/8.6).

## Release highlights (planned)


Below are the major highlights of 8.6.

- Improved support for **cross-compilation** (Moritz Angermann)

### Build system and miscellaneous changes

- Improved Windows support, including support for split sections, a new IOCP-based I/O manager, and long file paths (Tamar Christina)

- Support for building stating libraries for elf and mach-o (`-staticlib`)

## Landed in `master` branch

- Deriving via ([ proposal](https://github.com/Icelandjack/ghc-proposals/blob/239cfc8ef532db95f15ea392e073061f04273d8e/proposals/0000-deriving-via.rst), Ryan GL Scott)

- An early version of the GHCi `:doc` command

- [QuantifiedConstraints](quantified-constraints)

- The core functionality of the `ghc-heap-view` package has been merged into GHC, allowing introspection into the structure of GHC's heap. (Patrick Dougherty, [ Phab:D3055](https://phabricator.haskell.org/D3055))

- Many improvements to exhausiveness checking ([\#14546](https://gitlab.haskell.org//ghc/ghc/issues/14546))

- Valid hole fits ([\#14969](https://gitlab.haskell.org//ghc/ghc/issues/14969), [\#14990](https://gitlab.haskell.org//ghc/ghc/issues/14990), [\#10946](https://gitlab.haskell.org//ghc/ghc/issues/10946))

- Improvements in code generation, include a (often more efficient) new SRT representation

- Further improvements to DWARF unwinding support

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

## Tickets slated for 8.6.1

### merge/patch/upstream

## Status: merge (2 matches)

<table><tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#15552](https://gitlab.haskell.org//ghc/ghc/issues/15552)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Infinite loop/panic with an existential type.](https://gitlab.haskell.org//ghc/ghc/issues/15552)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr>
<tr><th>[\#15694](https://gitlab.haskell.org//ghc/ghc/issues/15694)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC panic from pattern synonym, "Type-correct unfilled coercion hole"](https://gitlab.haskell.org//ghc/ghc/issues/15694)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr>
<tr><th>## Status: patch (3 matches)

</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#14341](https://gitlab.haskell.org//ghc/ghc/issues/14341)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Show instance for TypeReps is a bit broken](https://gitlab.haskell.org//ghc/ghc/issues/14341)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D4084](https://phabricator.haskell.org/D4084), [ Phab:D5080](https://phabricator.haskell.org/D5080)</th>
<th></th></tr>
<tr><th>[\#15538](https://gitlab.haskell.org//ghc/ghc/issues/15538)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC boot script can't handle Git remote not named origin](https://gitlab.haskell.org//ghc/ghc/issues/15538)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D5077](https://phabricator.haskell.org/D5077)</th>
<th>ChaiTRex</th></tr>
<tr><th>[\#15594](https://gitlab.haskell.org//ghc/ghc/issues/15594)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[--abi-hash with Backpack incorrectly loads modules from dependent packages](https://gitlab.haskell.org//ghc/ghc/issues/15594)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>[ Phab:D5123](https://phabricator.haskell.org/D5123)</th>
<th></th></tr>
<tr><th>## Status: upstream (1 match)

</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ticket (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: id)</th>
<th>Type (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, desc: 1, order: priority)</th>
<th>Differential Rev(s) (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: differential)</th>
<th>Owner (Ticket query: status: merge, status: patch, status: upstream, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: differential, col: owner, order: owner)</th></tr>
<tr><th>[\#15494](https://gitlab.haskell.org//ghc/ghc/issues/15494)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cannot install GHC through stack on NixOS](https://gitlab.haskell.org//ghc/ghc/issues/15494)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th></th></tr></table>

### new

## Status: new (76 matches)

<table><tr><th>Ticket (Ticket query: status: new, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: new, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: new, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#15541](https://gitlab.haskell.org//ghc/ghc/issues/15541)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[package environment files and the GHC API](https://gitlab.haskell.org//ghc/ghc/issues/15541)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#15084](https://gitlab.haskell.org//ghc/ghc/issues/15084)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Functions in HsUtils don't have the most general type](https://gitlab.haskell.org//ghc/ghc/issues/15084)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15275](https://gitlab.haskell.org//ghc/ghc/issues/15275)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[AArch64 validation fails with many invalid relocations](https://gitlab.haskell.org//ghc/ghc/issues/15275)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15287](https://gitlab.haskell.org//ghc/ghc/issues/15287)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[T11627\[ab\] fail on some Darwin environments](https://gitlab.haskell.org//ghc/ghc/issues/15287)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15304](https://gitlab.haskell.org//ghc/ghc/issues/15304)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Huge increase of compile time and memory use from 8.0.2 to 8.2.2 or 8.4.2](https://gitlab.haskell.org//ghc/ghc/issues/15304)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>tdammers</th></tr>
<tr><th>[\#15350](https://gitlab.haskell.org//ghc/ghc/issues/15350)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[gcdExtInteger violates assertion](https://gitlab.haskell.org//ghc/ghc/issues/15350)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15567](https://gitlab.haskell.org//ghc/ghc/issues/15567)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[security of package environment files](https://gitlab.haskell.org//ghc/ghc/issues/15567)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#15399](https://gitlab.haskell.org//ghc/ghc/issues/15399)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Build failure on PowerPC 64-bit big endian](https://gitlab.haskell.org//ghc/ghc/issues/15399)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>trommler</th></tr>
<tr><th>[\#15493](https://gitlab.haskell.org//ghc/ghc/issues/15493)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Elide empty dictionaries](https://gitlab.haskell.org//ghc/ghc/issues/15493)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15498](https://gitlab.haskell.org//ghc/ghc/issues/15498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[HPC: do notation marks () as non-covered](https://gitlab.haskell.org//ghc/ghc/issues/15498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15504](https://gitlab.haskell.org//ghc/ghc/issues/15504)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-XStrict doesn't prevent warnings from -Wunbanged-strict-patterns](https://gitlab.haskell.org//ghc/ghc/issues/15504)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15512](https://gitlab.haskell.org//ghc/ghc/issues/15512)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Rewrite rules should be able to produce custom compiler errors](https://gitlab.haskell.org//ghc/ghc/issues/15512)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15513](https://gitlab.haskell.org//ghc/ghc/issues/15513)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[How to pass "-hide-all-packages" to the GHC API?](https://gitlab.haskell.org//ghc/ghc/issues/15513)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15521](https://gitlab.haskell.org//ghc/ghc/issues/15521)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Provide a strict version of sum](https://gitlab.haskell.org//ghc/ghc/issues/15521)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15522](https://gitlab.haskell.org//ghc/ghc/issues/15522)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cannot bind symbolic names in a rule](https://gitlab.haskell.org//ghc/ghc/issues/15522)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15524](https://gitlab.haskell.org//ghc/ghc/issues/15524)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Performance regression when using the GHC API to evaluate code compared to 8.4](https://gitlab.haskell.org//ghc/ghc/issues/15524)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15531](https://gitlab.haskell.org//ghc/ghc/issues/15531)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[CApiFFI generates bad prototypes for pointers of \`Foreign.C\` types](https://gitlab.haskell.org//ghc/ghc/issues/15531)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15532](https://gitlab.haskell.org//ghc/ghc/issues/15532)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Relaxing Levity-Polymorphic Binder Check for Lifted vs Unlifted pointers](https://gitlab.haskell.org//ghc/ghc/issues/15532)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15533](https://gitlab.haskell.org//ghc/ghc/issues/15533)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Access the number of bits in the target machine's Int type at compile time](https://gitlab.haskell.org//ghc/ghc/issues/15533)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15540](https://gitlab.haskell.org//ghc/ghc/issues/15540)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi does not follow the XDG Base Directory Specification](https://gitlab.haskell.org//ghc/ghc/issues/15540)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15546](https://gitlab.haskell.org//ghc/ghc/issues/15546)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Display coaxiom branch incompatibilities in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/15546)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>mniip</th></tr>
<tr><th>[\#15547](https://gitlab.haskell.org//ghc/ghc/issues/15547)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[A function \`nat2Word\# :: KnownNat n =\> Proxy\# n -\> Word\#\`](https://gitlab.haskell.org//ghc/ghc/issues/15547)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15553](https://gitlab.haskell.org//ghc/ghc/issues/15553)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC.IO.Encoding not flushing partially converted input](https://gitlab.haskell.org//ghc/ghc/issues/15553)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15554](https://gitlab.haskell.org//ghc/ghc/issues/15554)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[COMPLETE pragmas make overlapping-patterns warnings behave oddly](https://gitlab.haskell.org//ghc/ghc/issues/15554)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15557](https://gitlab.haskell.org//ghc/ghc/issues/15557)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Reduce type families in equations' RHS when testing equation compatibility](https://gitlab.haskell.org//ghc/ghc/issues/15557)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15561](https://gitlab.haskell.org//ghc/ghc/issues/15561)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[TypeInType: Type error conditioned on ordering of GADT and type family definitions](https://gitlab.haskell.org//ghc/ghc/issues/15561)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15562](https://gitlab.haskell.org//ghc/ghc/issues/15562)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\`-XStrict -XNoStrict\` is not neutral](https://gitlab.haskell.org//ghc/ghc/issues/15562)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15565](https://gitlab.haskell.org//ghc/ghc/issues/15565)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[ancient ghc release history on web page is incomplete](https://gitlab.haskell.org//ghc/ghc/issues/15565)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15570](https://gitlab.haskell.org//ghc/ghc/issues/15570)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Core transformations generate bad indexCharOffAddr\# call](https://gitlab.haskell.org//ghc/ghc/issues/15570)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>alpmestan</th></tr>
<tr><th>[\#15573](https://gitlab.haskell.org//ghc/ghc/issues/15573)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make bindings with multiple occurrences a join point instead of duplicating code during inlining.](https://gitlab.haskell.org//ghc/ghc/issues/15573)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15574](https://gitlab.haskell.org//ghc/ghc/issues/15574)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[C wrappers for Haskell foreign exports don't have finalizers (causes memory leak).](https://gitlab.haskell.org//ghc/ghc/issues/15574)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15576](https://gitlab.haskell.org//ghc/ghc/issues/15576)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Hadrian puts its build tree in the wrong place](https://gitlab.haskell.org//ghc/ghc/issues/15576)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15580](https://gitlab.haskell.org//ghc/ghc/issues/15580)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Specialize min/max functions for GHC provided instances.](https://gitlab.haskell.org//ghc/ghc/issues/15580)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15582](https://gitlab.haskell.org//ghc/ghc/issues/15582)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Phabricator shows "drafts" by default](https://gitlab.haskell.org//ghc/ghc/issues/15582)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>bgamari</th></tr>
<tr><th>[\#15587](https://gitlab.haskell.org//ghc/ghc/issues/15587)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[traceEvent tests failing in slow validate](https://gitlab.haskell.org//ghc/ghc/issues/15587)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>osa1</th></tr>
<tr><th>[\#15588](https://gitlab.haskell.org//ghc/ghc/issues/15588)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic when abusing kind inference](https://gitlab.haskell.org//ghc/ghc/issues/15588)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15589](https://gitlab.haskell.org//ghc/ghc/issues/15589)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Always promoting metavariables during type inference may be wrong](https://gitlab.haskell.org//ghc/ghc/issues/15589)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15595](https://gitlab.haskell.org//ghc/ghc/issues/15595)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Stack overflow in withArgs leads to infinite memory-consuming loop](https://gitlab.haskell.org//ghc/ghc/issues/15595)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15602](https://gitlab.haskell.org//ghc/ghc/issues/15602)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[PAP invariant of pointer tagging does not hold in profiling builds](https://gitlab.haskell.org//ghc/ghc/issues/15602)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15603](https://gitlab.haskell.org//ghc/ghc/issues/15603)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ref6 example from StaticPointers documentation doesn't type check](https://gitlab.haskell.org//ghc/ghc/issues/15603)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15605](https://gitlab.haskell.org//ghc/ghc/issues/15605)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Documentation of atomicModifyMutVar\# does not show properly](https://gitlab.haskell.org//ghc/ghc/issues/15605)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15606](https://gitlab.haskell.org//ghc/ghc/issues/15606)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Don't float out lets in between lambdsa](https://gitlab.haskell.org//ghc/ghc/issues/15606)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15612](https://gitlab.haskell.org//ghc/ghc/issues/15612)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Got Unable to commit 16777216 bytes of memory error on Ubuntu](https://gitlab.haskell.org//ghc/ghc/issues/15612)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15616](https://gitlab.haskell.org//ghc/ghc/issues/15616)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Bug when using TimerManager/GHC.Event ?](https://gitlab.haskell.org//ghc/ghc/issues/15616)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15617](https://gitlab.haskell.org//ghc/ghc/issues/15617)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Unboxed tuples/sum error message on \`a = show 5\` in expression evaluation and interactive modes](https://gitlab.haskell.org//ghc/ghc/issues/15617)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>JulianLeviston</th></tr>
<tr><th>[\#15619](https://gitlab.haskell.org//ghc/ghc/issues/15619)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[List comprehension seems to prevent some rewrite rules to fire](https://gitlab.haskell.org//ghc/ghc/issues/15619)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15620](https://gitlab.haskell.org//ghc/ghc/issues/15620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Speed up Data.Unique](https://gitlab.haskell.org//ghc/ghc/issues/15620)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15622](https://gitlab.haskell.org//ghc/ghc/issues/15622)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Generalize \`E{0,1,2,3,6,9,12}\` from \`Data.Fixed\`](https://gitlab.haskell.org//ghc/ghc/issues/15622)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rockbmb</th></tr>
<tr><th>[\#15626](https://gitlab.haskell.org//ghc/ghc/issues/15626)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Optimise wakeups for STM](https://gitlab.haskell.org//ghc/ghc/issues/15626)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15632](https://gitlab.haskell.org//ghc/ghc/issues/15632)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Undependable Dependencies](https://gitlab.haskell.org//ghc/ghc/issues/15632)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15639](https://gitlab.haskell.org//ghc/ghc/issues/15639)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Surprising failure combining QuantifiedConstraints with Coercible](https://gitlab.haskell.org//ghc/ghc/issues/15639)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15646](https://gitlab.haskell.org//ghc/ghc/issues/15646)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghci takes super long time to find the type of large fractional number](https://gitlab.haskell.org//ghc/ghc/issues/15646)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>JulianLeviston</th></tr>
<tr><th>[\#15650](https://gitlab.haskell.org//ghc/ghc/issues/15650)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add (or document if already exist) ability to derive custom typeclasses via source plugins](https://gitlab.haskell.org//ghc/ghc/issues/15650)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15655](https://gitlab.haskell.org//ghc/ghc/issues/15655)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Simpliify tcTyConScopedTyVars](https://gitlab.haskell.org//ghc/ghc/issues/15655)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15657](https://gitlab.haskell.org//ghc/ghc/issues/15657)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Support promotion of pattern synonyms to kinds](https://gitlab.haskell.org//ghc/ghc/issues/15657)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15661](https://gitlab.haskell.org//ghc/ghc/issues/15661)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Nullary constraint in GHCi breaks \`:t\` command](https://gitlab.haskell.org//ghc/ghc/issues/15661)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15663](https://gitlab.haskell.org//ghc/ghc/issues/15663)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[T9675 inexplicably regressed in allocations due to text submodule bump](https://gitlab.haskell.org//ghc/ghc/issues/15663)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>alpmestan</th></tr>
<tr><th>[\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Break up the stable pointer table](https://gitlab.haskell.org//ghc/ghc/issues/15665)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15668](https://gitlab.haskell.org//ghc/ghc/issues/15668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Allocations values for some compile tests are way too hight](https://gitlab.haskell.org//ghc/ghc/issues/15668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15670](https://gitlab.haskell.org//ghc/ghc/issues/15670)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[FloatFnInverses seems to show some weird rounding/precision issues.](https://gitlab.haskell.org//ghc/ghc/issues/15670)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15672](https://gitlab.haskell.org//ghc/ghc/issues/15672)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Flags missing documentation.](https://gitlab.haskell.org//ghc/ghc/issues/15672)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15674](https://gitlab.haskell.org//ghc/ghc/issues/15674)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[GADT's displayed type is misleading](https://gitlab.haskell.org//ghc/ghc/issues/15674)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15676](https://gitlab.haskell.org//ghc/ghc/issues/15676)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Users guide: broken external links](https://gitlab.haskell.org//ghc/ghc/issues/15676)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15679](https://gitlab.haskell.org//ghc/ghc/issues/15679)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Use String rather than \[Char\] where possible](https://gitlab.haskell.org//ghc/ghc/issues/15679)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15680](https://gitlab.haskell.org//ghc/ghc/issues/15680)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Flag for printing absolute paths in diagnostics](https://gitlab.haskell.org//ghc/ghc/issues/15680)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15682](https://gitlab.haskell.org//ghc/ghc/issues/15682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[evolve / improve Native Gen Format in Format.hs (especially in context of post simd cleanup)](https://gitlab.haskell.org//ghc/ghc/issues/15682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15683](https://gitlab.haskell.org//ghc/ghc/issues/15683)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[coerce fails for Coercible type families](https://gitlab.haskell.org//ghc/ghc/issues/15683)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15684](https://gitlab.haskell.org//ghc/ghc/issues/15684)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Add tests for SIMD loads and stores](https://gitlab.haskell.org//ghc/ghc/issues/15684)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15687](https://gitlab.haskell.org//ghc/ghc/issues/15687)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Type synonym unused binds no warning?](https://gitlab.haskell.org//ghc/ghc/issues/15687)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15689](https://gitlab.haskell.org//ghc/ghc/issues/15689)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[s390x builds flood with -Wunused-label warnings](https://gitlab.haskell.org//ghc/ghc/issues/15689)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15693](https://gitlab.haskell.org//ghc/ghc/issues/15693)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Abstracting out pattern into a pattern synonym fails with scary error](https://gitlab.haskell.org//ghc/ghc/issues/15693)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15697](https://gitlab.haskell.org//ghc/ghc/issues/15697)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Typed holes inferring a more polymorphic type](https://gitlab.haskell.org//ghc/ghc/issues/15697)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15699](https://gitlab.haskell.org//ghc/ghc/issues/15699)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Run sanity checker in more testsuite runs](https://gitlab.haskell.org//ghc/ghc/issues/15699)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>bgamari</th></tr>
<tr><th>[\#15705](https://gitlab.haskell.org//ghc/ghc/issues/15705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Confusing parser error in 8.6](https://gitlab.haskell.org//ghc/ghc/issues/15705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15707](https://gitlab.haskell.org//ghc/ghc/issues/15707)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[More liberally kinded coercions for newtypes](https://gitlab.haskell.org//ghc/ghc/issues/15707)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15698](https://gitlab.haskell.org//ghc/ghc/issues/15698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[SingleEntry update flag for Stg bindings is not used](https://gitlab.haskell.org//ghc/ghc/issues/15698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr></table>

### infoneeded

## Status: infoneeded (3 matches)

<table><tr><th>Ticket (Ticket query: status: infoneeded, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: status: infoneeded, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: infoneeded, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: infoneeded, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: infoneeded, milestone: 8.6.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#15485](https://gitlab.haskell.org//ghc/ghc/issues/15485)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC uses 300% CPU when calling into blocking C call](https://gitlab.haskell.org//ghc/ghc/issues/15485)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15516](https://gitlab.haskell.org//ghc/ghc/issues/15516)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghci: dynamic linking fails on osx](https://gitlab.haskell.org//ghc/ghc/issues/15516)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15542](https://gitlab.haskell.org//ghc/ghc/issues/15542)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[DuplicateRecordFields not honored within a data family?](https://gitlab.haskell.org//ghc/ghc/issues/15542)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr></table>