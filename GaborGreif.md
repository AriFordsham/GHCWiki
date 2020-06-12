(I was also `heisenbug` in the Trac)

Agenda:

- Pointer tagging for big families (#14373)
  - [ ] add to wiki
  - [ ] perf implications
  - [ ] cleanup patches (strictness?, better `partition`: !2321)
  - [ ] improve `#dataToTag` [see](https://gitlab.haskell.org/ghc/ghc/commit/ac977688523e5d77eb6f041f043552410b0c21da#note_241836)
  - [ ] figure out why the CI had an allocation bump (which went away)
- Reusing memory congruent objects in STG (#13861)
- pick up some work from HaL (Darwin dead-code-stripping avoidance optimisation)

Memory optimisations
- avoid having more than 7 tag appliers per compilation unit (`-O2`). These probably arise for each datatype now, and get invoked when a constructor is entered (i.e. never?).

Code size optimisations
- closure allocation cold path: don't pass the entry point, but just jump to the GC handler, and compute the return address from there. Also jump back to the place where the `Hp` is already incremented, behind the `HpLim` check.

- stack allocation cold path, same issue, but this is already pretty compact...

- very repetitive assembly:

``` haskell
module AoC6 (orbits) where

orbits = [

	"S5F)4L5",
	"7BP)2V1",
	"DHC)KGY",
	"JZG)PN7",
	"RV4)123",
	"2DJ)LPW",
	"19G)31B",
	"H4Z)TC6",
      ]
```