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

- very repetitive assembly (https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3012) :

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

- horrors like this:
``` asm
     142: 0f 85 00 00 00 00             jne     0 <_Lr89Q_info+0x28>
     148: ff 23                         jmpq    *(%rbx)
```
More than 780 bytes wasted:
```
$ llvm-objdump _build/stage1/compiler/build/GHC/Stg/CSE.o -d | grep ': 0f 85 00 00 00 00' | wc -l
131
$ ls -l _build/stage1/compiler/build/GHC/Stg/CSE.o
-rw-r--r-- 1 ggreif staff 84356 Jun 12 13:52 _build/stage1/compiler/build/GHC/Stg/CSE.o
```
almost 1%!

Turns out these are relocations! :-/

- same file (`CSE.o`)
```
    84ac: 4c 8d 05 03 00 00 00         	leaq	3(%rip), %r8
    84b3: 48 8d 3d 03 00 00 00         	leaq	3(%rip), %rdi
    84ba: 48 8d 35 01 00 00 00         	leaq	1(%rip), %rsi
```
why not
```
    84ac: 4c 8d 05 03 00 00 00         	leaq	3(%rip), %r8
                                        add c1 %r8, %rdi
                                        add c2 %r8, %rsi (or: add c3 %rdi, %rsi)
```