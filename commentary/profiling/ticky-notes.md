
(NB out-of-date, but maybe historically useful; cf [Debugging/TickyTicky](debugging/ticky-ticky))

# Kirsten's sketchy notes on getting ticky to work


Macros for bumping ticky counters are now defined in [includes/Cmm.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/Cmm.h). Currently, code compiled with the `-fticky-ticky` flag fails to link because the macros rely on counter variables (things with names like `ENT_DYN_IND_ctr` being declared, but there are actually no declarations for them. I'll add those declarations to [includes/RtsExternal.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/RtsExternal.h) so I can get something working. Really, there should be something that automatically generates both the macros that are in [includes/Cmm.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/Cmm.h) and the declarations for the corresponding variables, so that they stay in sync.


Actually, maybe it would make more sense to add a new file, `RtsTicky.h` or something, which contains only ticky counter declarations (the same declarations that still exist in [includes/StgTicky.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/StgTicky.h), which isn't used anymore), and that include that from [includes/RtsExternal.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/RtsExternal.h).


No -- put actual declarations for counter variables in another file, `TickyCounters.h` or something, and include that only from [rts/Ticky.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Ticky.c); put *extern* declarations for those counters in `RtsTicky.h`, still included from [includes/RtsExternal.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/RtsExternal.h). Then later we can automatically generate both `RtsTicky.h` and `TickyCounters.h`. The reason for this is that the ticky **macros** are all over the place and they refer to the ticky counters, so the ticky counters have to be **declared** someplace that everyone includes, but of course the actual initializations only need to happen in one place. (Maybe there's a better way to do this...)


No, there don't need to be two files; I was confused. Just `TickyCounters.h`.


Huh - we define ticky macros now in `Cmm.h` but we can only include that in CMM files and some C files, like `Schedule.c`, use ticky macros. This makes my brain hurt a little.
