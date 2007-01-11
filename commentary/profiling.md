# Profiling


GHC includes at least two types of profiling: cost-centre profiling and ticky-ticky profiling. Ticky-ticky profiling is currently *not working* in the HEAD, but hopefully should be working again soon.


Cost-centre profiling operates at something close to the source level, and ticky-ticky profiling operates at something much closer to the machine level. This means that the two types of profiling are useful for different tasks. Ticky-ticky profiling is mainly meant for compiler implementors, and cost-centre profiling for mortals. However, because cost-centre profiling operates at a high level, it can be difficult (if not impossible) to use it to profile optimized code. Personally, I (Kirsten) have had a lot of success using cost-centre profiling to find problems that were due to my own bad algorithms, but less success once I was fairly sure that I wasn't doing anything obviously stupid and was trying to figure out why my code didn't get optimized as well as it could have been.

## Cost-centre profiling


(add more details)

## Ticky-ticky profiling


(The following are my notes as I try to get this working again. Once it is working, I'll turn it into something more coherent. -krc)


Macros for bumping ticky counters are now defined in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h). Currently, code compiled with the `-fticky-ticky` flag fails to link because the macros rely on counter variables (things with names like `ENT_DYN_IND_ctr` being declared, but there are actually no declarations for them. I'll add those declarations to [includes/RtsExternal.h](/trac/ghc/browser/ghc/includes/RtsExternal.h) so I can get something working. Really, there should be something that automatically generates both the macros that are in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) and the declarations for the corresponding variables, so that they stay in sync.


Actually, maybe it would make more sense to add a new file, RtsTicky.h or something, which contains only ticky counter declarations (the same declarations that still exist in [includes/StgTicky.h](/trac/ghc/browser/ghc/includes/StgTicky.h), which isn't used anymore), and that include that from [includes/RtsExternal.h](/trac/ghc/browser/ghc/includes/RtsExternal.h).
