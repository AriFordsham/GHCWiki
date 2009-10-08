# GHC Commentary: Garbage Collecting CAFs


Files: [rts/sm/GC.c](/trac/ghc/browser/ghc/rts/sm/GC.c)


Constant Applicative Forms, or CAFs for short, are top-level values defined in a program. 
To avoid memory leaks caused by CAFs we need to detect when all values/functions that could potentially refer to a CAF are gone, and so it is safe to deallocate the CAF.  


(???)
To achieve this, all static objects are linked together with the static link field.  During GC we maintain traverse the static objects to see which are still live.
Closures that might refer to CAFs contain a Static Reference Table (SRT) which indicates what
static objects are still needed.

## Static Reference Tables


File: [includes/rts/storage/InfoTables.h](/trac/ghc/browser/ghc/includes/rts/storage/InfoTables.h)


The info table of various closures may contain information about what static objects are
references by the closure.  This information is stored in two parts:

1. a static reference table (SRT), which is an array of references to static objects
1. a bitmask which specifies which of the objects are actually used by the closure.


There are two different ways to access this information depending on the size of the SRT:

- "small": if `srt_bitmask` is a small bitmask, not all 1s, then GET_FUN?_SRT contains the SRT.
- "large": if `srt_bitmask` is all 1s, then GET_FUN?_SRT contains a large bitmap, and the actual SRT.
