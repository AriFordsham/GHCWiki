# GHC Commentary: Garbage Collecting CAFs


Files: [rts/sm/GC.c](/trac/ghc/browser/ghc/rts/sm/GC.c)


Constant Applicative Forms, or CAFs for short, are top-level values defined in a program. 
To avoid memory leaks caused by CAFs we need to detect when all values/functions that could potentially refer to a CAF are gone, and so it is safe to deallocate the CAF.  


To achieve this, during GC we maintain a linked list of static objects that are still live.
Closure that might refer to CAFs contains a Satic Reference Table (SRT) which indicates what
CAFs are still in use by this closure.
