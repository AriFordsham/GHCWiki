# GHC Commentary: Garbage Collecting CAFs


Files: [rts/sm/GC.c](/trac/ghc/browser/ghc/rts/sm/GC.c), function scavange_srt in [rts/sm/Scav.h](/trac/ghc/browser/ghc/rts/sm/Scav.h)


Constant Applicative Forms, or CAFs for short, are top-level values defined in a program.
Essentially, they are objects that are not allocated dynamically at run-time but, instead,
are part of the static data of the program.  Sometimes, a CAF may refer to many values in the heap.  To avoid memory leaks in such situations, we need to know when a CAF is never going to be used
again, and so we can deallocate the values that it refers to.


See Note \[CAF management\] in [rts/sm/Storage.c](/trac/ghc/browser/ghc/rts/sm/Storage.c) for more information.

## Static Reference Tables


File: [includes/rts/storage/InfoTables.h](/trac/ghc/browser/ghc/includes/rts/storage/InfoTables.h)


The info table of various closures may contain information about what static objects are
references by the closure.  This information is stored in two parts:

1. a static reference table (SRT), which is an array of references to static objects
1. a bitmask which specifies which of the objects are actually used by the closure.


There are two different ways to access this information depending on the size of the SRT:

- "small": if `srt_bitmap` is a small bitmap, not all 1s, then GET_FUN?_SRT contains the SRT.
- "large": if `srt_bitmap` is all 1s, then GET_FUN?_SRT contains a large bitmap, and the actual SRT.

## Evacuating Static Objects


Files: [rts/sm/GCThread.h](/trac/ghc/browser/ghc/rts/sm/GCThread.h), [rts/sm/Evac.c](/trac/ghc/browser/ghc/rts/sm/Evac.c), [rts/sm/GC.c](/trac/ghc/browser/ghc/rts/sm/GC.c)


While scavenging objects, we also process (aka "evacuate") any static objects that need to be kept alive.  When a GC thread discovers a live static object, it places it on its `static_objects`
list.  Later, this list is used to scavange the static objects, potentially finding more live objects.
Note that this process might find more static objects, and thus further extend the `static_objects` list.


When a static object is scavenged, it is removed from `static_objects` and placed on another list, called `scavenged_static_objects`.  Later, we use this list to "clean up" the liveness markers from these static objects, so that we can repeat the process on the next garbage collection.
Note that we can't "clean up" the liveness markers as we go along because we use them to notice
cycles among the static objects.
