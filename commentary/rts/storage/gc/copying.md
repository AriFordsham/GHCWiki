# Copying GC


GHC uses copying GC by default, while it requires more memory than mark/compact?, it is faster.


The basic copying scheme is [ Cheney's Algorithm](http://en.wikipedia.org/wiki/Cheney%27s_algorithm).  Starting from the [roots](commentary/rts/storage/gc/roots), we visit each live object:

- The object is *evacuated* (copied) to its destination generation.   The destination is given by `bd->dest` pointer in the `bdescr` of the
  block in which it lives; typically an object is promoted to the next highest generation, but the basic policy is affected by  [aging](commentary/rts/storage/gc/aging) and [eager promotion](commentary/rts/storage/gc/eager-promotion).

- The header word of the original object is replaced by a *forwarding pointer*.  The forwarding pointer is just the pointer to the new copy, with the least significant bit set to 1 so that forwarding pointers can be distinguished from info table pointers.

- We scan objects that have been evacuated, and *scavenge* each one.  Scavenging involves evacuating each of the pointers
  in the object, replacing each pointer with a pointer to the evacuated copy.

- When there are no more objects to be scavenged, the algorithm is complete.  The memory containing the evacuated objects is retained, all the memory containing the old objects and forwarding pointers is discarded.


Evacuation is implemented in the file [rts/sm/Evac.c](/trac/ghc/browser/ghc/rts/sm/Evac.c).

Scavenging is implemented in the file [rts/sm/Scav.c](/trac/ghc/browser/ghc/rts/sm/Scav.c).



The principle APIs are


<table><tr><th><tt>void evacuate (StgClosure **p)</tt></th>
<td>
which evacuates the object pointed to by the pointer at <tt>p</tt>, and updates <tt>p</tt> to point to the new location.
</td></tr></table>


<table><tr><th><tt>void scavenge_block (bdescr *bd)</tt></th>
<td>
which scavenges all the objects in the block <tt>bd</tt> (objects between <tt>bd->u.scan</tt> and <tt>bd->free</tt> are assumed to
be unscavenged so far).
</td></tr></table>


