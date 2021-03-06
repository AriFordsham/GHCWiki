# Replacing the stable pointer mechanism

## Goals

1. Collect the stable pointer table by generation (resolving #7670). At present, every minor collection needs to inspect every cell in the stable pointer table. Adding insult to injury, that even includes entries that haven't been occupied since the last major collection or even ever.

1. Avoid the extra pauses inherent in the current array-doubling mechanism (see #15665), turning a stable pointer into a pointer directly to the relevant entry.

1. Keep the stable pointer table approximately as compact as it is today. When the table is large (and has not shrunk from its maximum historical size), we should ensure that (approximately) half the space used is devoted to actual live pointers.

1. Reduce the synchronization overhead inherent to the current array-doubling mechanism (again see #15665).

1. To the extent that we can do so efficiently, make the system lock-free. Two potential advantages:

  1. Non-Haskell threads can delete stable pointers during garbage collection.
  1. If a non-Haskell thread is killed while it is deleting a stable pointer, the Haskell runtime will survive.

## Overview of proposed system


There are several different areas and levels of organization we need to handle. The stable pointer table needs to be broken into pieces, and there are things that have to be handled per-capability and per-generation. I'll try to explain things in a comprehensible order, but some things may not make sense in any one linear reading.


I will assume the stable pointer table is broken into "blocks" (which may be smaller than those provided by the block allocator) consisting of one or more "segments". The segment is the most important unit; blocks let us reduce the amount of extra space we need for GC.

### The segment


Each segment contains:

1. No more than `WORDSIZE` entries (probably a few less than that for alignment reasons).
1. A "free list" bitmap indicating which entries in the segment are currently free. This bitmap is a fundamental source of truth and is always considered up to date.
1. A "next-free-enough" pointer (see much more below).
1. For each generation, a bitmap of entries in that generation. To align segments nicely, I believe we need to limit the number of per-generation bitmaps to somewhere between 3 and 8 (depending on the number of segments per block and other trade-offs). Assuming there are N per-generation bitmaps, all stable pointer table entries in generations N-1 and older will be inspected when collecting generation N-1. A generation bitmap *may* be out of date:

  1. An entry may be allocated (removed from the free list bitmap) but not yet added to its generation bitmap.
  1. An entry may be deleted but still present in a generation bitmap.  
    However,
  1. All allocations will be complete (added to their generation bitmaps) before the garbage collector runs.
  1. No entry will ever be in more than one generation bitmap at a time.

### The block


Each block contains:

1. One or (most likely) more segments.
1. For each GC generation, a pointer to the next block in that generation (or null if this block is not in the generation).


A block is considered to be in a generation if at least one of its segments contains at least one (non-free) entry pointing to an object in that generation.

### Per-capability pieces

1. The active segment. Each capability allocates stable pointers into one segment (its active segment) at a time. Any given segment can be the active segment for at most one capability at a time. When a capability's active segment is sufficiently full, the capability "relinquishes" the segment.

1. A private copy of the free list bitmap for its active segment that lags behind the true free list. The capability steps through this private copy when allocating entries. When it runs out of entries in the private copy, it makes a fresh private copy and uses that to update the generation lists; this maintains the invariant that an entry is never in more than one generation.

1. A list of blocks in each generation. When GC runs, every block in each generation is in the list for that generation of *exactly one* capability (we don't care which). Once listed as being in a generation, a block is not removed from the generation list until that generation is collected. So a block may be traced unnecessarily, but all necessary blocks will be traced and no block will be traced twice.

1. (Optional) A private list of segments that are "free enough". See below.

### The free-enough list(s)


Once a segment has been filled (or nearly filled), we want to be able to reuse it once enough of its entries have been deleted to justify the administrative and synchronization cost of doing so. To avoid wasting too much space to thread migration and such, we need a *global* free-enough list that all capabilities can use. I imagine this as a classic lock-free many-writers-one-reader list. We have an in-stack and an out-stack. The in-stack is lock-free; the out-stack is controlled by a lock.

### Allocation


A capability allocates an entry in its active segment by choosing an entry from its private copy of the free list bitmap, clearing (fetch-and-add) the relevant bit in the true free list bitmap, clearing the relevant bit in its private copy, setting the relevant bit in the relevant generation bitmap, and populating the entry. Note: we do *not* deal with the list of blocks per generation here; that's delayed until segment relinquishment or GC.


If the active segment is full, the capability relinquishes it (see below). It gets another one from the free-enough list (if that's non-empty) and otherwise allocates a fresh block, taking the first segment for itself and adding the rest to the free-enough list. It loads its private free-list copy and updates the generation bitmaps accordingly.

### Freeing


We free an entry in a segment by setting (fetch-and-add) the relevant bit in the free list bitmap for that segment. We then take the `popCount` of the free list bitmap. If it is precisely at the free-enough threshold, we check the next-free-enough pointer. If that's null (meaning that the segment is not currently the active segment for a capability), we use a CAS loop to add the segment to the free-enough list. Why *precisely* at the free-enough threshold? Just to save a bit of time. One might ask what happens if the thread that deletes at the threshold dies before adding the segment to the free-enough list. In that case, the segment will never be added to the free-enough list, and that space will be lost forever. But that's exactly the same thing that will happen if it dies *while* adding the segment to the free-enough list, so there's nothing to be gained by being conservative.


If we like, we can reduce contention on the free-enough list in-stack by letting each capability maintain a private free-enough list. When that list exceeds a certain length, we use a CAS loop to transfer multiple segments to the global free-enough list at once. We can let non-Haskell threads maintain private free-enough lists as well, flushing them completely when they reach a certain size and when destroyed.


Memory ordering: my understanding of the C memory model is not great, but I'm guessing we *may* want the deletion operation to null out the entry. As I understand it, that (along with release-acquire ordering) should be sufficient to establish a single total ordering of allocation, dereferencing, and deletion of an entry. OTOH, it may not actually be necessary.

### Relinquishing a segment

1. For each generation represented in the segment, ensure that the block is on a list for that generation.
1. Set the next-free-enough pointer to null. As described above, this enables it to be added to the free-enough list once enough of its entries have been freed.
1. Reload the free-list bitmap and follow the procedure described in the freeing section to check whether the segment is now free enough to add to the free-enough list. This is necessary because it's possible (albeit unlikely) that a segment will become full, but then become free-enough (thanks to deletions by another thread) before it is relinquished.

### Garbage collection

1. Before garbage collection, make sure that all active segments with entries in this generation have been added to the appropriate generation list. (I imagine each capability will do this independently as the world stops.)
1. Traverse each of the lists of blocks in this generation. For each of them:

  1. Clear the generation pointer for this generation.
  1. For each segment in the block, combine the (approximate) generation bitmap with the (exact) free list bitmap to get a bitmap of all live entries in this generation. Evacuate each entry. Modify the generation bitmaps as appropriate when an entry moves from this generation to another. In the end, make sure that all represented generations are linked as required. If the current generation is (still) represented, add it to a new list.

### Multiple allocation


We can support efficient allocation of many stable pointers from an array or small array, putting the new stable pointers in a user-provided `MutableByteArray`. The idea is that we can allocate (up to) all the remaining space in an active block in one go, rather than performing a bunch of separately synchronized allocations. We can then let the user know what array slice bounds to give to continue the process with the next segment.
