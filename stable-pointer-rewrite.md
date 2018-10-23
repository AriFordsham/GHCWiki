## Goals

1. Collect the stable pointer table by generation (resolving [\#7670](https://gitlab.haskell.org//ghc/ghc/issues/7670)). At present, every minor collection needs to inspect every cell in the stable pointer table. Adding insult to injury, that even includes cells that haven't been occupied since the last major collection or ever.

1. Avoid the indirection and pauses inherent in the current array-doubling mechanism (see [\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)), turning a stable pointer into a pointer directly to the relevant cell.

1. Keep the stable pointer table approximately as compact as it is today. When the table is large (and has not shrunk from its maximum historical size), we should ensure that (approximately) half the space used is devoted to actual live pointers.

1. Reduce the synchronization overhead inherent to the current array-doubling mechanism (again see [\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)).

1. To the extent that we can do so efficiently, make the system lock-free. Two potential advantages:

  1. Non-Haskell threads can delete stable pointers during garbage collection.
  1. If a non-Haskell thread is killed while it is deleting a stable pointer, the Haskell runtime will survive.

## Overview of proposed system


There are several different areas and levels of organization we need to handle. The stable pointer table needs to be broken into pieces, and there are things that have to be handled per-capability and per-generation. I'll try to explain things in a comprehensible order, but some things may not make sense in any one linear reading.


I will assume the stable pointer table is broken into "blocks" (which may be smaller than those provided by the block allocator) consisting of one or more "segments". The segment is the most important unit; blocks let us reduce the amount of extra space we need for GC.

### The segment


Each segment contains:

1. No more than `WORDSIZE` entries (probably a few less than that for alignment reasons).
1. A "free list" bitmap indicating which entries in the segment are currently free.
1. A "next-free-enough" pointer (see below).

### The block


Each block contains:

1. One or (most likely) more segments.
1. For each GC generation, a pointer to the next block in that generation (or null if this block is not in the generation).


A block is considered to be in a generation if at least one of its segments contains at least one (non-free) entry pointing to an object in that generation.

### Per-capability pieces

1. The active segment. Each capability allocates stable pointers into one segment (its active segment) at a time. Any given segment can be the active segment for at most one capability at a time.
