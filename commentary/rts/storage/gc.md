# The Garbage Collector


GC concepts:

- [Aging](commentary/rts/storage/gc/aging)
- [Pinned objects](commentary/rts/storage/gc/pinneed)


GC algorithms supported:

- [Copying GC](commentary/rts/storage/gc/copying)
- Parallel GC?
- Marking? (for compaction or sweeping)
- Compaction?
- Sweeping? (for mark-region GC)

## GC overview


The GC is designed to be flexible, supporting lots of ways to tune its behaviour.  Here's an overview of the techniques we use:

- Generational GC, with a runtime-selectable number of generations (`+RTS -G<n> -RTS`, where `n >= 1`).  Currently it is a
  traditional generational collector where each collection collects a particular generation and all younger generations.
  Generalizing this such that any subset of generations can be collected is a possible future extension.

- The heap grows on demand.  This is straightforwardly implemented by basing the whole storage manager on a [block allocator](commentary/rts/storage/block-alloc).

- Aging: objects can be aged within a generation, to avoid premature promotion.  See [Commentary/Rts/Storage/GC/Aging](commentary/rts/storage/gc/aging).  In GHC 6.12 and earlier this was implemented by having each generation consist of a runtime-tunable number of *steps*, so objects would be moved through the list of steps before being promoted to the next highest generation.  We found that having 2 steps was almost always the best *integral* number of steps, but GHC 6.12 did not support a fractional number of steps.  In GHC 6.13 and later, the concept of steps was removed.  Aging is still supported, by having each block point to the generation to which objects in that block should be promoted; this lets us provide any fractional number of steps between 1 and 2, while eliminating the infrastructural overhead of steps.

- The heap collection policy is runtime-tunable.  You select how large a generation gets before it is collected using the `+RTS -F<n> -RTS` option, where `<n>` is a factor of the generation's size the last time it was collected.  The default value is 2, that is a generation is allowed to double in size before being collected.

## GC data structures

[includes/rts/storage/GC.h](/trac/ghc/browser/ghc/includes/rts/storage/GC.h)

### generation


The main data structure is `generation`, which contains:

<table><tr><th>`blocks`</th>
<td>
a pointer to a list of blocks
</td></tr></table>

<table><tr><th>`large_objects`</th>
<td>
a pointer to a list of blocks containing large objects
</td></tr></table>

<table><tr><th>`threads`</th>
<td>
a list of threads in this generation
</td></tr></table>

<table><tr><th>`mut_list`</th>
<td>
the "remembered set", a list of blocks containing pointers to objects in *this* generation that point to objects in *younger* generations
</td></tr></table>


and various other administrative fields (see [includes/rts/storage/GC.h](/trac/ghc/browser/ghc/includes/rts/storage/GC.h) for the details).


Generations are kept in the array `generations[]`, indexed by the generation number.

### nursery


A `nursery` is a list of blocks into which the mutator allocates new (small) objects.  For resaons of locality, we want to re-use the list of blocks for the nursery after each GC, so we keep the nursery blocks rather than freeing and re-allocating a new nursery after GC.


The struct `nursery` contains only two fields

<table><tr><th>`blocks`</th>
<td>
the list of blocks in this nursery
</td></tr>
<tr><th>`n_blocks`</th>
<td>
the number of blocks in the above list
</td></tr></table>


In the threaded RTS, there is one nursery per Capability, as each Capability allocates independently into its own allocation area.  Nurseries are therefore stored in an array `nurseries[]`, indexed by Capability number.


The blocks of the nursery notionally logically to generation 0, although they are not kept on the list `generations[0].blocks`.  The reason is that we want to keep the actual nursery blocks separate from any blocks containing live data in generation 0.  Generation 0 may contain live data for two reasons:

- objects live in the nursery are not promoted to generation 1 immediately, instead they are [aged](commentary/rts/storage/gc/aging), first being copied to generation 0, and then being promoted to generation 1 in the next GC cycle if they are still alive.

- If there is only one generation (generation 0), then live objects in generation 0 are retained in generation 0 after a GC.
