# Remembered Sets


Since in generational GC we may need to find all the live objects in a young generation without traversing the older generation(s), we need a record of the pointers from those old generations into the young generations.  This is termed the "remembered set".  


In GHC each `generation` structure contains a field `mut_list`, which points to a chain of blocks.  Each block in the chain contains a list of pointers to objects in that generation which contain pointers to objects in younger generations.  There are alternative schemes, e.g.

- Keeping track of each *pointer*, rather than *object* that points to a younger generation.  The remembered set would
  be larger (possibly very much larger, in the case of arrays), but it would be more accurate, and traversing the
  remembered set at GC time would be faster.

- Some GCs use "card-marking" schemes whereby the heap is divided into "cards" of a fixed size, and each card has a bit to
  indicate whether that card contains pointers to a younger generation.  This is much less accurate than a remembered set,
  but it is faster at runtime if a lot of mutation is taking place, and it takes less space than a remembered set.  In GHC
  we typically do not have much mutation to worry about, so card marking would be a poor compromise in our case.


The remembered set may contain duplicates, or it may contain pointers to objects that don't really point to young generations.

## Remembered set maintenance during mutation


While the mutator is running, we have to add any old-to-new generation pointers that are created.  Old-to-new pointers are created by mutating (writing to) an object in the old generation, and catching these writes is called a "write barrier".  


A pointer can be added to a remembered set using 

```c
void recordMutableCap (StgClosure *p, Capability *cap, nat gen);
```


This adds the pointer `p` to the remembered set for generation `gen`, using Capability `cap`.  Each Capability has its own remembered set for each generation, so that when running in parallel we can update remembered sets without taking a lock, and also so that we can take advantage of locality in the GC, by traversing a remembered set on the same CPU that created it.


Here are the cases where we need a write barrier in GHC:

### Thunk Updates


Updating a thunk in an old generation.  This is taken care of by the update code, see [rts/Updates.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Updates.h).

### Mutable objects: MUT_VAR, MVAR


For `MUT_VAR`, the writer must call `dirty_MUT_VAR`:

```c
void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);
```


(in [rts/sm/Storage.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Storage.c)).  The code generator inserts calls to `dirty_MUT_VAR` when it compiles a call to the primitive `writeMutVar#`.

`dirty_MUT_VAR` does the following: if the object's header is `MUT_VAR_CLEAN`, then the header is set to `MUT_VAR_DIRTY`, and the object is added to the remembered set if it resides in an old generation.  If the header was already `MUT_VAR_DIRTY`, no action
is taken.

`MVAR` is handled in the same way, with 

```c
void dirty_MVAR(StgRegTable *reg, StgClosure *p)
```

### Arrays: MUT_ARR_PTRS


Unlike mutable variables and MVARs, mutable arrays are kept in the remembered set permanently.  This reflects the fact that mutable arrays are likely to be written to more often, and there are likely to be fewer of them.  However, we still mark arrays according to whether the array is dirty or not, using `MUT_ARR_PTRS_DIRTY` and `MUT_ARR_PTRS_CLEAN`.  


There are also `MUT_ARR_PTRS_FROZEN` and `MUT_ARR_PTRS_FROZEN0`, which are used to indicate arrays that have been frozen using `unsafeFreezeArray#`.  A frozen array is different from a mutable array in the sense that while it may have old-to-new pointers, it is not going to be mutated any further, and so we probably want to use [eager promotion](commentary/rts/storage/gc/eager-promotion) on it.

### Threads: TSO


Threads (TSOs) have stacks, which are by definition mutable.  Running a thread is therefore an act of mutation, and if the
thread resides in an old generation, it must be placed in the remembered set.  Threads have two dirty bits: `tso->dirty`
is set to non-zero if the thread's stack or any part of the TSO structure may be dirty, and also there is a bit
`TSO_LINK_DIRTY` in `tso->flags` which is set if the TSO's link field may be dirty.  If the thread is executed,
then `dirty_TSO()` must be called in order to set the `tso->dirty` bit and add the TSO to the appropriate remembered set.

```c
void dirty_TSO (Capability *cap, StgTSO *tso);
```


To set the TSO's link field, use `setTSOLink()` (from [rts/sm/Storage.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Storage.c)) which arranges to add the TSO to
the remembered set if necessary.

```c
void setTSOLink (Capability *cap, StgTSO *tso, StgTSO *target);
```


there are a few exceptions where `setTSOLink()` does not need to be called; see [rts/sm/Storage.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Storage.c) for details.

## Remembered set maintenance during GC


During GC, the principle of write barriers is quite similar: whenever we create an old-to-new pointer, we have to record it in the remembered set.  The GC achieves this as follows:

- The GC thread structure has a field `gct->evac_gen` which specifies the desired destination generation.
- there is a flag `gct->failed_to_evac`, which is set to true by `evacuate` if it did not manage to evacuate
  the object into the desired generation.
- after scavenging an object, `scavenge_block` checks the `failed_to_evac` flag, and if it is set, adds the object to the remembered set, using `recordMutableGen_GC()` (the equivalent of `recordMutableCap` for calling within the GC).
