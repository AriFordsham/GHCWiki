
Back to [GarbageCollectorNotes](garbage-collector-notes)

## GC Data Structures

### Blocks and Mega Blocks


The GC allocates memory from the OS in 1 Mb sized chunks, called mega blocks, which it then divides into pages and manages itself. Each 4k page is called a block and is associated with a block descriptor (the abbreviation BD is used a lot). The BDs for all the blocks on the are stored at the start of the mega block. 



The BD keeps information about a block. This the BD definition from blocks.h in the RTS.


```
typedef struct bdescr_ {
  StgPtr start;                 /* start addr of memory */
  StgPtr free;                  /* first free byte of memory */
  struct bdescr_ *link;         /* used for chaining blocks together */
  union { 
      struct bdescr_ *back;     /* used (occasionally) for doubly-linked lists*/
      StgWord *bitmap;
  } u;
  unsigned int gen_no;          /* generation */
  struct step_ *step;           /* step */
  StgWord32 blocks;             /* no. of blocks (if grp head, 0 otherwise) */
  StgWord32 flags;              /* block is in to-space */
#if SIZEOF_VOID_P == 8
  StgWord32 _padding[2];
#else
  StgWord32 _padding[0];
#endif
} bdescr;
```


Most of the fields ina BD are self explanatory. Let me add a few quick descriptions however. Each bdescr or BD maintains its start address and a "free" pointer used to indicate the next free location in the block. Each block also knows about what generation it belongs to and has a pointer to the step it belongs to (more about generations and steps later). 


If a large object is allocated and a block is a part of a large object, then the first block is has a count of the number of blocks that are part of the object. The link list of blocks making up the object is maintained by the link pointer. \[This may not be entirely correct - I will come back to this later\].

### Generations


The GHC GC is a generational collector. The number of generations is set to 2 by default and they are referred to as gen 0 and gen 1. gen 0 has newer objects; objects that survive collections in gen 0 are promoted to gen 1. Older objects reside in gen 1. 

### Command Line Switches


The number of generations in an execution of a compiled haskell program can be changed by using the command line switch -G\<n\>, where n is the number of generations. This is an RTS switch and so has to be used as follows - 

```wiki
main.exe +RTS -G5 -RTS 
```


Where main.exe is the compiled program and we want it to have 5 generations. More about the RTS switches can be found here 
[Runtime Control](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html).


On the topic of command line switches, for a compiler hacker this is also interesting - 
[Debugging the compiler](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#compiler-debugging-options)

### Steps


GHC generations are divided into steps. The number of steps per generation is a configurable value as well. The last generation, the oldest one has only one step. By default GHC compiled programs have 2 steps per generation. Since GHC usually has only 2 generations, it has 2 steps in gen 0 and 1 step in gen 1. Steps of a generation are referred to by their index number starting from 0. 


Garbage collection happens at the level of generations. So when you say that you are collecting gen 0, you are essentially collecting all the steps in gen 0. If you are collecting gen 4, then you are collecting all the steps in gen 0 to gen 4. Objects that survive a collection are promoted into the next higher step, or into the next higher generation if the object is already in the highest step of its generation. 


The reasoning behind having steps is approximately this - if there were no steps and a surviving a gen0 collection meant automatic promotion to gen1, then it means that at the time when gen0 happened several very new objects (which are potentially shortlived as well) get promoted to gen1. gen1 collections are expensive since higher generations are larger and hence these objects will reside in memory a long time before they get collected. On the other had if gen0 were divided into two steps then a gen0step0 object hops to gen0setp1 on its first survival and needs to survive yet another GC before it is promoted to gen1. \[This I think is a very neat idea. My current understanding is that the .Net GC does have steps. Is this correct?\]


That said, let us look at the data structures. This is what a generation looks like - 

```wiki
typedef struct generation_ {
  unsigned int   no;			/* generation number */
  step *         steps;			/* steps */
  unsigned int   n_steps;		/* number of steps */
  unsigned int   max_blocks;		/* max blocks in step 0 */
  bdescr        *mut_list;      	/* mut objects in this gen (not G0)*/

  /* temporary use during GC: */
  bdescr        *saved_mut_list;

  /* stats information */
  unsigned int collections;
  unsigned int failed_promotions;
} generation;
```


This is what a step looks like - 

```wiki
typedef struct step_ {
  unsigned int         no;		/* step number */
  bdescr *             blocks;		/* blocks in this step */
  unsigned int         n_blocks;	/* number of blocks */
  struct step_ *       to;		/* destination step for live objects */
  struct generation_ * gen;		/* generation this step belongs to */
  unsigned int         gen_no;          /* generation number (cached) */
  bdescr *             large_objects;	/* large objects (doubly linked) */
  unsigned int         n_large_blocks;  /* no. of blocks used by large objs */
  int                  is_compacted;	/* compact this step? (old gen only) */

  /* During GC, if we are collecting this step, blocks and n_blocks
   * are copied into the following two fields.  After GC, these blocks
   * are freed. */
  bdescr *     old_blocks;	        /* bdescr of first from-space block */
  unsigned int n_old_blocks;		/* number of blocks in from-space */

  /* temporary use during GC: */
  StgPtr       hp;			/* next free locn in to-space */
  StgPtr       hpLim;			/* end of current to-space block */
  bdescr *     hp_bd;			/* bdescr of current to-space block */
  StgPtr       scavd_hp;		/* ... same as above, but already */
  StgPtr       scavd_hpLim;		/*     scavenged.  */
  bdescr *     scan_bd;			/* block currently being scanned */
  StgPtr       scan;			/* scan pointer in current block */
  bdescr *     new_large_objects;    	/* large objects collected so far */
  bdescr *     scavenged_large_objects; /* live large objs after GC (d-link) */
  unsigned int n_scavenged_large_blocks;/* size of above */
  bdescr *     bitmap;  		/* bitmap for compacting collection */
} step;
```


These definitions can be found in rts\\storage.h. 

TODO: The links below need to be updated.

Here is a little diagram of the data structure formed by the generations and steps. The global variable 'generations' is a an array of pointers to generations. Each generation has 'steps' as a pointer array to its steps. 

[http://www.cs.indiana.edu/\~rpjames/HaskellGC/ds/generations-steps.jpg](http://www.cs.indiana.edu/~rpjames/HaskellGC/ds/generations-steps.jpg)



Each step contains a pointer to a link list of blocks that are part of the step.



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/ds/step-blocks.jpg](http://www.cs.indiana.edu/~rpjames/HaskellGC/ds/step-blocks.jpg)


