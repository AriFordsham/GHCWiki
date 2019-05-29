# Allow for multiple instances of the GHCi linker


This page discusses a plan to fix bug #3372. Note that an MR has been merged that fixes the global state issues of the byte code linker (the Haskell portion). The object code linker, however, hasn't been adjusted yet.


## The problem

GHC includes its own linker, used by GHCi to resolve symbols. Now, the linker is composed of two rather different parts: the byte code linker and the object linker, each with its own symbol tables (and, in the case of the object linker, global variables). The latter is part of the RTS, written in C with plenty of \#ifdefs to handle a variety of platforms, object file formats, etc.

Fixing the byte code linker was relatively straight forward. There is now a single copy of the byte code linker's state in an `MVar` for each running instance of GHC. The object linker is much more fragile, however. In particular, it is harder to test since there is a lot of platform-dependent code under conditional compilation.


## Plan for the object linker

The object linker ([rts/Linker.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Linker.c)) is responsible of loading and keeping track of symbols in object files and shared libraries. For object files it basically uses three global variables:


```wiki
/* Hash table mapping symbol names to Symbol */
static /*Str*/HashTable *symhash;

/* Hash table mapping symbol names to StgStablePtr */
static /*Str*/HashTable *stablehash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;	/* initially empty */
```


Each time an object file is loaded, a new `ObjectCode` node is added to the `objects` linked list and `symhash` is populated with a pointer for each symbol.

*Question:* What is `stablehash` used for? 


For shared libraries the code varies with each platform. On Windows a linked list of handles to opened DLLs is stored in a global variable:

```wiki
typedef
   struct _OpenedDLL {
      char*              name;
      struct _OpenedDLL* next;
      HINSTANCE instance;
   }
   OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;
```


To lookup a symbol one has to iterate `opened_dlls` and for each handle, lookup the symbol there.


For the ELF and Mach-O case, libraries are dlopen'd using RTLD_GLOBAL and later accessed using the program's dl-handle. This is stored in:

```wiki
static void *dl_prog_handle;
```


A possible solution would be to put all these variables in a datastructure:

```wiki
typedef struct _ObjLinkerState {
  /* Hash table mapping symbol names to Symbol */
  /*Str*/HashTable *symhash;

  /* Hash table mapping symbol names to StgStablePtr */
  /*Str*/HashTable *stablehash;

  /* List of currently loaded objects */
  ObjectCode *objects = NULL;	/* initially empty */

#if defined(OBJFORMAT_PEi386)
  OpenedDLL* opened_dlls = NULL;
#endif

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
  void *dl_prog_handle;
#endif
} ObjLinkerState;
```


and add to `PersistentLinkerState` a `ForeignPtr` to a malloc'd `ObjLinkerState`.



*Question:* Will this work in the case of ELF shared libraries if two instances of GHC load two different (conflicting) versions of a .so? My impression is that it won't and that the workaround would be to use a linked list of handles like is done with DLLs.



*Question:* There are other platform-specific global variables defined in [rts/Linker.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Linker.c) that I don't know how should be handled:


- This one seems to be a constant that may be overridden during initialization:

  ```wiki
  static void *mmap_32bit_base = (void *)MMAP_32BIT_BASE_DEFAULT
  ```

>
>
> I guess it can continue being a global variable.
>
>

- No idea about these ones:

  ```wiki
  static Elf_Addr got[GOT_SIZE];
  static unsigned int gotIndex;
  static Elf_Addr gp_val = (Elf_Addr)got;
  ```
- No idea about these ones either:

  ```wiki
  static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
  static unsigned int functionTableIndex;
  ```