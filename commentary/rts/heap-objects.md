
\[ Up: [Commentary/Rts](commentary/rts) \]

# GHC Commentary: The Layout of Heap Objects

## Terminology

- A *lifted* type is one that contains bottom (_\|_), conversely an *unlifted* type does not contain _\|_.
  For example, `Array` is lifted, but `ByteArray#` is unlifted.

- A *boxed* type is represented by a pointer to an object in the heap, an *unboxed* object is represented by a value.
  For example, `Int` is boxed, but `Int#` is unboxed.


The representation of _\|_ must be a pointer: it is an object that when evaluated throws an exception or enters an infinite loop.  Therefore, only boxed types may be lifted.


There are boxed unlifted types: eg. `ByteArray#`.  If you have a value of type `ByteArray#`, it definitely points to a heap object with type `ARR_WORDS` (see below), rather than an unevaluated thunk.


Unboxed tuples `(#...#)` are both unlifted and unboxed.  They are represented by multiple values passed in registers or on the stack, according to the [return convention](commentary/rts/haskell-execution).

## Heap Objects


All heap objects have the same basic layout, embodied by the type `StgClosure` in [ Closures.h](http://darcs.haskell.org/ghc/includes/Closures.h).  The diagram below shows the layout of a heap object:

not handled: Image


A heap object always begins with a *header*, defined by `StgHeader` in [ Closures.h](http://darcs.haskell.org/ghc/includes/Closures.h):

```wiki
typedef struct {
    const struct _StgInfoTable* info;
#ifdef PROFILING
    StgProfHeader         prof;
#endif
#ifdef GRAN
    StgGranHeader         gran;
#endif
} StgHeader;
```


The most important part of the header is the *info pointer*, which points to the info table for the closure.  In the default build, this is all the header contains, so a header is normally just one word.  In other builds, the header may contain extra information: eg. in a profilnig build it also contains information about who built the closure.


Most of the runtime is insensitive to the size of `StgHeader`; that is, we are careful not to hardcode the offset to the payload anywhere, instead we use C struct indexing or `sizeof(StgHeader)`.  This makes it easy to extend `StgHeader` with new fields if we need to.

## Info Tables


The info table contains all the information that the runtime needs to know about the closure.

## Types of Heap Object

## The stack, and stack objects