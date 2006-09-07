
\[ Up: [Commentary/Rts](commentary/rts) \]

# Layout of the stack


Every [TSO object](commentary/rts/heap-objects#thread-state-objects) contains a stack.  The stack of a TSO grows downwards, with the topmost (most recently pushed) word pointed to by `tso->sp`, and the bottom of the stack given by `tso->stack + tso->stack_size`.


The stack consists of a sequence of *stack frames* (also sometimes called *activation records*) where each frame has the same layout as a heap object:

<table><tr><th> Header </th>
<th> Payload... 
</th></tr></table>


There are several kinds of [stack frame](commentary/rts/stack#stack-frames), but the most common types are those pushed when evaluating a `case` expression:

```wiki
  case e0 of p1 -> e1; ...; pn -> en 
```


The code for evaluating a `case` pushes a new stack frame representing the alternatives of the case, and continues by evaluating `e0`.  When `e0` completes, it returns to the stack frame pushed earlier, which inspects the value and selects the appropriate branch of the case.  The stack frame for a `case` includes the values of all the free variables in the case alternatives.

## Info tables for stack frames


The info table for a stack frame has a couple of extra fields in addition to the [basic info table layout](commentary/rts/heap-objects#info-tables):

not handled: Image


The *SRT* field points to the SRT table for this stack frame (see [Commentary/Rts/CAFs](commentary/rts/ca-fs) for details of SRTs).  The return vector gives a vector of return addresses in the case of the `RET_VEC_SMALL` and `RET_VEC_BIG` types of return addresses; see [vectored returns](commentary/rts/haskell-execution#) for more details.

## Layout of the payload


Unlike heap objects which mainly have "pointers first" layout, in a stack frame the pointers and non-pointers are intermingled.  This is so that we can support "stack stubbing" whereby a live variable stored on the stack can be later marked as dead simply by pushing a new stack frame that identifies that slot as containing a non-pointer, so the GC will not follow it.


The stack frame describes the pointerhood of each word in the payload by means of a bitmap.  There are two kinds of bitmap: *small* and *large*:

### Small bitmaps


A small bitmap fits into a single word (the layout word of the info table), and looks like this:

<table><tr><th> Size (bits 0-4) </th>
<th> Bitmap (bits 5-31) 
</th></tr></table>


(for a 64-bit word size, the size is given 6 bits instead of 5).  


The size field gives the size of the payload, and each bit of the bitmap is 1 if the corresponding word of payload contains a pointer to a live object.


The macros `MK_BITMAP`, `BITMAP_SIZE`, and `BITMAP_BITS` in [includes/InfoTables.h](/trac/ghc/browser/ghc/includes/InfoTables.h) provide ways to conveniently operate on small bitmaps.

### Large bitmaps


If the size of the stack frame is larger than the 27 words that a small bitmap can describe, then the fallback mechanism is the large bitmap.  A large bitmap is a separate structure, containing a single word size and a multi-word bitmap: see `StgLargeBitmap` in [includes/InfoTables.h](/trac/ghc/browser/ghc/includes/InfoTables.h).

## Stack Frames

`RET_BCO`,
`RET_SMALL`,
`RET_VEC_SMALL`,
`RET_BIG`,
`RET_VEC_BIG`,
`RET_DYN`,
`RET_FUN`,
`UPDATE_FRAME`,
`CATCH_FRAME`,
`STOP_FRAME`,
`ATOMICALLY_FRAME`,
`CATCH_RETRY_FRAME`,
`CATCH_STM_FRAME`