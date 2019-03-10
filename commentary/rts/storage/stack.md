# Layout of the stack


Every [TSO object](commentary/rts/heap-objects#thread-state-objects) contains a stack.  The stack of a TSO grows downwards, with the topmost (most recently pushed) word pointed to by `tso->sp`, and the bottom of the stack given by `tso->stack + tso->stack_size`.


The stack consists of a sequence of *stack frames* (also sometimes called *activation records*) where each frame has the same layout as a heap object:

<table><tr><th> Header </th>
<th> Payload... 
</th></tr></table>


There are several kinds of [stack frame](commentary/rts/stack#kinds-of-stack-frame), but the most common types are those pushed when evaluating a `case` expression:

```wiki
  case e0 of p1 -> e1; ...; pn -> en 
```


The code for evaluating a `case` pushes a new stack frame representing the alternatives of the case, and continues by evaluating `e0`.  When `e0` completes, it returns to the stack frame pushed earlier, which inspects the value and selects the appropriate branch of the case.  The stack frame for a `case` includes the values of all the free variables in the case alternatives.

## Info tables for stack frames


The info table for a stack frame has a couple of extra fields in addition to the [basic info table layout](commentary/rts/heap-objects#info-tables).  A stack-frame info table is defined by `StgRetInfoTable` in [includes/InfoTables.h](/trac/ghc/browser/ghc/includes/InfoTables.h).

[](/trac/ghc/attachment/wiki/Commentary/Rts/Storage/Stack/ret-itbl.png)


The *SRT* field points to the SRT table for this stack frame (see [Commentary/Rts/CAFs](commentary/rts/ca-fs) for details of SRTs).  The return vector gives a vector of return addresses in the case of the `RET_VEC_SMALL` and `RET_VEC_BIG` types of return addresses; see [vectored returns](commentary/rts/haskell-execution#vectored-returns) for more details.

## Layout of the payload


Unlike heap objects which mainly have "pointers first" layout, in a stack frame the pointers and non-pointers are intermingled.  This is so that we can support "stack stubbing" whereby a live variable stored on the stack can be later marked as dead simply by pushing a new stack frame that identifies that slot as containing a non-pointer, so the GC will not follow it.


Stack frames therefore have [bitmap layout](commentary/rts/heap-objects#bitmap-layout).

## Kinds of Stack Frame

- `RET_BCO`
- `RET_SMALL`
- `RET_VEC_SMALL`
- `RET_BIG`
- `RET_VEC_BIG`
- `RET_DYN`
- `RET_FUN`
- `UPDATE_FRAME`
- `CATCH_FRAME`
- `STOP_FRAME`
- `ATOMICALLY_FRAME`
- `CATCH_RETRY_FRAME`
- `CATCH_STM_FRAME`