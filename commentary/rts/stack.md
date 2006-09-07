
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


The info table for a stack frame has a couple of extra fields in addition to the [basic info table layout](commentary/rts/heap-objects#info-tables).

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