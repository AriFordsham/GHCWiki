---

## Stack Layout


The stack-layout phase decides where to spill variables. The important goals are to avoid memory traffic and to minimize the size of the stack frame. Both of these goals are accomplished by reusing stack slots.

### The old approach


In the old code generator, we have a phase ordering problem: no compiler phase can name a stack slot until stack layout has been fixed. But stack layout can only be fixed at the end of the pipeline. The consequence of this approach is that most of the pipeline requires special treatment for code that must refer to stack slots (e.g. parameter passing in calling conventions, or spills and reloads). In particular, we defined special instructions for CopyIn and CopyOut of function arguments, which implicitly stand for an adjustment to the stack pointer and some parallel assignments to the function parameters or return results.


For example, we compile a function call

```wiki
x, y = f(a, b, c)
```


into the following C--:

```wiki
  CopyOut(a, b, c);
  call f returns to k;
k:
  CopyIn (x, y)
```


Every stage of the back end must cope with the CopyIn and CopyOut pseudoinstructions.

### The new approach


A better approach is to introduce a unique name for each stack slot, then treat the name as the addressing expression for the slot. At the end of the pipeline, we choose a stack layout, then replace each stack slot with its offset from the stack pointer. The benefit is that we break the phase-ordering problem: any phase of the compiler can name a stack slot.


For example, if we want to spill a variable *x*, we use a regular store instruction to a stack slot at address *stack\<x\>*:

```wiki
m[stack<x>] := x;
```


where *m\[e\]* refers to an address *e* in memory.


But what about parameter passing? We use a similar technique, but this time we describe the slot for each location as an offset within the area where the parameters are passed. For example, we compile a function call

```wiki
x, y = f(a, b, c);
```


into approximately the following C--:

```wiki
  sp := stack<k + 4>;
  m[stack<k + 1>] := k_info_table;
  m[stack<k + 2>] := a;
  m[stack<k + 3>] := b;
  m[stack<k + 4>] := c;
k:  // on entry to k, sp == stack<k+3>
  x := m[stack<k + 2>]
  y := m[stack<k + 3>]
```


Note that the semantics of the now-unnecessary CopyIn and CopyOut are reified by an assignment to the stack pointer and a series of copy instructions. If an optimization understands copy instructions, it can improve this code -- without having to worry about the semantics of CopyIn.


Furthermore, the job of laying out the stack is now pleasantly simple: decide where to place the slots and parameter-passing areas, then rewrite the references to stack locations. The stack-layout phase is no longer responsible for inserting stack adjustments or lowering CopyIn and CopyOut nodes to data-movement instructions.


We use the following types to represent stack slots and parameter-passing areas:

```wiki
data Area
  = RegSlot  LocalReg
  | CallArea BlockId Int Int
  deriving (Eq, Ord)

data CmmExpr
  = CmmLit CmmLit
  ...
  | CmmStackSlot Area Int
  deriving Eq
```


An `Area` represents space on the stack; it may contain either a single stack slot for a register or a number of parameters passed to/from a function call/return. The `BlockId` is the label of the function call's continuation, and the two integers are the sizes of the outgoing and incoming parameter-passing areas.


To name a specific location on the stack, we represent its address with a new kind of `CmmExpr`: the `CmmStackSlot`. A `CmmStackSlot` is just an integer offset into an `Area`.


Note: We don't have a virtual frame pointer in this story, but do we really need it? Here's a minor argument against: it requires special treatment by some analyses in Quick C--, although it might just fold away into the large set of global registers in GHC.

## ToDo

- Explain the stack layout algorithm.
- State the invariants.
- Say something about aliasing.

## Old Text


The current CMM code-generation path leaves stack management completely implicit until the end of the pipeline. The consequence is that a number of things must happen all at once:

- The stack is laid out.
- CopyIn and CopyOut nodes are converted to the appropriate moves, loads and stores, as required by the calling conventions.
- The stack pointer is adjusted to conventional locations before and after function calls.
- The return address is pushed on the stack at call sites.


And of course, none of the argument-passing or stack-adjusting instructions are available during optimization, before the stack layout is fixed.


A much better approach is to give symbolic names to locations on the stack, lower all the argument passing and stack adjusting to the actual data-movement instructions, and replace the names later when the final stack layout is fixed.


For example

```wiki
f(x) {
   y = x;
   ...

   call g(x) returning to L;

 L:
   ...
}
```


should be compiled to

```wiki
f:
   x = m[stack(f, 1)]; // copy the argument from stack area `f' to local x
   y = x;

   ...

   sp = stack(L, 1); // Make room on the stack for the arguments
   m[stack(L, 1)] = L; // copy the return address
   m[stack(L, 0)] = x; // copy the argument
   call g();

 L:
   ... 
}
```


The exact syntax will probably be a bit different, but there's the gist of it.


But how do we map the stack slots to actual hardware locations? As it turns out, it is quite common in GHC that the first definition of a variable comes when its value is returned from a function call. If the value is returned on the stack, then an important optimization is to avoid copying that value to some other ``local`` location on the stack. How is that achieved? By making sure the location where the value is returned is defined as its local location.


For example,

```wiki
f() {
  x = g();
  ...
}
```


If g() returns x on the stack, we would like the return location to be x's stack slot for the rest of the procedure.
Issues raised by this:

- Stack holes where return addresses were stored. Such a hole can be filled with a variable that can't be stored in a convenient return slot.
- Stack allocation must be based on control flow -- how else would you know if x has already been placed or if it can be stored on the bottom of the stack?
