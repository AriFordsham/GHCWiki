---

## Stack Layout


The stack-layout phase decides where to spill variables. The important goals are to avoid memory traffic and to minimize the size of the stack frame. Both of these goals are accomplished by reusing stack slots.

### Representing Stack Slots


For each stack slot, we introduce a new name, then treat the name as the addressing expression for the slot. At the end of the pipeline, we choose a stack layout, then replace each stack slot with its offset from the stack pointer. The benefit is that we break the phase-ordering problem: any phase of the compiler can name a stack slot.


For example, if we want to spill a variable `x`, we use a regular store instruction to a stack slot at address `SS(x)`:

```wiki
m[SS(x)] := x;
```


where `m[e]` refers to an address `e` in memory.


But what about parameter passing? We use a similar technique, but this time we describe the slot for each location as an offset within the area where the parameters are passed. For example, we lower a function call

```wiki
x, y = f(a, b, c);
```


into approximately the following C--:

```wiki
  sp := SS(k + 4);
  m[SS(k + 1)] := k_info_table;
  m[SS(k + 2)] := a;
  m[SS(k + 3)] := b;
  m[SS(k + 4)] := c;
  call f returns to k;
k:  // on entry to k, sp == stack<k+3>
  x := m[SS(k + 2)]
  y := m[SS(k + 3)]
```


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


An `Area` represents space on the stack; it may use either the `RegSlot` constructor to represent a single stack slot for a register or the `CallArea` constructor to represent parameters passed to/from a function call/return. In a `CallArea`, the `BlockId` is the label of the function call's continuation, and the two integers are the sizes of the outgoing and incoming parameter-passing areas.


To name a specific location on the stack, we represent its address with a new kind of `CmmExpr`: the `CmmStackSlot.` A `CmmStackSlot` is just an integer offset into an `Area`. 
Notice that a `CmmStackSlot` is an *address*, so we can say

```wiki
  Sp = SS(a+0)
```


to make `Sp` point to an particular area.   Use a `CmmLoad` to load from the stack.


Each stack area grows down, with offset 0 pointing to the old end of the area. If we wanted to place a 4-byte object at the old end of the area, we would address it using the offset 4.


The following image shows the layout of a `CallArea` for both the outgoing parameters (function call) and incoming results (continuation after returning from the function call). Note that the incoming and outgoing parameters may be different, and they may overlap.

[](/trac/ghc/attachment/wiki/Commentary/Compiler/StackAreas/CallArea.png)


Note: If the `Area` is a `RegSlot`, we might still use a non-zero offset: for example, we might want to load the low word from a long integer.

**More detail needed about which location in a `CallArea` is numbered 0**


Note: We don't have a virtual frame pointer in this story, but do we really want it? Here's a minor argument against: it requires special treatment by some analyses in Quick C-- (on the other hand, QC-- doesn't have distinguished global registers, so it might not even be an issue in GHC, which has many distinguished global registers).

### Laying out the stack


The business of the stack-layout pass is to construct a mapping (fixed across a single procedure)

```wiki
   Area |-> VirtualOffset
```


which assigns a virtual stack slot (i.e offset relative to the virtual frame pointer) to each `Area`.   **Mutter about vfp**.


A naive approach to laying out the stack would be to give each variable its own stack slot for spilling, and allocate only the ends of the stack frame for parameter-passing areas. But this approach misses two opportunities for optimization:

- Stack slots can be reused by variables that are never on the stack at the same time
- If a function returns a variable on the stack, we might be able to use the return location as the variable's stack slot.


As it turns out, it is quite common in GHC that the first definition of a variable comes when its value is returned from a function call. If the value is returned on the stack, then an important optimization is to avoid copying that value to some other location on the stack. How is that achieved? By making sure the location where the value is returned is also its spill slot.

### The greedy algorithm


One way to assign stack slots is to traverse the flow graph in a reverse post-order depth-first search, allocating a stack location to each stack slot the first time we encounter the slot. (Note: When we encounter a parameter-passing area for the first time, we allocate stack locations for the whole area.) The allocation is then reused for every reference to the stack slot.


The algorithm keeps two pieces of information:

- A many-to-one map from each allocated stack slot to its assigned location on the stack: this map is used to make sure that we use only a single stack location for each stack slot.
- The (one-to-many) inverse of the first map. This map gives the contents of each stack location.


We want to reuse stack slots whenever possible. Therefore, if a stack slot is assigned to a location `l`, we need a variation of liveness analysis to identify final references to `l`. Under this analysis, we say that a stack slot is "live out" if the stack slot may be referenced as either a use or definition in a subsequent instruction. If a location `l` is not live out, we can reallocate `l` to other stack slots.


The algorithm makes a forward pass over the code, making the following decisions for stack slots in each instruction:

- If the stack slot has already been allocated, leave it alone.
- If the (unallocated) stack slot is part of a `CallArea`, then allocate the entire `CallArea` after the youngest stack slot that is live out. We might be able to do better than this, of course, but probably not without significantly more effort.
- If the (unallocated) stack slot is a variable spill, then allocate it to any stack location that is not live out (or, more accurately, that does not contain a stack slot that is live out). If possible, this stack slot should not come from the youngest end of the stack.
- If the instruction is a reload from a stack slot `s` in a `CallArea` to a variable `x` whose stack slot is unallocated, and `s` is not live out, then allocate `stack<x>` to `s`. We avoid allocating `stack<x>` to `s` if `s` is live out because we don't have full interference information to ensure that they can share a stack slot.


We could rewrite the graph in a subsequent pass or in the same pass. The latter seems preferable.


Let's walk through an example. The following is a simple program in pseudo-C--:

```wiki
   if <> then
     x, y = f(a);
     ... <uses y>
   else
     x, z = g(a);
   spill<x> // some source code resulting in x getting spilled
```


The program may be lowered to the following C-- code:

```wiki
   if <> then goto L0; else goto L2;
L0:
   sp := stack<L1 + 2>;
   m[stack<L1 + 1>] := L1_info_table;
   m[stack<L1 + 2>] := a;
   call f returns to L1;
L1:  // on entry to L1, sp == stack<L1+3>
   x := m[stack<L1 + 2>];
   y := m[stack<L1 + 3>];
   ... <uses y>
   goto L4; // L1 stack area dead out
L2: // stack<y> is live out
   sp := stack<L3 + 2>;
   m[stack<L3 + 1>] := L3_info_table;
   m[stack<L3 + 2>] := a;
   call f returns to L3;
L3:  // on entry to L3, sp == stack<L3+3>
   x := m[stack<L3 + 2>];
   y := m[stack<L3 + 3>];
   goto L4; // L3 stack area dead out
L4: // y dead out
   m[stack<x>] := x;
   m[stack<z>] := x;
L5:
```


Let's assume we visit the blocks in lexical order, which is what a reverse post-order depth-first search would give us. Here's a map we might expect to produce at input to each label:

- L0: {} (empty input map -- should have incoming and outgoing parameters for the function)
- L1: {L1 + 0\> -\> 0, \<L1 + 1\> -\> 1, \<L1 + 2\> -\> 2, \<L1 + 3\> -\> 3}
- L2: {\<L1 + 0\> -\> 0, \<L1 + 1\> -\> 1, \<L1 + 2\> -\> 2, \<L1 + 3\> -\> 3, stack\<x\> -\> 2, stack\<y\> -\> 3}
- L3: {\<L1 + 0\> -\> 0, \<L1 + 1\> -\> 1, \<L1 + 2\> -\> 2, \<L1 + 3\> -\> 3, stack\<x\> -\> 2, stack\<y\> -\> 3,

> >
> > \<L3 + 1\> -\> 4, \<L3 + 2\> -\> 5, \<L3 + 3\> -\> 6}

- L4: {\<L1 + 0\> -\> 0, \<L1 + 1\> -\> 1, \<L1 + 2\> -\> 2, \<L1 + 3\> -\> 3, stack\<x\> -\> 2, stack\<y\> -\> 3,

> >
> > \<L3 + 1\> -\> 4, \<L3 + 2\> -\> 5, \<L3 + 3\> -\> 6}

- L5: {\<L1 + 0\> -\> 0, \<L1 + 1\> -\> 1, \<L1 + 2\> -\> 2, \<L1 + 3\> -\> 3, stack\<x\> -\> 2, stack\<y\> -\> 3,

> >
> > \<L3 + 1\> -\> 4, \<L3 + 2\> -\> 5, \<L3 + 3\> -\> 6, stack\<z\> -\> 3}


MISSING BIT:
WE NEED TO INITIALIZE THE MAP TO DEAL WITH THE INCOMING AND OUTGOING CALL AREA OF THE FUNCTION, WHOSE LOCATIONS ARE FIXED BY CONVENTION AT THE OLD END OF THE STACK FRAME.

### Notes


Note: Call instructions should indicate not only the registers they use and kill but also the stack slots.


Note: This greedy algorithm makes the copy propagation only if the stack location is not "live"-out. Otherwise, we would have two values sharing the stack slot, with no guarantee that they could safely share the location. If we had an interference graph, we could make a more informed decision, but that's a non-greedy optimization.


Problem: I don't think this approach deals nicely with the case where the arguments returned along both branches are bound to the same variables. In particular, when allocating the first returned value, it may be the same on both branches, but you don't know if the second one will be, so you can't commit to using the same stack space.


What about a procedure's incoming and outgoing parameters, which should appear at the young end of the stack, in a predetermined location? The locations of these stack slots are determined by convention. Should we initialize the map with their locations?


Invariant: A load from a stack slot must be preceded by a spill to that stack slot. And we should visit that spill before the reload. Otherwise, something has gone horribly wrong.


Note: Obviously, we need to keep track of the actual widths of values to compute stack addresses.

## Random Thoughts


Assignments into parameter-passing areas can't be hoisted past adjustments to the stack pointer until we know the stack layout -- otherwise we might try to write off the young end of the stack. There must be some sort of invariant here, something along the lines of: "a reference to a parameter passing area must (a) succeed the adjustment moving the stack pointer to the bottom of the area and (b) precede any other stack adjustment.
Restated: a def or use of a stack location is well-defined only if the stack pointer points "past" the location. Therefore, we can move a stack assignment/reference past an assignment to the stack pointer only if we can prove that this property is maintained.


This all feels related to the coalescing optimization performed by register allocators.


We're missing another optimization opportunity: there's no reason why different live ranges need to share the same stack slot.

## ToDo

- State the invariants.
- Say something about aliasing.
