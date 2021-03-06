---

## Stack Layout


The stack-layout phase decides where to spill variables. The important goals are to avoid memory traffic and to minimize the size of the stack frame. Both of these goals are accomplished by reusing stack slots.

### Representing Stack Slots


For each stack slot, we introduce a new name, then treat the name as the addressing expression for the slot. At the end of the pipeline, we choose a stack layout, then replace each stack slot with its offset from the stack pointer. The benefit is that we break the phase-ordering problem: any phase of the compiler can name a stack slot.


For example, for a variable `x`, the expression `SS(x)` is the address of the stack slot where we can spill `x`. (I don't think we output any C-- that uses SS anymore, but the new code generator marks its stack slots prior to layout with `young<k> + 4`, etc. -- Edward) The stack is assumed to grow down, and we assume that the address `SS(x)` points to the old end of the slot. Therefore, to address the low address of a 4-byte slot, we would use the expression `SS(x + 4)`. And we would spill `x` using the following instruction:

```wiki
m[SS(x + 4)] := x;
```


where `m[e]` refers to an address `e` in memory.


But what about parameter passing? We use a similar technique, but this time we describe the slot for each location as an offset within the area where the parameters are passed. For example, we lower a function call

```wiki
x, y = f(a, b, c);
```


into approximately the following C--:

```wiki
  sp := SS(k + 16);
  m[SS(k + 4)]  := k_info_table;
  m[SS(k + 8)]  := a;
  m[SS(k + 12)] := b;
  m[SS(k + 16)] := c;
  call f returns to k;
k:  // on entry to k, sp == stack<k+12>
  x := m[SS(k + 8)]
  y := m[SS(k + 12)]
```


We use the following types to represent stack slots and parameter-passing areas:

```haskell
data Area
  = RegSlot  LocalReg
  | CallArea AreaId
  deriving (Eq, Ord)

data AreaId
  = Old
  | Young BlockId
  deriving (Eq, Ord)

data CmmExpr
  = CmmLit CmmLit
  ...
  | CmmStackSlot Area Int
  deriving Eq
```


An `Area` represents space on the stack; it may use either the `RegSlot` constructor to represent a single stack slot for a register or the `CallArea` constructor to represent parameters passed to/from a function call/return. In a young `CallArea`, the `BlockId` is the label of the function call's continuation, and it passes parameters to the call. 

**Area layout and addressing**

- Each `Area` grows down, towards lower machine addresses. 
- *Offsets* are always-positive byte displacements within an `Area`.
- The low-offset end is also called the "old end" of the area, the high-offset end is also called the "young end".
- Notice that the low-offset (old) end has higher machine addresses.
- Offset 0 (if we allowed it) would address the byte one *beyond* the high-address end of the `Area`. 
- Larger offsets (from the beginning of the `Area`) correspond to lower machine addresses.
- Hence, to address a 4-byte object at the old end of `Area` a, we use the offset +4, thus `(CmmStackSlot a 4)`.


The `Old` call area is the initial state of the stack on entry to the function (the overflow parameters and the return address) as well as any arguments that will be passed to a tail call.  (SLPJ believes that:) On entry to the function, register `Sp` contains the address of the youngest (lowest-address, highest offset) byte in the `Old` area. 


Note that `RegSlot` areas are very small (since they only need to store a single register), while `CallArea` are contiguous chunks of arguments.


To name a specific location on the stack, we represent its address with a new kind of `CmmExpr`: the `CmmStackSlot`.
A `CmmStackSlot` is just an integer offset into an `Area`. 


Notice that a `CmmStackSlot` is an *address*, so we can say

```wiki
  Sp = SS(a+4)
```


to make `Sp` point to a particular stack slot.   Use a `CmmLoad` to load from the stack slot.


The following figure shows the layout of a `CallArea` for both the outgoing parameters (function call) and incoming results (continuation after returning from the function call). Note that the incoming and outgoing parameters may be different, and they may overlap.

![](stack-areas/call-area.png)


A `RegSlot` is laid out in the same fashion, with the offset 0 pointing off the high byte of the stack slot. To address an 8-byte double-word, we would use the offset 8. To address only the high word of the same stack slot, we would use the offset 4.


Currently, the intermediate code does not explicitly use a virtual frame pointer, but when we talk about offsets into the stack, we implicitly assume that there is a virtual frame pointer that points just off the oldest byte of the return address on entry to the procedures. Therefore, on entry to the procedure, the offset of the (4-byte) return address is 4.

### Laying out the stack


The business of the stack-layout pass is to construct a mapping (fixed across a single procedure)

```wiki
   Area |-> VirtualOffset
```


which assigns a virtual stack slot (i.e. offset in bytes, relative to the virtual frame pointer) to each `Area`.


A naive approach to laying out the stack would be to give each variable its own stack slot for spilling, and allocate only the ends of the stack frame for parameter-passing areas. But this approach misses two opportunities for optimization:

- Stack slots can be reused by variables that are never on the stack at the same time
- If a function returns a variable on the stack, we might be able to use the return location as the variable's stack slot.


As it turns out, it is quite common in GHC that the first definition of a variable comes when its value is returned from a function call. If the value is returned on the stack, then an important optimization is to avoid copying that value to some other location on the stack. How is that achieved? By making sure the location where the value is returned is also its spill slot.

### A greedy algorithm


We rewrite the stack slots in two passes:

1. Walk over the graph and choose an offset for each `Area`.
1. Walk over the graph, keeping track of the stack pointer, and rewrite each address of a stack slot with an offset from the stack pointer. Also, insert adjustments to the stack pointer before and after proc points.


The details are in cmm/CmmProcPointZ.hs (they have not yet been committed, but will be soon - Aug 4, 2008).
