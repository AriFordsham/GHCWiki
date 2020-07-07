# High-level Cmm

This is the discussion page about a "high-level" variant of Cmm.

## Motivation

[Asterius][asterius] is a GHC-based Haskell-to-WebAssembly compiler. Currently,
Asterius emits WebAssembly from Cmm, and at the Cmm level, there's a lot of
implicit convention about how the runtime works, e.g. closure representation and
heap allocation. The Asterius custom runtime needs to work with these
conventions, and implement its own garbage collector.

It's possible to use the host platform to represent closures and do garbage
collection. WebAssembly's [reference types proposal][wasm-anyref] allow
JavaScript objects to be imported into WebAssembly as opaque references, and by
representing closures as JavaScript objects, a language runtime can get garbage
collection for free. [schism][schism] is such an example. WebAssembly is also
working on an MVP of [GC proposal][wasm-gc] which adds native support of
garbage-collected structs and arrays.

In order to take advantage of the host platform's garbage collector, we need to
work with a "high-level" variant of Cmm. It should satisfy the following
properties:

- The garbage collected pointer type should be completely opaque
- Closure allocation is done by a single primitive, no explicit heap/stack check
- Can be quickly lowered to vanilla Cmm for NCG, and native runtime performance
  of generated code doesn't regress.

Such a high-level Cmm will be useful to any GHC backend which targets a managed
runtime.

## Required features in high-level Cmm

- `CmmGcPtr` as a `CmmType`. `CmmGcPtr` is not tied to the platform's word
  size, and there is no bitcast between `CmmGcPtr` and regular Cmm types.
- `CmmGcLoad`/`CmmGcStore`, which load/store a single `CmmGcPtr` field in a
  closure. The closure field address consists of two `CmmExpr`s: the closure
  address and the offset.
- `CmmGcAlloc` as a `CmmNode`, which return the `CmmGcPtr` of the allocated
  closure.
- Dedicated set of `GlobalReg`s for passing `CmmGcPtr`s in Cmm function calls.

## Lowering to vanilla Cmm

- `CmmGcPtr` is converted to `gcWord`.
- `CmmGcLoad`/`CmmGcStore` is converted to `CmmLoad`/`CmmStore`.
- `CmmGcAlloc` is converted to the regular heap check.

## Interaction with existing runtime features

- STG stack: allocating a stack frame should use the same mechanism of
  allocating a regular closure, possibly with extra annotation that this is for
  the stack. For managed runtimes, the stack can be modeled as an array or
  linked list of stack frame closures, and no need to handle underflow/overflow.
  For NCG, it can be lowered to regular stack check.
- Dynamic pointer tagging: won't work at all in high-level Cmm. We always need
  to read a closure's info table to extract the desired info, e.g. if it's
  evaluated.
- Selector thunk optimization and `IND` elimination: works with high-level Cmm.
- `byteArrayContents#`: expose the payload of a pinned `ByteArray#` as an
  unmanaged pointer. Works with high-level Cmm, since we support non-gcptr
  payload in closures, and we can put a `malloc`ed pointer in a `ByteArray#`.
- `anyToAddr#`: reinterpret-cast from a gcptr to an unmanaged pointer. Won't
  work in high-level Cmm.
- Weak pointers and finalizers: the semantics of weak pointers and finalizers
  will depend on the embedder of high-level Cmm.
- `ghc-heap` compatability: likely won't work.
- Compact region: unknown.

[asterius]: https://github.com/tweag/asterius
[schism]: https://github.com/google/schism
[wasm-anyref]: https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md
[wasm-gc]: https://github.com/WebAssembly/gc/blob/master/proposals/gc/MVP.md
