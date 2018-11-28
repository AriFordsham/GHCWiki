## The Hidden Dangers Of `touch#`


Haskell is a very high-level language, and most of the time, programmers don't have to think about memory allocation concerns at all, and the compiler and runtime do a good job of managing these things correctly.


However, there are situations where we do need to meddle with low-level concerns of memory allocation and pointers after all, for example:

- ...when runtime performance is critical
- ...when writing FFI code wrapping C libraries not written with Haskell in mind
- ...when implementing base library functionality or foundational data structures


In such situations, the compiler's assumption of free reign over low-level memory management concerns, and our desire to manipulate pointers and memory allocation more directly, are at odds: for example, taking a pointer to a managed value may lead to that pointer escaping the managed value's lifespan. For example, consider this hypothetical example:

```
main=do
    p <- allocaBytes 4
    poke p (12345::Word32)
    peek p >>= putStrLn
```


If we were to implement the above, we would have to decide how to manage the lifecycle of the allocated memory. But none of the options is great:

- We can allocate unmanaged memory through `malloc()` and just use that; it will stay alive until the process exits. This is undesirable because we're leaking memory.
- We can allocate unmanaged memory through `malloc()` and leave the responsibility for cleaning up to the caller. The existing `mallocBytes` function does exactly that, but it is often undesirable because it is brittle; we want something that automatically cleans up after itself.
- We can allocate managed memory and thus leave cleanup to the GC. The problem with this is that we're not retaining any references that the GC can follow, so our allocated memory becomes eligible for GC right after we return from `allocaBytes`.


A somewhat obvious solution is to change `allocaBytes` to have a different type: instead of `allocaBytes :: Int -> IO (Ptr a)`, we make it `allocaBytes :: Int -> (Ptr a -> IO b) -> IO b`. This is the tried-and-tested bracket pattern, allowing us to run deallocation code after the payload action has finished. A simple implementation might look like this:

```
allocaBytes n action =do
    p <- mallocBytes n
    retval <- action p
    free p
    return retval
```


And this would actually work. However, `malloc` isn't very efficient compared to the Haskell heap, so ideally, we would want to have the cake and eat it by allocating from the Haskell heap, while still keeping the memory alive until the action has finished. This means that we need a magical token that tells the GC that the memory we allocated is still alive, and which we can place in a strategic position right after the action. 


That magical construct is the `touch#` primop, which essentially says "I will need this value to be alive until at least this point". So `allocaBytes` can be implemented as the moral equivalent of:

```
allocaBytes n action =do
    a <- newByteArray n
    p <- addressOf a
    retval <- action p
    touch# a
    return retval
```


Great! However, as evidenced by [\#14346](https://gitlab.haskell.org//ghc/ghc/issues/14346), there is a problem. The correctness of the above code hinges on the presence of `touch#` (without it, the GC is free to pick up `a` before `action p` finishes), but GHC's optimizer doesn't actually know this. Specifically, if `action` can be proven to never return, then the dead code elimination optimization will happily delete the `touch#` and `return` parts, and we're back at square 1.


The first workaround for this was to add a `NOINLINE` pragma on `allocaBytes` - without inlining `allocaBytes`, the dead code elimination stops at the function boundary, and cannot proceed to removing `touch#`, because:

- From the call site, we can infer that `action` doesn't terminate, but we cannot step into the implementation of `allocaBytes` to continue our analysis.
- From inside `allocaBytes`, we cannot tell whether `action` terminates, so we cannot eliminate the `touch#`.


This prevents the problem, but it's not a great solution. By preventing inlining, we may also miss out on other, valid, optimizations, so it's a sledgehammer solution; and it's also one that requires manual diligence - we have to get the inlining pragmas just right for every usage of `touch#`. Clearly this isn't ideal.


So we take a different route. Instead of a primop that says "please consider this value alive at this point in time", we provide one that says "please consider this value alive as long as the primop itself is alive". That primop is `with#`, and it is the moral equivalent of `with :: a -> IO b -> IO b`, meaning: "keep the first argument alive for as long as the second argument runs". And we can rewrite `allocaBytes` to the moral equivalent of:

```
allocaBytes n action =do
    p <- allocateMemory n
    with# p action
```


Now the inferred lifecycle of both `with#` and `p` accurately reflect our intentions. In a way, we have baked the bracket pattern into the `with#` primop.

---


Note that the actual implementations of `allocaBytes`, `with#`, and `touch#`, look slightly different, due to the fact that `IO`-like primops are implemented in terms of the "secret" underlying representation of `IO`, i.e. `State# RealWorld -> (# State# RealWorld, a #)` rather than `IO a`.
