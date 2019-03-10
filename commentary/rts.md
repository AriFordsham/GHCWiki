# GHC Commentary: The Runtime System


GHC's runtime system is a slightly scary beast: 50,000 lines of C and C-- code, much of which seems at first glance to be completely obscure.  What on earth does the RTS *do*?  Here are the highlights:

- It includes all the bits required to execute Haskell code that aren't compiled into the code itself.
  For example, the RTS contains the code that knows how to raise an exception when you call `error`,
  and code to implement the multi-precision `Integer` operations (via the GMP library).

- It includes a sophisticated storage manager, including a multi-generational garbage collector with copying
  and compacting strategies.

- It includes a user-space scheduler for Haskell threads, together with support for scheduling Haskell threads
  across multiple CPUs, and allowing Haskell threads to call foreign functions in separate OS threads.

- There's a byte-code interpreter for GHCi, and a dynamic linker for loading up object code into a GHCi session.

- Heap-profiling (of various kinds) and time-profiling of Haskell code are included.

- Support for Software Transactional Memory is now included...


Next, we try to make sense of how it all fits together.

## Block Diagram

[](/trac/ghc/attachment/wiki/Commentary/Rts/rts-overview.png)

## RTS: Contents

- [RTS Configurations](commentary/rts/config)
- [The Word](commentary/rts/word)
- [What the hell is a .cmm file?](commentary/rts/cmm)
- [The Storage Manager](commentary/rts/storage)
- [Sanity Checking](commentary/rts/sanity)
- [The Haskell Execution model](commentary/rts/haskell-execution)
- [The Scheduler](commentary/rts/scheduler)
- [So how does foreign import "wrapper" work?](commentary/rts/ffi)
- [GHCi support: the byte-code interpreter and dynamic linker](commentary/rts/interpreter)
- [Asynchronous exceptions](commentary/rts/async-exceptions)
- [Software Transactional Memory (STM)](commentary/rts/stm)
- [Weak Pointers and Finalizers](commentary/rts/weak)
- [Coding conventions in the RTS](commentary/rts/conventions)