# GHC Source Tree Roadmap: rts/


This directory contains the source code for the runtime system.


There are three types of files:

- **`.h`**

  Header files that are *private to the RTS*.  That is, header files in this directory are
not shipped with GHC, and APIs they define are therefore intended to be private and not
usable by client code (in practice, we do not and probably cannot enforce this).  Header
files that we *do* ship with GHC are in the [includes](commentary/source-tree/includes)
directory.

- **`.c`**

  C source code for the runtime system.  Conventions used in this code are described in
[Commentary/Rts/Conventions](commentary/rts/conventions).

- **`.cmm`**

  C-- code for parts of the runtime that are part of the Haskell execution environment: for
example, the implementation of primitives, exceptions, and so on.  A `.cmm` file is
pseudo C--: more or less C-- syntax with some omissions and some additional macro-like
extensions implemented by GHC.  The `.cmm` files are compiled using GHC itself: see
[Commentary/Rts/Cmm](commentary/rts/cmm).

### Subdirectories of rts/

- **`posix/`**
- **`win32/`**

  POSIX and Win32-specific parts of the runtime respectively.  We try to put platform-specific stuff in these directories,
however not all of the RTS follows this convention right now.

- **`hooks/`**

  Hooks for changing the RTS behaviour from client code, eg. changing the default heap size.
(see [User's Guide for more about hooks](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-hooks)).

- **`sm/`**

  The [Storage Manager](commentary/rts/storage).

### Haskell Execution


All this code runs on the Haskell side of the Haskell/C divide; 
`StgCRun` is the interface between the two layers.

- **[Apply.cmm](http://darcs.haskell.org/ghc/rts/Apply.cmm), [AutoApply.h](http://darcs.haskell.org/ghc/rts/AutoApply.h), `AutoApply.cmm`, [Apply.h](http://darcs.haskell.org/ghc/rts/Apply.h)**

  The eval/apply machinery.  Note: `AutoApply.cmm` is the family
of functions for performing generic application of unknown
functions, this code depends on the number of registers available
for argument passing, so it is generated automatically by the program
`genapply` in `utils/genapply`.

- **[Exception.cmm](http://darcs.haskell.org/ghc/rts/Exception.cmm)**

  Support for execptions.

- **[HeapStackCheck.cmm](http://darcs.haskell.org/ghc/rts/HeapStackCheck.cmm)**

  Code for preparing the stack when the current Haskell thread needs
to return to the RTS, because we either ran out of heap or stack, or
need to block (eg. `takeMVar`), or yield.

- **[PrimOps.cmm](http://darcs.haskell.org/ghc/rts/PrimOps.cmm)**

  Implementation of out-of-line primitives (see [Commentary/PrimOps](commentary/prim-ops)).

- **[StgMiscClosures.cmm](http://darcs.haskell.org/ghc/rts/StgMiscClosures.cmm)**

  Some built-in closures, such as the family of small `Int`s and
`Chars`, and some built-in info tables such as `BLACKHOLE`
and `ARR_WORDS`.

- **[StgStartup.cmm](http://darcs.haskell.org/ghc/rts/StgStartup.cmm)**

  Code that executes when a Haskell thread begins and ends.

- **[StgStdThunks.cmm](http://darcs.haskell.org/ghc/rts/StgStdThunks.cmm)**

  Some built-in thunks: [selector thunks](commentary/rts/storage/heap-objects#selector-thunks) and "apply" thunks.

- **[Updates.cmm](http://darcs.haskell.org/ghc/rts/Updates.cmm), [Updates.h](http://darcs.haskell.org/ghc/rts/Updates.h)**

  [Updates](commentary).

- **[HCIncludes.h](http://darcs.haskell.org/ghc/rts/HCIncludes.h)**

  Header file included when compiling `.cmm` files via C.

- **[StgCRun.c](http://darcs.haskell.org/ghc/rts/StgCRun.c), [StgRun.h](http://darcs.haskell.org/ghc/rts/StgRun.h)**

  The interface between the C execution layer and the Haskell
execution layer.

- **[StgPrimFloat.c](http://darcs.haskell.org/ghc/rts/StgPrimFloat.c)**

  Floating-point stuff.

- **[STM.c](http://darcs.haskell.org/ghc/rts/STM.c)**

  Implementation of Software Transactional Memory.

### The [Storage Manager](commentary/rts/storage)

- **[sm/Storage.c](http://darcs.haskell.org/ghc/rts/sm/Storage.c)**

  Top-level of the storage manager.

- **[sm/MBlock.c](http://darcs.haskell.org/ghc/rts/sm/MBlock.c), [sm/MBlock.h](http://darcs.haskell.org/ghc/rts/sm/MBlock.h), [sm/OSMem.h](http://darcs.haskell.org/ghc/rts/sm/OSMem.h)**

  The "megablock" allocator; this is the thin layer between the RTS and
the operating system for allocating memory.

- **[sm/BlockAlloc.c](http://darcs.haskell.org/ghc/rts/sm/BlockAlloc.c), [sm/BlockAlloc.h](http://darcs.haskell.org/ghc/rts/sm/BlockAlloc.h)**

  The low-level block allocator, requires only `MBlock`.

- **[sm/GC.c](http://darcs.haskell.org/ghc/rts/sm/GC.c), [sm/Scav.c](http://darcs.haskell.org/ghc/rts/sm/Scav.c), [sm/Evac.c](http://darcs.haskell.org/ghc/rts/sm/Evac.c), [sm/GCUtils.c](http://darcs.haskell.org/ghc/rts/sm/GCUtils.c), [sm/MarkWeak.c](http://darcs.haskell.org/ghc/rts/sm/MarkWeak.c)**

  The generational copying garbage collector.

- **[sm/Compact.c](http://darcs.haskell.org/ghc/rts/sm/Compact.c), [sm/Compact.h](http://darcs.haskell.org/ghc/rts/sm/Compact.h)**

  The compacting garbage collector.

- **[ClosureFlags.c](http://darcs.haskell.org/ghc/rts/ClosureFlags.c)**

  Determining properties of various types of closures.

- **[Sanity.c](http://darcs.haskell.org/ghc/rts/Sanity.c), [Sanity.h](http://darcs.haskell.org/ghc/rts/Sanity.h)**

  A sanity-checker for the heap and related data structures.

- **[Stats.c](http://darcs.haskell.org/ghc/rts/Stats.c), [Stats.h](http://darcs.haskell.org/ghc/rts/Stats.h)**

  Statistics for the garbage collector and storage manager.

- **[Stable.c](http://darcs.haskell.org/ghc/rts/Stable.c)**

  Stable names and stable pointers.

- **[Weak.c](http://darcs.haskell.org/ghc/rts/Weak.c), [Weak.h](http://darcs.haskell.org/ghc/rts/Weak.h)**

  Weak pointers.

### Data Structures


Data structure abstractions for use in the RTS:

- **[Arena.c](http://darcs.haskell.org/ghc/rts/Arena.c), [Arena.h](http://darcs.haskell.org/ghc/rts/Arena.h)**

  An arena allocator

- **[Hash.c](http://darcs.haskell.org/ghc/rts/Hash.c), [Hash.h](http://darcs.haskell.org/ghc/rts/Hash.h)**

  A generic hash table implementation.

### The [Scheduler](commentary/rts/scheduler)

- **[Capability.c](http://darcs.haskell.org/ghc/rts/Capability.c), [Capability.h](http://darcs.haskell.org/ghc/rts/Capability.h)**

  Capabilities: virtual CPUs for executing Haskell code.

- **[RaiseAsync.c](http://darcs.haskell.org/ghc/rts/RaiseAsync.c), [RaiseAsync.h](http://darcs.haskell.org/ghc/rts/RaiseAsync.h)**

  Asynchronous exceptions.

- **[Schedule.c](http://darcs.haskell.org/ghc/rts/Schedule.c), [Schedule.h](http://darcs.haskell.org/ghc/rts/Schedule.h)**

  The scheduler itself.

- **[Sparks.c](http://darcs.haskell.org/ghc/rts/Sparks.c), [Sparks.h](http://darcs.haskell.org/ghc/rts/Sparks.h)**

  Sparks: the implementation of `par`.

- **[ThreadLabels.c](http://darcs.haskell.org/ghc/rts/ThreadLabels.c), [ThreadLabels.h](http://darcs.haskell.org/ghc/rts/ThreadLabels.h)**

  Labelling threads.

- **[Threads.c](http://darcs.haskell.org/ghc/rts/Threads.c), [Threads.h](http://darcs.haskell.org/ghc/rts/Threads.h)**

  Various thread-related functionality.

- **[ThreadPaused.c](http://darcs.haskell.org/ghc/rts/ThreadPaused.c)**

  Suspending a thread before it returns to the RTS.

- **[Task.c](http://darcs.haskell.org/ghc/rts/Task.c), [Task.h](http://darcs.haskell.org/ghc/rts/Task.h)**

  Task: an OS-thread abstraction.

- **[AwaitEvent.h](http://darcs.haskell.org/ghc/rts/AwaitEvent.h)**

  Waiting for events (non-threaded RTS only).

- **[Timer.c](http://darcs.haskell.org/ghc/rts/Timer.c), [Timer.h](http://darcs.haskell.org/ghc/rts/Timer.h),  [Ticker.h](http://darcs.haskell.org/ghc/rts/Ticker.h)**

  The runtime's interval timer, used for context switching and profiling.

### C files: the [FFI](commentary/rts/ffi)

- **[Adjustor.c](http://darcs.haskell.org/ghc/rts/Adjustor.c)**

  Very hairy support for `foreign import "wrapper"`.

- **[HsFFI.c](http://darcs.haskell.org/ghc/rts/HsFFI.c), [RtsAPI.c](http://darcs.haskell.org/ghc/rts/RtsAPI.c)**

  Implementation of the Haskell FFI C interface: `hs_init()`,
`hs_exit()`, etc.
  

### The [Byte-code Interpreter](commentary/rts/interpreter)

- **[Disassembler.c](http://darcs.haskell.org/ghc/rts/Disassembler.c), [Disassembler.h](http://darcs.haskell.org/ghc/rts/Disassembler.h), [Interpreter.c](http://darcs.haskell.org/ghc/rts/Interpreter.c), [Interpreter.h](http://darcs.haskell.org/ghc/rts/Interpreter.h)**

  The [byte-code interpreter](commentary/rts/interpreter) and disassembler.

- **[Linker.c](http://darcs.haskell.org/ghc/rts/Linker.c)**

  [LinkerInternals.h](http://darcs.haskell.org/ghc/rts/LinkerInternals.h)
The dynamic object-code linker?.


### [Profiling](commentary/profiling)

- **[LdvProfile.c](http://darcs.haskell.org/ghc/rts/LdvProfile.c), [LdvProfile.h](http://darcs.haskell.org/ghc/rts/LdvProfile.h)**

  Lag-drag-void profiling (also known as Biographical Profiling).

- **[ProfHeap.c](http://darcs.haskell.org/ghc/rts/ProfHeap.c), [ProfHeap.h](http://darcs.haskell.org/ghc/rts/ProfHeap.h)**

  Generic heap-profilng support.

- **[Profiling.c](http://darcs.haskell.org/ghc/rts/Profiling.c), [Profiling.h](http://darcs.haskell.org/ghc/rts/Profiling.h)**

  Generic profilng support.

- **[Proftimer.c](http://darcs.haskell.org/ghc/rts/Proftimer.c), [Proftimer.h](http://darcs.haskell.org/ghc/rts/Proftimer.h)**

  The profiling timer.

- **[RetainerProfile.c](http://darcs.haskell.org/ghc/rts/RetainerProfile.c), [RetainerProfile.h](http://darcs.haskell.org/ghc/rts/RetainerProfile.h), [RetainerSet.c](http://darcs.haskell.org/ghc/rts/RetainerSet.c), [RetainerSet.h](http://darcs.haskell.org/ghc/rts/RetainerSet.h)**

  Retainer profiling.

- **[Ticky.c](http://darcs.haskell.org/ghc/rts/Ticky.c), [Ticky.h](http://darcs.haskell.org/ghc/rts/Ticky.h)**

  Ticky-ticky profiling (currently defunct; needs reviving).

### RTS Debugging

- **[Printer.c](http://darcs.haskell.org/ghc/rts/Printer.c), [Printer.h](http://darcs.haskell.org/ghc/rts/Printer.h)**

  Generic printing for heap objects and stacks (not used much).

- **[Trace.c](http://darcs.haskell.org/ghc/rts/Trace.c), [Trace.h](http://darcs.haskell.org/ghc/rts/Trace.h)**

  Generic support for various kinds of trace and debugging messages.  

### The Front Panel


The front panel is currently defunct.  It offers a graphical view of
the running Haskell program in real time, and was pretty cool when it
worked.

- **[FrontPanel.c](http://darcs.haskell.org/ghc/rts/FrontPanel.c), [FrontPanel.h](http://darcs.haskell.org/ghc/rts/FrontPanel.h)**
- **[VisCallbacks.c](http://darcs.haskell.org/ghc/rts/VisCallbacks.c), [VisCallbacks.h](http://darcs.haskell.org/ghc/rts/VisCallbacks.h)**
- **[VisSupport.c](http://darcs.haskell.org/ghc/rts/VisSupport.c), [VisSupport.h](http://darcs.haskell.org/ghc/rts/VisSupport.h)**
- **[VisWindow.c](http://darcs.haskell.org/ghc/rts/VisWindow.c), [VisWindow.h](http://darcs.haskell.org/ghc/rts/VisWindow.h)**

### Other

- **[Main.c](http://darcs.haskell.org/ghc/rts/Main.c)**

  The C `main()` function for a standalone Haskell program;
basically this is just a client of `HsFFI.h`.

- **[RtsFlags.c](http://darcs.haskell.org/ghc/rts/RtsFlags.c)**

  Understands the `+RTS ... -RTS` flags.

- **[RtsMessages.c](http://darcs.haskell.org/ghc/rts/RtsMessages.c)**

  Support for emitting messages from the runtime.

- **[RtsSignals.c](http://darcs.haskell.org/ghc/rts/RtsSignals.c), [RtsSignals.h](http://darcs.haskell.org/ghc/rts/RtsSignals.h)**

  Signal-related stuff.


Miscellaneous stuff:

- **[RtsUtils.c](http://darcs.haskell.org/ghc/rts/RtsUtils.c), [RtsUtils.h](http://darcs.haskell.org/ghc/rts/RtsUtils.h)**
- **[GetTime.h](http://darcs.haskell.org/ghc/rts/GetTime.h)**
- **[PosixSource.h](http://darcs.haskell.org/ghc/rts/PosixSource.h)**
- **[Prelude.h](http://darcs.haskell.org/ghc/rts/Prelude.h)**
- **[Typeable.c](http://darcs.haskell.org/ghc/rts/Typeable.c)**
- **[RtsDllMain.c](http://darcs.haskell.org/ghc/rts/RtsDllMain.c)**

### OLD stuff

- **`parallel/`**

  Code for GUM: parallel GHC.  This is heavily bitrotted and currently doesn't work (as of GHC 6.6; it last worked around
5.02 I believe).

- **`dotnet/`**

  Bitrotted code for GHC.NET.