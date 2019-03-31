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

<table><tr><th>[Arena.c](http://darcs.haskell.org/ghc/rts/Arena.c), [Arena.h](http://darcs.haskell.org/ghc/rts/Arena.h)</th>
<td>
An arena allocator
</td></tr></table>

<table><tr><th>[Hash.c](http://darcs.haskell.org/ghc/rts/Hash.c), [Hash.h](http://darcs.haskell.org/ghc/rts/Hash.h)</th>
<td>
A generic hash table implementation.
</td></tr></table>

### The [Scheduler](commentary/rts/scheduler)

<table><tr><th>[Capability.c](http://darcs.haskell.org/ghc/rts/Capability.c), [Capability.h](http://darcs.haskell.org/ghc/rts/Capability.h)</th>
<td>
Capabilities: virtual CPUs for executing Haskell code.
</td></tr></table>

<table><tr><th>[RaiseAsync.c](http://darcs.haskell.org/ghc/rts/RaiseAsync.c), [RaiseAsync.h](http://darcs.haskell.org/ghc/rts/RaiseAsync.h)</th>
<td>
Asynchronous exceptions.
</td></tr></table>

<table><tr><th>[Schedule.c](http://darcs.haskell.org/ghc/rts/Schedule.c), [Schedule.h](http://darcs.haskell.org/ghc/rts/Schedule.h)</th>
<td>
The scheduler itself.
</td></tr></table>

<table><tr><th>[Sparks.c](http://darcs.haskell.org/ghc/rts/Sparks.c), [Sparks.h](http://darcs.haskell.org/ghc/rts/Sparks.h)</th>
<td>
Sparks: the implementation of `par`.
</td></tr></table>

<table><tr><th>[ThreadLabels.c](http://darcs.haskell.org/ghc/rts/ThreadLabels.c), [ThreadLabels.h](http://darcs.haskell.org/ghc/rts/ThreadLabels.h)</th>
<td>
Labelling threads.
</td></tr></table>

<table><tr><th>[Threads.c](http://darcs.haskell.org/ghc/rts/Threads.c), [Threads.h](http://darcs.haskell.org/ghc/rts/Threads.h)</th>
<td>
Various thread-related functionality.
</td></tr></table>

<table><tr><th>[ThreadPaused.c](http://darcs.haskell.org/ghc/rts/ThreadPaused.c)</th>
<td>
Suspending a thread before it returns to the RTS.
</td></tr></table>

<table><tr><th>[Task.c](http://darcs.haskell.org/ghc/rts/Task.c), [Task.h](http://darcs.haskell.org/ghc/rts/Task.h)</th>
<td>
Task: an OS-thread abstraction.
</td></tr></table>

<table><tr><th>[AwaitEvent.h](http://darcs.haskell.org/ghc/rts/AwaitEvent.h)</th>
<td>
Waiting for events (non-threaded RTS only).
</td></tr></table>

<table><tr><th>[Timer.c](http://darcs.haskell.org/ghc/rts/Timer.c), [Timer.h](http://darcs.haskell.org/ghc/rts/Timer.h),  [Ticker.h](http://darcs.haskell.org/ghc/rts/Ticker.h)</th>
<td>
The runtime's interval timer, used for context switching and profiling.
</td></tr></table>

### C files: the [FFI](commentary/rts/ffi)

<table><tr><th>[Adjustor.c](http://darcs.haskell.org/ghc/rts/Adjustor.c)</th>
<td>
Very hairy support for `foreign import "wrapper"`.
</td></tr></table>

<table><tr><th>[HsFFI.c](http://darcs.haskell.org/ghc/rts/HsFFI.c), [RtsAPI.c](http://darcs.haskell.org/ghc/rts/RtsAPI.c)</th>
<td>
Implementation of the Haskell FFI C interface: `hs_init()`,
`hs_exit()`, etc.
  
</td></tr></table>

### The [Byte-code Interpreter](commentary/rts/interpreter)

<table><tr><th>[Disassembler.c](http://darcs.haskell.org/ghc/rts/Disassembler.c), [Disassembler.h](http://darcs.haskell.org/ghc/rts/Disassembler.h)</th>
<td></td></tr>
<tr><th>[Interpreter.c](http://darcs.haskell.org/ghc/rts/Interpreter.c), [Interpreter.h](http://darcs.haskell.org/ghc/rts/Interpreter.h)</th>
<td>
The [byte-code interpreter](commentary/rts/interpreter) and disassembler.
</td></tr></table>

<table><tr><th>[Linker.c](http://darcs.haskell.org/ghc/rts/Linker.c)</th>
<td>[LinkerInternals.h](http://darcs.haskell.org/ghc/rts/LinkerInternals.h)
The dynamic object-code linker?.
</td></tr></table>

### [Profiling](commentary/profiling)

<table><tr><th>[LdvProfile.c](http://darcs.haskell.org/ghc/rts/LdvProfile.c), [LdvProfile.h](http://darcs.haskell.org/ghc/rts/LdvProfile.h)</th>
<td>
Lag-drag-void profiling (also known as Biographical Profiling).
</td></tr></table>

<table><tr><th>[ProfHeap.c](http://darcs.haskell.org/ghc/rts/ProfHeap.c), [ProfHeap.h](http://darcs.haskell.org/ghc/rts/ProfHeap.h)</th>
<td>
Generic heap-profilng support.
</td></tr></table>

<table><tr><th>[Profiling.c](http://darcs.haskell.org/ghc/rts/Profiling.c), [Profiling.h](http://darcs.haskell.org/ghc/rts/Profiling.h)</th>
<td>
Generic profilng support.
</td></tr></table>

<table><tr><th>[Proftimer.c](http://darcs.haskell.org/ghc/rts/Proftimer.c), [Proftimer.h](http://darcs.haskell.org/ghc/rts/Proftimer.h)</th>
<td>
The profiling timer.
</td></tr></table>

<table><tr><th>[RetainerProfile.c](http://darcs.haskell.org/ghc/rts/RetainerProfile.c), [RetainerProfile.h](http://darcs.haskell.org/ghc/rts/RetainerProfile.h)</th>
<td></td></tr>
<tr><th>[RetainerSet.c](http://darcs.haskell.org/ghc/rts/RetainerSet.c), [RetainerSet.h](http://darcs.haskell.org/ghc/rts/RetainerSet.h)</th>
<td>
Retainer profiling.
</td></tr></table>

<table><tr><th>[Ticky.c](http://darcs.haskell.org/ghc/rts/Ticky.c), [Ticky.h](http://darcs.haskell.org/ghc/rts/Ticky.h)</th>
<td>
Ticky-ticky profiling (currently defunct; needs reviving).
</td></tr></table>

### RTS Debugging

<table><tr><th>[Printer.c](http://darcs.haskell.org/ghc/rts/Printer.c), [Printer.h](http://darcs.haskell.org/ghc/rts/Printer.h)</th>
<td>
Generic printing for heap objects and stacks (not used much).
</td></tr></table>

<table><tr><th>[Trace.c](http://darcs.haskell.org/ghc/rts/Trace.c), [Trace.h](http://darcs.haskell.org/ghc/rts/Trace.h)</th>
<td>
Generic support for various kinds of trace and debugging messages.  
</td></tr></table>

### The Front Panel


The front panel is currently defunct.  It offers a graphical view of
the running Haskell program in real time, and was pretty cool when it
worked.

<table><tr><th>[FrontPanel.c](http://darcs.haskell.org/ghc/rts/FrontPanel.c), [FrontPanel.h](http://darcs.haskell.org/ghc/rts/FrontPanel.h)</th>
<td></td></tr>
<tr><th>[VisCallbacks.c](http://darcs.haskell.org/ghc/rts/VisCallbacks.c), [VisCallbacks.h](http://darcs.haskell.org/ghc/rts/VisCallbacks.h)</th>
<td></td></tr>
<tr><th>[VisSupport.c](http://darcs.haskell.org/ghc/rts/VisSupport.c), [VisSupport.h](http://darcs.haskell.org/ghc/rts/VisSupport.h)</th>
<td></td></tr>
<tr><th>[VisWindow.c](http://darcs.haskell.org/ghc/rts/VisWindow.c), [VisWindow.h](http://darcs.haskell.org/ghc/rts/VisWindow.h)</th>
<td></td></tr></table>

### Other

<table><tr><th>[Main.c](http://darcs.haskell.org/ghc/rts/Main.c)</th>
<td>
The C `main()` function for a standalone Haskell program;
basically this is just a client of `HsFFI.h`.
</td></tr></table>

<table><tr><th>[RtsFlags.c](http://darcs.haskell.org/ghc/rts/RtsFlags.c)</th>
<td>
Understands the `+RTS ... -RTS` flags.
</td></tr></table>

<table><tr><th>[RtsMessages.c](http://darcs.haskell.org/ghc/rts/RtsMessages.c)</th>
<td>
Support for emitting messages from the runtime.
</td></tr></table>

<table><tr><th>[RtsSignals.c](http://darcs.haskell.org/ghc/rts/RtsSignals.c), [RtsSignals.h](http://darcs.haskell.org/ghc/rts/RtsSignals.h)</th>
<td>
Signal-related stuff.
</td></tr></table>


Miscellaneous stuff:

<table><tr><th>[RtsUtils.c](http://darcs.haskell.org/ghc/rts/RtsUtils.c), [RtsUtils.h](http://darcs.haskell.org/ghc/rts/RtsUtils.h)</th>
<td></td></tr>
<tr><th>[GetTime.h](http://darcs.haskell.org/ghc/rts/GetTime.h)</th>
<td></td></tr>
<tr><th>[PosixSource.h](http://darcs.haskell.org/ghc/rts/PosixSource.h)</th>
<td></td></tr>
<tr><th>[Prelude.h](http://darcs.haskell.org/ghc/rts/Prelude.h)</th>
<td></td></tr>
<tr><th>[Typeable.c](http://darcs.haskell.org/ghc/rts/Typeable.c)</th>
<td></td></tr>
<tr><th>[RtsDllMain.c](http://darcs.haskell.org/ghc/rts/RtsDllMain.c)</th>
<td></td></tr></table>

### OLD stuff

<table><tr><th>`parallel/`</th>
<td>
Code for GUM: parallel GHC.  This is heavily bitrotted and currently doesn't work (as of GHC 6.6; it last worked around
5.02 I believe).
</td></tr></table>

<table><tr><th>`dotnet/`</th>
<td>
Bitrotted code for GHC.NET.
</td></tr></table>