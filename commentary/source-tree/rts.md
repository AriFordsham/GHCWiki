# GHC Source Tree Roadmap: rts/


This directory contains the source code for the runtime system.



There are three types of files:


<table><tr><th><tt>.h</tt></th>
<td>
Header files that are <i>private to the RTS</i>.  That is, header files in this directory are
not shipped with GHC, and APIs they define are therefore intended to be private and not
usable by client code (in practice, we do not and probably cannot enforce this).  Header
files that we <i>do</i> ship with GHC are in the <a href="commentary/source-tree/includes">includes</a>
directory.
</td></tr></table>


<table><tr><th><tt>.c</tt></th>
<td>
C source code for the runtime system.  Conventions used in this code are described in
<a href="commentary/rts/conventions">Commentary/Rts/Conventions</a>.
</td></tr></table>


<table><tr><th><tt>.cmm</tt></th>
<td>
C-- code for parts of the runtime that are part of the Haskell execution environment: for
example, the implementation of primitives, exceptions, and so on.  A <tt>.cmm</tt> file is
pseudo C--: more or less C-- syntax with some omissions and some additional macro-like
extensions implemented by GHC.  The <tt>.cmm</tt> files are compiled using GHC itself: see
<a href="commentary/rts/cmm">Commentary/Rts/Cmm</a>.
</td></tr></table>


### Subdirectories of rts/


<table><tr><th><tt>posix/</tt></th>
<td>
</td></tr>
<tr><th><tt>win32/</tt></th>
<td>
POSIX and Win32-specific parts of the runtime respectively.  We try to put platform-specific stuff in these directories,
however not all of the RTS follows this convention right now.
</td></tr></table>


<table><tr><th><tt>hooks/</tt></th>
<td>
Hooks for changing the RTS behaviour from client code, eg. changing the default heap size.
(see <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-hooks"> User&apos;s Guide for more about hooks</a>).
</td></tr></table>


<table><tr><th><tt>sm/</tt></th>
<td>
The <a href="commentary/rts/storage">Storage Manager</a>.
</td></tr></table>


### Haskell Execution


All this code runs on the Haskell side of the Haskell/C divide; 
`StgCRun` is the interface between the two layers.


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Apply.cmm"> Apply.cmm</a>, <a href="http://darcs.haskell.org/ghc/rts/AutoApply.h"> AutoApply.h</a>, <tt>AutoApply.cmm</tt>, <a href="http://darcs.haskell.org/ghc/rts/Apply.h"> Apply.h</a></th>
<td>
The eval/apply machinery.  Note: <tt>AutoApply.cmm</tt> is the family
of functions for performing generic application of unknown
functions, this code depends on the number of registers available
for argument passing, so it is generated automatically by the program
<tt>genapply</tt> in <tt>utils/genapply</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Exception.cmm"> Exception.cmm</a></th>
<td>
Support for execptions.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/HeapStackCheck.cmm"> HeapStackCheck.cmm</a></th>
<td>
Code for preparing the stack when the current Haskell thread needs
to return to the RTS, because we either ran out of heap or stack, or
need to block (eg. <tt>takeMVar</tt>), or yield.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/PrimOps.cmm"> PrimOps.cmm</a></th>
<td>
Implementation of out-of-line primitives (see <a href="commentary/prim-ops">Commentary/PrimOps</a>).
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/StgMiscClosures.cmm"> StgMiscClosures.cmm</a></th>
<td>
Some built-in closures, such as the family of small <tt>Int</tt>s and
<tt>Chars</tt>, and some built-in info tables such as <tt>BLACKHOLE</tt>
and <tt>ARR_WORDS</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/StgStartup.cmm"> StgStartup.cmm</a></th>
<td>
Code that executes when a Haskell thread begins and ends.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/StgStdThunks.cmm"> StgStdThunks.cmm</a></th>
<td>
Some built-in thunks: <a href="commentary/rts/storage/heap-objects#selector-thunks">selector thunks</a> and &quot;apply&quot; thunks.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Updates.cmm"> Updates.cmm</a>, <a href="http://darcs.haskell.org/ghc/rts/Updates.h"> Updates.h</a></th>
<td>
<a href="commentary">Updates</a>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/HCIncludes.h"> HCIncludes.h</a></th>
<td>
Header file included when compiling <tt>.cmm</tt> files via C.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/StgCRun.c"> StgCRun.c</a>, <a href="http://darcs.haskell.org/ghc/rts/StgRun.h"> StgRun.h</a></th>
<td>
The interface between the C execution layer and the Haskell
execution layer.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/StgPrimFloat.c"> StgPrimFloat.c</a></th>
<td>
Floating-point stuff.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/STM.c"> STM.c</a></th>
<td>
Implementation of Software Transactional Memory.
</td></tr></table>


### The [Storage Manager](commentary/rts/storage)


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/sm/Storage.c"> sm/Storage.c</a></th>
<td>
Top-level of the storage manager.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/sm/MBlock.c"> sm/MBlock.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/MBlock.h"> sm/MBlock.h</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/OSMem.h"> sm/OSMem.h</a></th>
<td>
The &quot;megablock&quot; allocator; this is the thin layer between the RTS and
the operating system for allocating memory.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/sm/BlockAlloc.c"> sm/BlockAlloc.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/BlockAlloc.h"> sm/BlockAlloc.h</a></th>
<td>
The low-level block allocator, requires only <tt>MBlock</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/sm/GC.c"> sm/GC.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/Scav.c"> sm/Scav.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/Evac.c"> sm/Evac.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/GCUtils.c"> sm/GCUtils.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/MarkWeak.c"> sm/MarkWeak.c</a></th>
<td>
The generational copying garbage collector.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/sm/Compact.c"> sm/Compact.c</a>, <a href="http://darcs.haskell.org/ghc/rts/sm/Compact.h"> sm/Compact.h</a></th>
<td>
The compacting garbage collector.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/ClosureFlags.c"> ClosureFlags.c</a></th>
<td>
Determining properties of various types of closures.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Sanity.c"> Sanity.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Sanity.h"> Sanity.h</a></th>
<td>
A sanity-checker for the heap and related data structures.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Stats.c"> Stats.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Stats.h"> Stats.h</a></th>
<td>
Statistics for the garbage collector and storage manager.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Stable.c"> Stable.c</a></th>
<td>
Stable names and stable pointers.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Weak.c"> Weak.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Weak.h"> Weak.h</a></th>
<td>
Weak pointers.
</td></tr></table>

### Data Structures



Data structure abstractions for use in the RTS:


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Arena.c"> Arena.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Arena.h"> Arena.h</a></th>
<td>
An arena allocator
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Hash.c"> Hash.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Hash.h"> Hash.h</a></th>
<td>
A generic hash table implementation.
</td></tr></table>


### The [Scheduler](commentary/rts/scheduler)


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Capability.c"> Capability.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Capability.h"> Capability.h</a></th>
<td>
Capabilities: virtual CPUs for executing Haskell code.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RaiseAsync.c"> RaiseAsync.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RaiseAsync.h"> RaiseAsync.h</a></th>
<td>
Asynchronous exceptions.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Schedule.c"> Schedule.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Schedule.h"> Schedule.h</a></th>
<td>
The scheduler itself.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Sparks.c"> Sparks.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Sparks.h"> Sparks.h</a></th>
<td>
Sparks: the implementation of <tt>par</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/ThreadLabels.c"> ThreadLabels.c</a>, <a href="http://darcs.haskell.org/ghc/rts/ThreadLabels.h"> ThreadLabels.h</a></th>
<td>
Labelling threads.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Threads.c"> Threads.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Threads.h"> Threads.h</a></th>
<td>
Various thread-related functionality.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/ThreadPaused.c"> ThreadPaused.c</a></th>
<td>
Suspending a thread before it returns to the RTS.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Task.c"> Task.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Task.h"> Task.h</a></th>
<td>
Task: an OS-thread abstraction.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/AwaitEvent.h"> AwaitEvent.h</a></th>
<td>
Waiting for events (non-threaded RTS only).
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Timer.c"> Timer.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Timer.h"> Timer.h</a>,  <a href="http://darcs.haskell.org/ghc/rts/Ticker.h"> Ticker.h</a></th>
<td>
The runtime&apos;s interval timer, used for context switching and profiling.
</td></tr></table>


### C files: the [FFI](commentary/rts/ffi)


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Adjustor.c"> Adjustor.c</a></th>
<td>
Very hairy support for <tt>foreign import "wrapper"</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/HsFFI.c"> HsFFI.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RtsAPI.c"> RtsAPI.c</a></th>
<td>
Implementation of the Haskell FFI C interface: <tt>hs_init()</tt>,
<tt>hs_exit()</tt>, etc.
  
</td></tr></table>


### The [Byte-code Interpreter](commentary/rts/interpreter)


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Disassembler.c"> Disassembler.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Disassembler.h"> Disassembler.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/Interpreter.c"> Interpreter.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Interpreter.h"> Interpreter.h</a></th>
<td>
The <a href="commentary/rts/interpreter">byte-code interpreter</a> and disassembler.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Linker.c"> Linker.c</a></th>
<td>
<a href="http://darcs.haskell.org/ghc/rts/LinkerInternals.h"> LinkerInternals.h</a>
The dynamic object-code linker?.
</td></tr></table>


### [Profiling](commentary/profiling)


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/LdvProfile.c"> LdvProfile.c</a>, <a href="http://darcs.haskell.org/ghc/rts/LdvProfile.h"> LdvProfile.h</a></th>
<td>
Lag-drag-void profiling (also known as Biographical Profiling).
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/ProfHeap.c"> ProfHeap.c</a>, <a href="http://darcs.haskell.org/ghc/rts/ProfHeap.h"> ProfHeap.h</a></th>
<td>
Generic heap-profilng support.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Profiling.c"> Profiling.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Profiling.h"> Profiling.h</a></th>
<td>
Generic profilng support.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Proftimer.c"> Proftimer.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Proftimer.h"> Proftimer.h</a></th>
<td>
The profiling timer.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RetainerProfile.c"> RetainerProfile.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RetainerProfile.h"> RetainerProfile.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/RetainerSet.c"> RetainerSet.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RetainerSet.h"> RetainerSet.h</a></th>
<td>
Retainer profiling.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Ticky.c"> Ticky.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Ticky.h"> Ticky.h</a></th>
<td>
Ticky-ticky profiling (currently defunct; needs reviving).
</td></tr></table>


### RTS Debugging


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Printer.c"> Printer.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Printer.h"> Printer.h</a></th>
<td>
Generic printing for heap objects and stacks (not used much).
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Trace.c"> Trace.c</a>, <a href="http://darcs.haskell.org/ghc/rts/Trace.h"> Trace.h</a></th>
<td>
Generic support for various kinds of trace and debugging messages.  
</td></tr></table>

### The Front Panel


The front panel is currently defunct.  It offers a graphical view of
the running Haskell program in real time, and was pretty cool when it
worked.


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/FrontPanel.c"> FrontPanel.c</a>, <a href="http://darcs.haskell.org/ghc/rts/FrontPanel.h"> FrontPanel.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/VisCallbacks.c"> VisCallbacks.c</a>, <a href="http://darcs.haskell.org/ghc/rts/VisCallbacks.h"> VisCallbacks.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/VisSupport.c"> VisSupport.c</a>, <a href="http://darcs.haskell.org/ghc/rts/VisSupport.h"> VisSupport.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/VisWindow.c"> VisWindow.c</a>, <a href="http://darcs.haskell.org/ghc/rts/VisWindow.h"> VisWindow.h</a></th>
<td>
</td></tr></table>


### Other


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/Main.c"> Main.c</a></th>
<td>
The C <tt>main()</tt> function for a standalone Haskell program;
basically this is just a client of <tt>HsFFI.h</tt>.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RtsFlags.c"> RtsFlags.c</a></th>
<td>
Understands the <tt>+RTS ... -RTS</tt> flags.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RtsMessages.c"> RtsMessages.c</a></th>
<td>
Support for emitting messages from the runtime.
</td></tr></table>


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RtsSignals.c"> RtsSignals.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RtsSignals.h"> RtsSignals.h</a></th>
<td>
Signal-related stuff.
</td></tr></table>



Miscellaneous stuff:


<table><tr><th><a href="http://darcs.haskell.org/ghc/rts/RtsUtils.c"> RtsUtils.c</a>, <a href="http://darcs.haskell.org/ghc/rts/RtsUtils.h"> RtsUtils.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/GetTime.h"> GetTime.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/PosixSource.h"> PosixSource.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/Prelude.h"> Prelude.h</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/Typeable.c"> Typeable.c</a></th>
<td>
</td></tr>
<tr><th><a href="http://darcs.haskell.org/ghc/rts/RtsDllMain.c"> RtsDllMain.c</a></th>
<td>
</td></tr></table>


### OLD stuff


<table><tr><th><tt>parallel/</tt></th>
<td>
Code for GUM: parallel GHC.  This is heavily bitrotted and currently doesn&apos;t work (as of GHC 6.6; it last worked around
5.02 I believe).
</td></tr></table>


<table><tr><th><tt>dotnet/</tt></th>
<td>
Bitrotted code for GHC.NET.
</td></tr></table>


