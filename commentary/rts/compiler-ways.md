# Compiler and runtime system ways in GHC


GHC can compile programs in different *ways*.
For instance, a program might be compiled with profiling enabled (`-prof`), or for multithreaded execution (`-threaded`), or maybe making some debugging tools available (`-debug`, see [Debugging/RuntimeSystem](debugging/runtime-system) for a description).


There are two types of GHC ways, RTS-only ways and full ways.

- **Runtime system (RTS) ways** affect the way that the runtime system is built. As an example, `-threaded` is a runtime system way. When you compile a program with `-threaded`, it will be linked to a (precompiled) version of the RTS with multithreading enabled.


Obviously, the compiler's RTS must have been built for this way (the threaded RTS is activated by default BTW). In customised builds, an RTS way can be added in the build configuration `mk/build.mk` (see [mk/build.mk.sample](https://gitlab.haskell.org/ghc/ghc/blob/master/mk/build.mk.sample)), by adding its *short name* to the variable `GhcRTSWays`.

- **Full ways**


Full compiler ways are ways which affect both the generated code and the runtime system that runs it. 


The profiling way `-prof` is such a way. The machine code of a program compiled for profiling differs from a normal version's code by all code that gathers the profiling information, and the runtime system has additional functionality to access and report this information. Therefore, all libraries used in a profiling-enabled program need to also have profiling enabled, i.e. a separate library version for profiling needs to be installed to compile the program with `prof`. (If the library was installed without this profiling version, the program cannot be linked). 


In customised builds, a full way is added in the build configuration `mk/build.mk` by adding its tag to the variable `GhcLibWays`.

## Available ways in a standard GHC


Ways are identified internally by a way name, and enabled by specific compilation flags. In addition, there are short names (tags) for the available ways, mainly used by the build system.


Here  is a table of available ways in a standard GHC, as of May 2015.

<table><tr><th>Way flag  </th>
<th> Way name </th>
<th> Tag </th>
<th> Type </th>
<th> Description 
</th></tr>
<tr><th> -         </th>
<th> -         </th>
<th> <tt>v</tt>   </th>
<th> Full </th>
<th> (vanilla way) default 
</th></tr>
<tr><th><tt>-threaded</tt> </th>
<th> WayThreaded </th>
<th> <tt>thr</tt> </th>
<th> RTS  </th>
<th> multithreaded runtime system 
</th></tr>
<tr><th><tt>-debug</tt>    </th>
<th> WayDebug    </th>
<th> <tt>debug</tt> </th>
<th> RTS  </th>
<th> debugging, enables trace messages and extra checks 
</th></tr>
<tr><th><tt>-prof</tt>     </th>
<th> WayProf     </th>
<th> <tt>p</tt>    </th>
<th> Full </th>
<th> profiling, enables cost centre stacks and profiling reports 
</th></tr>
<tr><th><tt>-eventlog</tt> </th>
<th> WayEventLog </th>
<th> <tt>l</tt>    </th>
<th> RTS  </th>
<th> Event logging (for ghc-events, threadscope, and EdenTV) 
</th></tr>
<tr><th><tt>-dyn</tt>      </th>
<th> WayDyn      </th>
<th> <tt>dyn</tt>  </th>
<th> Full </th>
<th> Dynamic linking 
</th></tr></table>


The standard (*vanilla*) way of GHC has a name (*vanilla*), but it could (probably?) even be switched off in a custom build if desired.
Obviously, the libraries would still need to be built in the vanilla way for all RTS-only ways, so one would need `GhcLibWays=v` when building any
other RTS-only way.


The code (see below) contains another way, for Glasgow parallel Haskell, which is currently unmaintained (`WayPar`).

### Ways for parallel execution on clusters and multicores


The parallel Haskell runtime system for Eden (available from [http://github.com/jberthold/ghc](http://github.com/jberthold/ghc)) defines several RTS-only ways for Eden.
All these ways execute the RTS in multiple instances with distributed heaps, they differ in the communication substrate (and consequently in the platform).

<table><tr><th>Way flag  </th>
<th> Way name </th>
<th> Tag </th>
<th> Type </th>
<th> communication (OS) 
</th></tr>
<tr><th><tt>-parpvm</tt> </th>
<th> WayParPvm   </th>
<th><tt>pp</tt></th>
<th> RTS </th>
<th> PVM (Linux) 
</th></tr>
<tr><th><tt>-parmpi</tt> </th>
<th> WayParMPI   </th>
<th><tt>pm</tt></th>
<th> RTS </th>
<th> MPI (Linux) 
</th></tr>
<tr><th><tt>-parcp</tt>  </th>
<th> WayParCp    </th>
<th><tt>pc</tt></th>
<th> RTS </th>
<th> OS-native shared memory (Windows/Linux) 
</th></tr>
<tr><th><tt>-parms</tt>  </th>
<th> WayParMSlot </th>
<th><tt>ms</tt></th>
<th> RTS </th>
<th> Windows mail slots (Windows) 
</th></tr></table>

## Combining ways


The alert reader might have noticed that combinations like "threaded with dynamic linking" or "profiled with eventlog" are not covered in the table.
Some ways can be used together (most prominently, debugging can be used together with any other way), others are mutually excluding each other (like profiling with eventlog).


The allowed combinations are defined inside the compiler, in [compiler/GHC/Driver/Session.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Session.hs).
Which brings us to discussing some of the internals.


When compiling ghc, the available combinations are listed in `mk/config.mk.in`; as of 7.10.3:

```wiki
thr         : threaded
thr_p       : threaded profiled
debug       : debugging (compile with -g for the C compiler, and -DDEBUG)
debug_p     : debugging profiled
thr_debug   : debugging threaded
thr_debug_p : debugging threaded profiled
l           : event logging
thr_l       : threaded and event logging
```


For example, to build both the profiling and the debug version of the profiling RTS (`libHSrts_debug_p`/`lHSrts_debug_p`), you can add 

```wiki
GhcRTSWays        += p debug_p
```


to your `build.mk`.

# Internals


Ways are defined in [compiler/GHC/Driver/Session.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Session.hs) as a Haskell data structure `Way`.


Function `dynamic_flags` defines the actual flag strings for the ghc invocation (like `-prof`, `-threaded`), which activate the respective `Way`.


The short name tags for ways are defined in `wayTag`. The tags are used in the suffixes of \*.o and \*.a files for RTS and libraries, for instance `*.p_o` for profiling, `*.l_o` for eventlog.


A number of other functions in there customise behaviour depending on the ways. 
Note `wayOptc` which sets some options for the C compiler, like `-DTRACING` for the `-eventlog` way.


However, this is not the full truth. For instance, there is no `-DDEBUG` for the debug way here, but the RTS is full of `#ifdef DEBUG`.


In [mk/ways.mk](https://gitlab.haskell.org/ghc/ghc/blob/master/mk/ways.mk), we find all the short names and all combinations enumerated, and some more options are defined here (`WAY_*_HC_OPTS`). These definitions are for the driver script, and pass on the right (long-name) options to the Haskell compiler to activate what is inside DynFlags (like -prof for WAY_p_HC_OPTS).
Here we find
```WAY_debug_HC_OPTS= -static -optc-DDEBUG -ticky -DTICKY_TICKY```
so we can learn that ticky profiling is activated by compiling with `debug`.


(TODO be more precise on where the options from ways.mk are used.)
