# RTS Configurations


The RTS can be built in several different ways, corresponding to global CPP defines.  The flavour of the RTS is chosen by GHC when compiling a Haskell program, in response to certain command-line options: `-prof`, `-threaded`, etc.



The CPP symbols and their corresponding command-line flags are:


<table><tr><th><tt>PROFILING</tt></th>
<td>
Enables profiling.<br>
GHC option: <tt>-prof</tt><br>
RTS suffix: <tt>p</tt>
</td></tr></table>


<table><tr><th><tt>THREADED_RTS</tt></th>
<td>
Enables multithreading in the RTS, bound threads, and SMP execution.<br>
GHC option: <tt>-threaded</tt><br>
RTS suffix: <tt>thr</tt>
</td></tr></table>


<table><tr><th><tt>DEBUG</tt></th>
<td>
Enables extra debugging code, assertions, traces, and the <tt>+RTS -D</tt> options.<br>
GHC option: <tt>-debug</tt><br>
RTS suffix: <tt>debug</tt>
</td></tr></table>


<table><tr><th><tt>TRACING</tt></th>
<td>
Enables RTS tracing and event logging, see <a href="/trac/ghc/browser/ghc/rts/Trace.c">rts/Trace.c</a>.  Implied by <tt>DEBUG</tt>.<br>
GHC option: <tt>-eventlog</tt><br>
RTS suffix: <tt>l</tt>
</td></tr></table>



So for example, `libHSrts_thr_debug.a` is the version of the runtime compiled with `THREADED_RTS` and `DEBUG`, and will be linked in if you use the `-threaded` and `-debug` options to GHC.


The ways that the RTS is built in are controlled by the `GhcRTSWays` Makefile variable.  

## Combinations



All combinations are allowed.  Only some are built by default though; see [mk/config.mk.in](/trac/ghc/browser/mk/config.mk.in)[](/trac/ghc/export/HEAD/ghc/mk/config.mk.in) to see how the `GhcRTSWays` variable is set.


## Other configuration options


<table><tr><th><tt>NO_REGS</tt></th>
<td>
Disabled the use of hardware registers for the stack pointer (<tt>Sp</tt>), heap pointer (<tt>Hp</tt>), etc.  This is
enabled when building &quot;unregisterised&quot; code, which is controlled by the <tt>GhcUnregisterised</tt> build option.
Typically this is necessary when building GHC on a platform for which there is no native code generator
and LLVM does not have a GHC calling convention.
</td></tr></table>


<table><tr><th><tt>USE_MINIINTERPRETER</tt></th>
<td>
Enables the use of the RTS &quot;mini-interpreter&quot;, which simulates tail-calls.  Again, this is enabled by
<tt>GhcUnregisterised</tt> in the build system.
</td></tr></table>


<table><tr><th><tt>TABLES_NEXT_TO_CODE</tt></th>
<td>
Controls whether the info table is placed directly before the entry code for a closure or return continuation.
This is normally turned on if the platform supports it, but is turned off by <tt>GhcUnregisterised</tt>.
</td></tr></table>


