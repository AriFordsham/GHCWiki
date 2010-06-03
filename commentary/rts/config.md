# RTS Configurations


The RTS can be built in several different ways, corresponding to global CPP defines.  The flavour of the RTS is chosen by GHC when compiling a Haskell program, in response to certain command-line options: `-prof`, `-threaded`, etc.


The CPP symbols and their corresponding command-line flags are:

<table><tr><th>`PROFILING`</th>
<td>
Enables profiling.

GHC option: `-prof`

RTS suffix: `p`</td></tr></table>

<table><tr><th>`THREADED_RTS`</th>
<td>
Enables multithreading in the RTS, bound threads, and SMP execution.

GHC option: `-threaded`

RTS suffix: `thr`</td></tr></table>

<table><tr><th>`DEBUG`</th>
<td>
Enables extra debugging code, assertions, traces, and the `+RTS -D` options.

GHC option: `-debug`

RTS suffix: `debug`</td></tr></table>

<table><tr><th>`TRACING`</th>
<td>
Enables RTS tracing and event logging, see [rts/Trace.c](/trac/ghc/browser/ghc/rts/Trace.c).  Implied by `DEBUG`.

GHC option: `-eventlog`

RTS suffix: `l`</td></tr></table>

<table><tr><th>`NO_REGS`, `USE_MINIINTERPRETER`</th>
<td>
Enables "unregisterised" compilation, i.e. via C with no mangler.

GHC option: `-unreg`

RTS suffix: `u`</td></tr></table>


So for example, `libHSrts_thr_debug.a` is the version of the runtime compiled with `THREADED_RTS` and `DEBUG`, and will be linked in if you use the `-threaded` and `-debug` options to GHC.


The ways that the RTS is built in are controlled by the `GhcRTSWays` Makefile variable.  

## Combinations


The following combinations are allowed:

- `DEBUG` with anything
- `PROFILING` only with `NO_REGS`, `USE_MINIINTERPRETER`

## OLD ways


The following ways are bitrotted and currently don't work (GHC 6.6):

<table><tr><th>`PAR`, `GRANSIM`</th>
<td>
Parallel Haskell

GHC option: `-par`

RTS suffix: `mp, mg`</td></tr></table>

<table><tr><th>`TICKY`</th>
<td>
Ticky-ticky profiling used to be a separate "way"; you had to rebuild all the libraries and the RTS for ticky-ticky profiling, 
just like ordinary time/space profiling.  This isn't the case any more: you can link modules compiled with `-ticky`
against modules or packages compiled without it.  Since 6.12.1, the `-debug` RTS version also include ticky-ticky
support, and there is no separate RTS version for ticky.  If you use the `-ticky` flag when linking a program, it implies
`-debug`.
</td></tr></table>