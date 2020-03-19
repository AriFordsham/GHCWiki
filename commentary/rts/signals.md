# The GHC Commentary: Signals


This section describes how the RTS and Haskell programs interacts with the OS signal facilities, and how the RTS implements recurring actions that need to happen regularly (the "timer signal").


These two topics are documented together because historically, GHC implemented the timer signal using OS signals.  Today, this is no longer the case for most platforms, and newer techniques are the defaults, but fallback implementations are still available.


Throughout we use the term "signal" to refer to both POSIX-style signals and Windows *ConsoleEvents*, and "timer signal" for how the RTS implements the recurring actions (this is usually NOT an OS signal, see below).


How signal handling and the timer signal work differs between the *threaded* version of the runtime (linking with `ghc -threaded`) and the non-threaded version (see [Commentary/Rts/Config](commentary/rts/config)).  Here we discuss only the threaded version, since we expect that to become the standard version in due course.


## The RTS timer signal

The timer signal is used for several tasks:

- To cause the [scheduler](commentary/rts/scheduler) to context switch
- Sampling for [time profiling](commentary/profiling)
- To detect deadlock (see [Commentary/Rts/Scheduler](commentary/rts/scheduler))

The timer signal schedules itself by using the `Ticker` interface, which provides a platform-dependent implementation of "an arbitrary callback that happens at a specified interval".

On most platforms/configurations the Ticker is implemented with a thread that sleeps at that interval, and wakes up to perform these tasks. Only in legacy situations an OS signal is used to implement the Ticker.

Source files:

- Timer signal:
  - The timer signal API for starting/stopping the timer:
    - [`rts/Timer.h`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Timer.h)
  - The timer signal implementation, also including the `handle_tick()` callback function that runs the above tasks
    - [`rts/Timer.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Timer.c)
- Tickers:
  - The Ticker API, defining what functions a platform-specific Ticker implementation must provide:
    - [`rts/Ticker.h`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Ticker.h)
  - The `.cabal` file of the `rts` package that decides whether to include Windows or POSIX implementation `c-sources`:
    - [`rts/rts.cabal.in`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/rts.cabal.in)
  - Windows Ticker implementation:
    - [`rts/win32/Ticker.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/win32/Ticker.c)
    - Uses [`CreateTimerQueue()`](https://docs.microsoft.com/en-us/windows/win32/sync/using-timer-queues)
  - POSIX Ticker implementation dispatcher, which decides which one of many POSIX ticker implementations is used based on the concrete POSIX OS used, and compile-time configuration options:
    - [`rts/posix/Itimer.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/Itimer.c) (historically called `Itimer` for "interval timer", might better be named `Ticker.c` like on Windows)
    - The implementations (best to worst):
      - [`rts/posix/itimer/Pthread.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/itimer/Pthread.c)
        - Starts an OS thread that `sleep()`s in a loop.
        - `timerfd` is used instead of `sleep()` if available (Linux >= 2.6.25 has it).
          This is more accurate for regular intervals, because it works on absolute absolute time, while the sleep based implementation sleeps releative to the time the sleep call starts, thus not accounting for the time that the callback function takes to run, or for time when the thread is not sleeping but de-scheduled because the system is under high load.
      - [`rts/posix/itimer/TimerCreate.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/itimer/TimerCreate.c)
        - Uses POSIX real-time timers via `timer_create()` (a better/newer alternative to `setitimer()` below).
        - Upon timer expiry, it sends a `SIGVTALRM` OS signal to the process (this was changed from `SIGALRM` in #850).
      - [`rts/posix/itimer/Setitimer.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/itimer/Setitimer.c)
        - Uses `setitimer()`, which has lots of problems. But it is available on any POSIX platform.
        - Upon timer expiry, it sends a `SIGALRM` OS signal to the process.


## OS signals

### The interrupt signal


The OS interrupt signal is `SIGINT` on POSIX systems or `CTRL_C_EVENT/CTRL_BREAK_EVENT` on Windows, and is normally sent to the process when the user hits Control-C.   By default, interrupts are handled by the runtime.  Haskell programs can choose to handle them themselves instead (see following subsection).  For example, [GHCi](commentary/compiler/backends/GHCi) hooks the interrupt signal so that it can abort the current interpreted computation and return to the prompt, rather than terminating the whole GHCi process.


When the interrupt signal is received, the default behaviour of the runtime is to attempt to shut down the Haskell program gracefully.  It does this by calling `interruptStgRts()` in [`rts/Schedule.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Schedule.c) (see [Commentary/Rts/Scheduler](commentary/rts/scheduler#shutting-down)).  If a second interrupt signal is received, then we terminate the process immediately; this is just in case the normal shutdown procedure failed or hung for some reason, the user is always able to stop the process with two control-C keystrokes.

### OS signal handling in Haskell code


Source files:

- POSIX: [`rts/posix/Signals.h`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/Signals.h), [`rts/posix/Signals.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/posix/Signals.c)
- Windows: [`rts/win32/ConsoleHandler.h`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/win32/ConsoleHandler.h), [`rts/win32/ConsoleHandler.c`](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/win32/ConsoleHandler.c)


A Haskell program can ask to install signal handlers, via the `System.Posix.Signals` API, or `GHC.ConsoleHandler` on Windows.  When a signal arrives that has a Haskell handler, it is the job of the runtime to create a new Haskell thread to run the signal handler and place the new thread on the run queue of a suitable [Capability](commentary/rts/scheduler#capabilities).


When the runtime is idle, the OS threads will all be waiting inside `yieldCapability()`, waiting for some work to arrive.  We want a signal to be able to create a new Haskell thread and wake up one of these OS threads to run it, but unfortunately the range of operations that can be performed inside a POSIX signal handler is extremely limited, and doesn't include any inter-thread synchronisation (because the signal handler might be running on the same stack as the OS thread it is communicating with).


The solution we use, on both Windows and POSIX systems, is to pass all signals that arrive to the [IO Manager](commentary/rts/io-manager) thread.  On POSIX this works by sending the signal number down a pipe, on Windows it works by storing the signal number in a buffer and signaling the IO Manager's `Event` object to wake it up.  The IO Manager thread then wakes up and creates a new thread for the signal handler, before going back to sleep again.

## RTS Alarm Signals and Foreign Libraries

As described above, some POSIX implementations of the Ticker interface sends OS signals (`timer_create()` with `SIGVTALRM`, or `setitimer()` with `SIGALRM`).

If one of these implementations is used, then the following must be considered:

Many system calls get interrupted by OS signals. They usually indicate failure and set `errno = EINTR` in that case.

When using foreign libraries through the Haskell FFI, it is important
to ensure that the foreign code is capable of dealing with system call
interrupts due to alarm signals GHC is generating if an OS signal based
Ticker implementation is used.


For example, in this `strace` output
a `select` call is interrupted, but the foreign C code interprets the
interrupt as an application error and closes a critical file
descriptor:

```wiki
[pid 22338] send(7, "\1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0
    \0\0\0\0\0\0\0\0\0\0\0"..., 116, MSG_NOSIGNAL) = 116
[pid 22338] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 22338] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 22338] sigreturn()                 = ? (mask now [])
[pid 22338] gettimeofday({1267656511, 467069}, NULL) = 0
[pid 22338] stat64("/etc/localtime", {st_mode=S_IFREG|0644, st_size=3519, ...}) = 0
[pid 22338] write(6, "Communication failed in RPC"..., 176) = 176
[pid 22338] close(7)                    = 0
```

Here we see that `select()` is interrupted by `SIGVTALRM`, thus returning failure, and the surrounding code does not retry. (Note that `errno = ERESTARTNOHAND` is shown, which _should_ be an artifact of using `strace`; the application should always see `errno = EINTR` instead, see [here](https://groups.google.com/d/msg/fa.linux.kernel/OMPYfdmLhXs/iPbTjd6DjEsJ).)

Once the C code was modified to deal with the interrupt properly (retrying on `EINTR`), it
proceeded correctly (note that `select()` is restarted 3 times before it succeeds).

```wiki
[pid 23967] send(7, "\f\0\0\0\244\1\0\0\0\0\0\0B\4\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"..., 536, MSG_NOSIGNAL <unfinished ...>
[pid 23968] <... select resumed> )      = ? ERESTARTNOHAND (To be restarted)
[pid 23968] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23968] sigreturn()                 = ? (mask now [])
[pid 23968] futex(0x9b52a88, FUTEX_WAIT_PRIVATE, 7, NULL <unfinished ...>
[pid 23967] <... send resumed> )        = 536
[pid 23967] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 23967] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23967] sigreturn()                 = ? (mask now [])
[pid 23967] select(8, [7], NULL, NULL, NULL) = ? ERESTARTNOHAND (To be restarted)
[pid 23967] --- SIGVTALRM (Virtual timer expired) @ 0 (0) ---
[pid 23967] sigreturn()                 = ? (mask now [])
[pid 23967] select(8, [7], NULL, NULL, NULL) = 1 (in [7])
[pid 23967] recv(7, "\7\2\0\0\0\0\0\0\0\0\0\0\0\0\0\0\200\0\0\0\244]\0\0\0\0\0\0\0\0\0\0"..., 116, 0) = 116
```

This concern does not arise when a not-OS-signal based "timer signal" implementation is used. That means that on most platforms and configurations, this is not an issue and C code can run unmodified. However, library authors have no control over platforms and configurations. Somebody might want to run code on a new or exotic platform, or not use the `-threaded` runtime. Finally, signals can arive for other, non-Haskell reasons, such as when the user pauses a program using Ctrl+Z / `SIGSTOP`, or when signal handling is part of the application logic. Thus, (C) libraries should try to deal with OS signal interrupts properly in all cases.


### Signals and `foreign import interruptible`

`interruptible` foreign calls send a signal to the blocked thread when the associated Haskell thread receives an async exception, see section "Interruptible foreign calls" in the [FFI docs](https://gitlab.haskell.org/ghc/ghc/blob/master/docs/users_guide/exts/ffi.rst).

As described there, this puts even stricter conditions on the foreign (C) code than the previous section does.