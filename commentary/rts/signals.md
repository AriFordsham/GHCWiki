# The GHC Commentary: Signals


This section describes how the RTS interacts with the OS signal facilities.  Throughout we use the term "signal" to refer to both POSIX-style signals and Windows *ConsoleEvents*.


Signal handling differs between the *threaded* version of the runtime and the non-threaded version (see [Commentary/Rts/Config](commentary/rts/config)).  Here we discuss only the threaded version, since we expect that to become the standard version in due course.


Source files:

- POSIX signal handling:

  - [rts/posix/Signals.h](/trac/ghc/browser/ghc/rts/posix/Signals.h), [rts/posix/Siglans.c](/trac/ghc/browser/ghc/rts/posix/Siglans.c)
- Windows console events:

  - [rts/win32/ConsoleHandler.h](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.h), [rts/win32/ConsoleHandler.c](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.c)

## Signal handling in the RTS


The RTS is interested in two signals: a timer signal, and an interrupt signal.

### The timer signal


 
The timer signal is used for several things:

- To cause the [scheduler](commentary/rts/scheduler) to context switch
- Sampling for [time profiling](commentary/profiling)
- To detect deadlock (see [Commentary/Rts/Scheduler](commentary/rts/scheduler))


Source files:

- The timer interrupt handler, and starting/stopping the timer:

  - [rts/Timer.h](/trac/ghc/browser/ghc/rts/Timer.h), [rts/Timer.c](/trac/ghc/browser/ghc/rts/Timer.c)
- Platform-independent ticker interface, used by the timer:

  - [rts/Ticker.h](/trac/ghc/browser/ghc/rts/Ticker.h)
- Posix implementation of ticker:

  - [rts/posix/Itimer.h](/trac/ghc/browser/ghc/rts/posix/Itimer.h), [rts/posix/Itimer.h](/trac/ghc/browser/ghc/rts/posix/Itimer.h)
- Windows implementation of ticker:

  - [rts/win32/Ticker.c](/trac/ghc/browser/ghc/rts/win32/Ticker.c)


On Posix, the timer signal is implemented by calling `setitimer()` to generate regular `SIGALRM` signals (the single threaded RTS uses SIGVTALRM).  This isn't ideal, since we'd like to allow the application t so use `SIGALRM` if it needs to.  Ideally we should use something better (see [\#850](https://gitlab.haskell.org//ghc/ghc/issues/850)).


On Windows, we spawn a new thread that repeatedly sleeps for the timer interval and then executes the timer interrupt handler.

## The interrupt signal


The interrupt signal is `SIGINT` on POSIX systems or `CTRL_C_EVENT/CTRL_BREAK_EVENT`on Windows, and is normally sent to the process when the user hits Control-C.   By default, interrupts are handled by the runtime.  They can be caught and handled by Haskell code instead, using `System.Posix.Signals` on POSIX systems or `GHC.ConsoleHandler` on Windows systems.  For example, [GHCi](commentary/gh-ci) hooks the interrupt signal so that it can abort the current interpreted computation and return to the prompt, rather than terminating the whole GHCi process.


When the interrupt signal is received, the default behaviour of the runtime is to attempt to shut down the Haskell program gracefully.  It does this by calling `interruptStgRts()` in [rts/Schedule.c](/trac/ghc/browser/ghc/rts/Schedule.c) (see [Commentary/Rts/Scheduler](commentary/rts/scheduler#shutting-down)).  If a second interrupt signal is received, then we terminate the process immediately; this is just in case the normal shutdown procedure failed or hung for some reason, the user is always able to stop the process with two control-C keystrokes.

## Signal handling in Haskell code


Source files:

- POSIX: [rts/posix/Signals.h](/trac/ghc/browser/ghc/rts/posix/Signals.h), [rts/posix/Signals.c](/trac/ghc/browser/ghc/rts/posix/Signals.c)
- Windows: [rts/win32/ConsoleHandler.h](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.h), [rts/win32/ConsoleHandler.c](/trac/ghc/browser/ghc/rts/win32/ConsoleHandler.c)


A Haskell program can ask to install signal handlers, via the `System.Posix.Signals` API, or `GHC.ConsoleHandler` on Windows.  When a signal arrives that has a Haskell handler, it is the job of the runtime to create a new Haskell thread to run the signal handler and place the new thread on the run queue of a suitable [Capability](commentary/rts/scheduler#capabilities).


When the runtime is idle, the OS threads will all be waiting inside `yieldCapability()`, waiting for some work to arrive.  We want a signal to be able to create a new Haskell thread and wake up one of these OS threads to run it, but unfortunately the range of operations that can be performed inside a POSIX signal handler is extremely limited, and doesn't include any inter-thread synchronisation (because the signal handler might be running on the same stack as the OS thread it is communicating with).


The solution we use, on both Windows and POSIX systems, is to pass all signals that arrive to the [IO Manager](commentary/rts/io-manager) thread.  On POSIX this works by sending the signal number down a pipe, on Windows it works by storing the signal number in a buffer and signaling the IO Manager's `Event` object to wake it up.  The IO Manager thread then wakes up and creates a new thread for the signal handler, before going back to sleep again.
