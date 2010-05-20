# The Scheduler


The scheduler is the heart of the runtime: it is the single part of
the system through which all entry to the Haskell world goes, and it
handles requests from outside to invoke Haskell functions (foreign
export).


In this part of the commentary we'll discuss the *threaded* version
of the runtime (see [Commentary/Rts/Config](commentary/rts/config)), that is, the
version of the runtime that uses multiple OS threads, because it is by
far the most complex beast.


We begin by discussing the basic abstractions used in the scheduler.

## OS Threads


Source files: [includes/rts/OSThreads.h](/trac/ghc/browser/ghc/includes/rts/OSThreads.h),
[rts/win32/OSThreads.c](/trac/ghc/browser/ghc/rts/win32/OSThreads.c), [rts/posix/OSThreads.c](/trac/ghc/browser/ghc/rts/posix/OSThreads.c)


We assume that the OS provides some kind of native threads, and for
SMP parallelism we assume that the OS will schedule multiple OS
threads across the available CPUs.


OS threads are only used by the runtime for two reasons:

- To support non-blocking foreign calls: a foreign call
  should not block the other Haskell threads in the system from
  running, and using OS threads is the only way to ensure that.

- To support SMP parallelism.


Haskell threads are much lighter-weight (at least 100x) than OS threads.


When running on an SMP, we begin by creating the number of OS threads specified by the `+RTS -N` option, although during the course of running the program more OS threads might be created in order to continue running Haskell code while foreign calls execute.  Spare OS threads are kept in a pool attached to each `Capability` (see [\#Capabilities](commentary/rts/scheduler#capabilities)).


The RTS provides a platform-independent abstraction layer for OS
threads in [includes/rts/OSThreads.h](/trac/ghc/browser/ghc/includes/rts/OSThreads.h).

## Haskell threads


A Haskell thread is represented by a Thread State Object
([TSO](commentary/rts/storage/heap-objects#thread-state-objects)).  There are
two kinds of Haskell thread:

- A *bound* thread is created as the result of a *call-in* from
  outside Haskell; that is, a call to `foreign export` or
  `foreign import "wrapper"`.  A bound thread is tied to the
  OS thread that made the call; all further foreign calls made by
  this Haskell thread are made in the same OS thread.  (this is part
  of the design of the FFI, described in the paper 
  [ Extending the Haskell Foreign Function Inteface with Concurrency](http://www.haskell.org/~simonmar/papers/conc-ffi.pdf)).

- An *unbound* thread is created by
  `Control.Concurrent.forkIO`.  Foreign calls made by an unbound
  thread are made by an arbitrary OS thread.

## Tasks


Source files: [rts/Task.h](/trac/ghc/browser/ghc/rts/Task.h), [rts/Task.c](/trac/ghc/browser/ghc/rts/Task.c)


A Task is a further layer of abstraction over an OS thread.  One `Task` structure is created for each OS thread known to the runtime.  To get the `Task` associated with with the current OS thread, use the function `myTask`:

```wiki
  Task *myTask (void);
```


The `myTask` function is implemented using thread-local storage.


The Task contains a mutex and a condition variable used when OS threads in the runtime need to synchronise with each other or sleep waiting for a condition to occur.  The `Task` also points to the `Capability` that the `Task` currently owns (`task->cap`), or `NULL` if the `Task` does not currently own a `Capability`.


The important components of a Task are:

- The OS thread that owns this Task
- The *Capability* that this Task holds (see below)
- The current `InCall` for this Task (see below)
- A condition variable on which this Task can put itself to sleep
- Some link fields for placing the Task on various queues

## InCalls


When an in-call is made, a Task is allocated (unless the current OS thread already has a Task), and an `InCall` structure is allocated for the call.  The `InCall` structure contains

- a pointer to the `Task` that made the in-call
- a pointer to the `TSO` that is executing the call
- a slot to save the `TSO` in the event that this `TSO` needs to make a foreign call itself
- a pointer to the previous `InCall`, if the current `Task` had already made an in-call followed by an out-call that lead to this in-call


Each task points to its current `InCall`.  A worker Task (i.e. one that was created by the RTS rather than externally) also has an `InCall` structure, but in that case `incall->tso` is NULL.


When a `TSO` makes a foreign call, the current `InCall` is placed on a queue attached to the `Capability`, `cap->suspended_ccalls`, from where the garbage collector can find the `TSO`s involved in foreign calls.  If one of these threads makes another in-call into Haskell, then another `InCall` is allocated, which points back to the original `InCall` via `incall->prev_stack`.  So we have a representation of the out-call/in-call stack for each `Task`, and we can restore the previous `InCall` when an in-call returns.


A task has a small cache of spare `InCall` structures so that it can allocate a fresh one quickly and without taking any locks; this is important for in-call performance.

## Capabilities


Source files: [rts/Capability.h](/trac/ghc/browser/ghc/rts/Capability.h), [rts/Capability.c](/trac/ghc/browser/ghc/rts/Capability.c)


A Capability is a *virtual CPU* for executing Haskell code.  The
number of capabilities in the system is chosen by the `+RTS -N`
option.  This value should be chosen to be the same as the number of
real CPU cores, so that we never try to run more Haskell threads
simultaneously than we have real CPUs available.


Invariant: a task that holds a capability is not blocked in the operating system.


This makes some parts of the system simpler - for example, we can use
spin locks that spin indefinitely, because we can ensure that the spin
lock is only held by a currently executing CPU, and will therefore be
released in a finite (and short) amount of time.


Also we can maximise the advantage of our lightweight threading by not
using OS-level context switching.  We still use OS-level blocking I/O,
however - only the OS knows how to do that in general.


A Capability is in one of two states:

- It is free if `cap->running_task == NULL`.  The Capability
  is dormant, not currently executing any code.

- Otherwise, it is held by a Task, and `cap->running_task` points
  to the Task that is currently holding it.


The important components of a Capability are:

- The [registers](commentary/rts/haskell-execution#registers) of
  the virtual machine, for executing Haskell code (although while
  actually executing, some of these registers may be held in real
  machine registers, they are only saved to the Capability when
  returning to the scheduler).

- The Task that is currently animating this Capability.

- A queue of runnable Haskell threads (the *run queue*).

- A list of Haskell thrads currently making safe foreign calls.

- A list of worker OS threads.

- A list of Tasks waiting to return to Haskell from foreign calls.

- A list of Haskell threads waiting to wake up on this Capability.

### Handing over a Capability


The Task currently holding a Capability might need to relinquish it
for one of the following reasons:

- The Haskell thread at the head of the run queue is not appropriate
  for this Task: it is bound to another Task, or it is unbound and
  the current Task is bound.

- There is a Task waiting to return to Haskell from a foreign call
  (these are given priority over Haskell threads in the run queue,
  because in general they haven't had a full time slice yet).

- Another Task is trying to garbage collect (in the current
  single-threaded GC, all activity has to stop in order to GC).


The details of handing over a Capability are rather subtle, so look at
the code for the definitive picture.  Broadly speaking, when handing
over a Capability to a Task, we make the Task aware that it should
wake up and on which Capability, and we mark the Capability as free.
The Task wakes up, tries to acquire ownership of the Capability.  If
it fails because another Task is holding the Capability (this is
entirely possibly, since the Capability was marked free momentarily),
then it goes back to sleep: the other Task will release and hand it
over at some point.


One reason behind marking a Capability as free when it is handed over
is to support fast callouts.  When making a safe foreign call we have
to release the Capability, and therefore hand it over to another
worker thread.  If the foreign call is short, we don't want to incur
the cost of a context switch on returning, but since we marked the
Capability as free there's a good chance the returning Task will be
able to re-acquire it immediately and continue.  The worker that we
woke up will find that the Capability is owned, and go back to sleep
again (this may incur a double context switch if there are no free
CPUs on which to run the worker, however).

## The Scheduler's main loop


A transcript of Simon's explanation at the board:

```wiki
scheduler(cap)
{
  for (;;) {
    yieldCapability(cap);  /* give cap to anybody wanting in from outside */
    tso = popRunQueue(cap);
    result = StgRun(tso);
    case result of
      out of heap -> re-enqueue tso; call GC;
      out of stack -> enlarge tso; re-enqueue tso;
      time expired -> put tso on end of queue; /* round robin */
      finished -> 
        if (tso is a bound thread)
          return;
        else
          continue;
    }
}
```

## Sparks and the par operator


Source files: [rts/Sparks.c](/trac/ghc/browser/ghc/rts/Sparks.c), [rts/Sparks.h](/trac/ghc/browser/ghc/rts/Sparks.h).


The [par](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Parallel.html#v%3Apar) operator is used for annotating computations that could be evaluated in parallel.  See also [Parallel Haskell](http://www.haskell.org/ghc/docs/latest/html/users_guide/lang-parallel.html) in the GHC User's Guide.


Par itself is implemented in terms of the `par#` primop, which the code generator compiles into a call to `newSpark` in [rts/Sparks.c](/trac/ghc/browser/ghc/rts/Sparks.c).


Par doesn't actually create a new thread immediately; instead it places a pointer to its first argument in the *spark pool*.  The spark pool is a circular buffer, when it is full we have the choice of either overwriting the oldest entry or dropping the new entry - currently we drop the new entry (see code for `newSpark`).  Each capability has its own spark pool, so this operation can be performed without taking a lock.


So how does the spark turn into a thread?  When the scheduler spots that the current capability has no runnable threads, it checks the spark pool, and if there is a valid spark (a spark that points to a THUNK), then the spark is turned into a real thread and placed on the run queue: see `createSparkThread` in [rts/Sparks.c](/trac/ghc/browser/ghc/rts/Sparks.c).  Also, the scheduler attempts to share its available sparks with any other idle capabilities: see `schedulePushWork` in [rts/Scheduler.c](/trac/ghc/browser/ghc/rts/Scheduler.c).

## Affinity and migration

## Shutting Down