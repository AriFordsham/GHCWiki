# Lightweight Concurrency in GHC


This page documents the effort to move GHC's concurrency support from its current location in the C part of the runtime system (RTS) to Haskell. This works builds on Peng Li's earlier work ([ http://community.haskell.org/\~simonmar/papers/conc-substrate.pdf](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)). This page contains information about the design, implementation, problems and potential solutions for building user-level concurrency primitives in GHC. Currently, the focus is on user-level implementation of non-deterministic parallelism in GHC ([Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html)).


Lightweight concurrency implementation resides in the `ghc-lwc` branch in the git repo.

## Introduction


GHC has a rich support for concurrency (forkIO, MVars, STM, Asynchronous exceptions, bound threads, safe FFI, transparent scaling on multicores, etc.) and a fast and robust runtime system. However, the concurrency support is implemented in C and baked into the RTS. The concurrency primitives non-trivially interact among each other, and along with the lightweight thread scheduler, through a cascade of locks and condition variables. Often times, the invariants on which RTS fields can be accessed when are expressed as comments, and enforced through assertions (See [here](/trac/ghc/browser/rts/Task.h?rev=085c7fe5d4ea6e7b59f944d46ecfeba3755a315b#L37)[](/trac/ghc/export/085c7fe5d4ea6e7b59f944d46ecfeba3755a315b/ghc/rts/Task.h#L37) for one such fascinating example). This policy of enforcing through assertions keeps the overheads low, but makes the task of modifying and extending the runtime cumbersome.


But, why would we be interested in modifying GHC's concurrency environment? There are several good reasons to believe that a particular concurrent programming model, or a scheduling policy would not suit every application. With the emergence of many-core processors, we see NUMA effects becoming more prominent, and applications might benefit from NUMA aware scheduling and load balancing policies. Moreover, an application might have a better knowledge of the scheduling requirements -- a thread involved in user-interaction is expected to be given more priority over threads performing background processing. We might want to experiment with various work-stealing or work-sharing policies. More ambitiously, we might choose to build X10 style async-finish or Cilk style spawn-sync task parallel abstractions. Ideally, we would like allow the programmer to write an application that can  seamlessly combine all of these different programming abstractions, with pluggable scheduling and load balancing policies.


While we want to provide flexibility to the Haskell programmer, this should not come at a cost of added complexity and decreased performance. This idea reflects in the synchronization abstractions exposed to the programmer - [Primitive Transactional Memory(PTM)](lightweight-concurrency#ptm)), and our decision to keep certain pieces of the concurrency puzzle in the RTS ([Safe FFI](lightweight-concurrency#),[Blackholes](lightweight-concurrency#)). One would think lifting parts of the runtime system to Haskell, and retaining other parts in C, would complicate the interactions between the concurrency primitives and schedulers. We abstract the scheduler interface using PTM monads, which simplifies the interactions. The figure below captures the key design principles of the proposed system.

[](/trac/ghc/attachment/wiki/LightweightConcurrency/GHC_LWC_Key.jpg)


Although implementing concurrency primitives as a library is hardly a novel idea, the aim of this work is to bring it to the GHC programmer, without having to give up any of the existing concurrency features in return.

## Background - GHC's Concurrency RTS


For the high-level design principle for the current scheduler, see [ Scheduler](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler).

## Design

### Concurrency Substrate


The idea of the concurrency substrate is to provide a minimal set of primitives over which a variety of user-level concurrency libraries can be implemented. As such, the concurrency substrate must provide a way to create threads, a way to schedule them, and a synchronization mechanism in a multiprocessor context. Whereas, the Creation and maintenance of schedulers and concurrent data structures is the task of the concurrency library. Concurrency substrate resides at libraries/base/LwConc/Substrate.hs.

#### PTM


The only synchronization mechanism exposed by the concurrency substrate is a primitive transactional memory (PTM). Locks and condition variables can be notoriously difficult to use, especially in an environment with user-level threads, where they tend to interfere with the scheduler. Moreover, they do not compose elegantly with lazy evaluation. PTM interface is shown below:

```wiki
data PTM a
data PVar a
instance Monad PTM

newPVar    :: a -> PTM (PVar a)
readPVar   :: PVar a -> PTM a
writePVar  :: PVar a -> a -> PTM ()
atomically :: PTM a -> IO a
```


A PTM transaction may allocate, read and write transactional variables of type `PVar a`. It is important to notice that PTM does not provide a blocking `retry` mechanism. Such a blocking action needs to interact with the scheduler, to block the current thread and resume another thread. We will see [later](lightweight-concurrency#) how to allow such interactions while not imposing any restriction on the structure of the schedulers.

#### One-shot continuations


The concurrency substrate enables creation and scheduling of I/O-performing computations through *one-shot continuations*. An SCont (stack continuation) is a suspended I/O computation, which is in fact just a reference to a TSO object. Hence, capturing the current continuation is just getting a reference to the current TSO. SCont interface is shown below:

```wiki
data SCont
newSCont         :: IO () -> IO SCont
switch           :: (SCont -> PTM SCont) -> IO ()
getCurrentSCont  :: PTM SCont
switchTo         :: SCont -> PTM
```


Given an I/O-performing computation `newSCont` returns a reference to an SCont which when scheduled, will perform the I/O action. The switch primitive is a bit unique. Switch takes a function which is applied to the current continuation. The result of the function is PTM transaction of type PTM SCont. This transaction can encapsulate the actions necessary for appending the current SCont to the scheduler and fetching the next SCont to switch to. The switch primitive performs this transaction atomically, and switches control to the resultant SCont. 


Performing the body of switch atomically in a transaction avoids the nasty race conditions usually seen in multicore runtimes where one-shot continuations are used for modelling schedulers. In such systems, there are often cases where before the switch primitive has had a chance to return, another processor picks up the current continuation (appended to the scheduler) and tries to switch to it. It becomes necessary to go for complicated solutions such as releasing the scheduler locks after the target thread resumes execution to prevent races. In our case, PTM eliminates the need for such a mechanism - the other processor would not be able to access the current SCont, unless the transaction has committed and control has switched to the target SCont.


Of course, care must be taken to ensure that the control does not switch to an SCont that is either running or completed.

### Capabilities and Tasks


Whatever be the concurrency model, we would like to retain the non-programmatic control over parallelism (using +RTS -N). Just like in the current system, this runtime parameter controls the number of capabilities. Cores are system resources and hence, the control over their allocation to different processes should be a property of the context under which the programs are run. For example, in a multi-programmed environment, it might be wiser to run the programs on a fewer cores than available to avoid thrashing. At the very least, this will avoid the cases where a poorly written concurrency library would not bring down the performance of the entire system.


We retain the task model of the current runtime system. There is a one-to-one mapping between tasks and system threads. A bound SCont has its own bound task, which is the only task capable of running the bound SCont. However, an unbounded SCont might be run on any unbounded task (referred to as worker tasks). New worker tasks are created whenever the number of available tasks is less than the number of capabilities.

## Related Work

- [Concurrent Programming in GHC](lightweight-concurrency#)
- [ Lightweight Concurrent Primitives for GHC](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)
- [ Tackling the awkward squad](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/)
- [ Runtime Support for Multicore Haskell](http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf)