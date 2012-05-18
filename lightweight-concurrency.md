# Lightweight Concurrency in GHC


This page documents the effort to move GHC's concurrency support from its current location in the C part of the runtime system (RTS) to Haskell. This works builds on Peng Li's earlier work ([ http://community.haskell.org/\~simonmar/papers/conc-substrate.pdf](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)). This page contains information about the design, implementation, problems and potential solutions for building user-level concurrency primitives in GHC. Currently, the focus is on user-level implementation of non-deterministic parallelism in GHC ([Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html)).


Lightweight concurrency implementation resides in the `ghc-lwc` branch in the git repo.

## Table of Contents

- [Table of Contents](lightweight-concurrency#table-of-contents)
- [Introduction](lightweight-concurrency#introduction)
- [Background - GHC's Concurrency RTS](lightweight-concurrency#)
- [Concurrency Substrate](lightweight-concurrency#concurrency-substrate)

  - [PTM](lightweight-concurrency#ptm)
  - [One-shot Continuations](lightweight-concurrency#)

    - [Return value of a switching transaction](lightweight-concurrency#return-value-of-a-switching-transaction)
  - [SCont Status](lightweight-concurrency#scont-status)
- [Abstracting the Scheduler](lightweight-concurrency#abstracting-the-scheduler)
- [User-level Concurrency](lightweight-concurrency#)

  - [Schedulers](lightweight-concurrency#schedulers)
  - [MVars](lightweight-concurrency#mvars)
- [Capabilities and Tasks](lightweight-concurrency#capabilities-and-tasks)

  - [SCont Affinity](lightweight-concurrency#scont-affinity)
  - [Bound SCont](lightweight-concurrency#bound-scont)
- [Related Work](lightweight-concurrency#related-work)

## Introduction


GHC has a rich support for concurrency (forkIO, MVars, STM, Asynchronous exceptions, bound threads, safe FFI, transparent scaling on multicores, etc.) and a fast and robust runtime system. However, the concurrency support is implemented in C and baked into the RTS. The concurrency primitives non-trivially interact among each other, and along with the lightweight thread scheduler, through a cascade of locks and condition variables. Often times, the invariants on which RTS fields can be accessed when are expressed as comments, and enforced through assertions (See [here](/trac/ghc/browser/rts/Task.h?rev=085c7fe5d4ea6e7b59f944d46ecfeba3755a315b#L37)[](/trac/ghc/export/085c7fe5d4ea6e7b59f944d46ecfeba3755a315b/ghc/rts/Task.h#L37) for one such fascinating example). This policy of enforcing through assertions keeps the overheads low, but makes the task of modifying and extending the runtime cumbersome.


But, why would we be interested in modifying GHC's concurrency environment? There are several good reasons to believe that a particular concurrent programming model, or a scheduling policy would not suit every application. With the emergence of many-core processors, we see NUMA effects becoming more prominent, and applications might benefit from NUMA aware scheduling and load balancing policies. Moreover, an application might have a better knowledge of the scheduling requirements -- a thread involved in user-interaction is expected to be given more priority over threads performing background processing. We might want to experiment with various work-stealing or work-sharing policies. More ambitiously, we might choose to build X10 style async-finish or Cilk style spawn-sync task parallel abstractions. Ideally, we would like allow the programmer to write an application that can  seamlessly combine all of these different programming abstractions, with pluggable scheduling and load balancing policies.


While we want to provide flexibility to the Haskell programmer, this should not come at a cost of added complexity and decreased performance. This idea reflects in the synchronization abstractions exposed to the programmer - [Primitive Transactional Memory(PTM)](lightweight-concurrency#ptm)), and our decision to keep certain pieces of the concurrency puzzle in the RTS ([Safe FFI](lightweight-concurrency#),[Blackholes](lightweight-concurrency#)). One would think lifting parts of the runtime system to Haskell, and retaining other parts in C, would complicate the interactions between the concurrency primitives and schedulers. We abstract the scheduler interface using PTM monads, which simplifies the interactions. The figure below captures the key design principles of the proposed system.

[](/trac/ghc/attachment/wiki/LightweightConcurrency/GHC_LWC_Key.jpg)


Although implementing concurrency primitives as a library is hardly a novel idea, the aim of this work is to bring it to the GHC programmer, without having to give up any of the existing concurrency features in return.

## Background - GHC's Concurrency RTS


For the high-level design principle for the current scheduler, see [ Scheduler](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler).

## Concurrency Substrate


The idea of the concurrency substrate is to provide a minimal set of primitives over which a variety of user-level concurrency libraries can be implemented. As such, the concurrency substrate must provide a way to create threads, a way to schedule them, and a synchronization mechanism in a multiprocessor context. Whereas, the Creation and maintenance of schedulers and concurrent data structures is the task of the concurrency library. Concurrency substrate resides at libraries/base/LwConc/Substrate.hs.

### PTM


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

### One-shot Continuations


The concurrency substrate enables creation and scheduling of I/O-performing computations through *one-shot continuations*. An SCont (stack continuation) is a suspended I/O computation, which is in fact just a reference to a TSO object. Capturing the current continuation is just getting a reference to the current TSO, and hence is very fast. SCont interface is shown below:

```wiki
data SCont
newSCont         :: IO () -> IO SCont
switch           :: (SCont -> PTM SCont) -> IO ()
getCurrentSCont  :: PTM SCont
switchTo         :: SCont -> PTM
```


Given an I/O-performing computation `newSCont` returns a reference to an SCont which when scheduled, will perform the I/O action. The switch primitive is a bit unique. Switch takes a function which is applied to the current continuation. The result of the function is PTM transaction of type PTM SCont. This transaction can encapsulate the actions necessary for appending the current SCont to the scheduler and fetching the next SCont to switch to. The switch primitive performs this transaction atomically, and switches control to the resultant SCont. 


Performing the body of switch atomically in a transaction avoids the nasty race conditions usually seen in multicore runtimes where one-shot continuations are used for modelling schedulers. In such systems, there are often cases where before the switch primitive has had a chance to return, another processor picks up the current continuation (appended to the scheduler) and tries to switch to it. It becomes necessary to go for complicated solutions such as releasing the scheduler locks after the target thread resumes execution to prevent races. In our case, PTM eliminates the need for such a mechanism - the other processor would not be able to access the current SCont, unless the transaction has committed and control has switched to the target SCont. Primitive `getCurrentSCont` returns a reference to the current SCont under PTM. Primitive `switchTo` commits the current PTM transaction and switches to the given SCont. As we will see, these two primitives are necessary for [abstracting the scheduler](lightweight-concurrency#abstracting-the-scheduler). 

#### Return value of a switching transaction


Since switchTo eagerly commits the transaction, the code that follows switchTo is not evaluated. This is a problem if the transaction that contains switchTo has a type different than PTM (). Consider the following code:

```wiki
s :: String <- atomically $ do {
  sc <- getCurrentSCont;
  -- save the current SCont somewhere
  switchTo someSCont;
  return "This is never evaluated!"
}
print s
```


The type of the transaction that contains switchTo is PTM string, and atomically performing the transaction is expected to return a String value. But the value returned when the current SCont resumes execution (after switchTo) is a (). Our solution is to make switchTo return a `error "Attempting to use return value of a switched transaction"`, and any attempt to use the return value of a switching transaction throws a runtime error.

### SCont Status


Of course, care must be taken to ensure that the control does not switch to an SCont that is either running, blocked on an MVar, or completed. But how do we know whether the given SCont is ready to run? We expect the scheduler writer or library implementer to indicate the status of SCont before switching. SCont status API is show below.

```wiki
data SContStatus = SContRunning |           -- SCont is currently running
                   SContKilled  |           -- SCont was killed by an (asynchronous) exception
	           SContSwitched SContSwitchReason
data SContSwitchReason = Yielded |          -- SCont has yielded, but runnable
                         BlockedInHaskell | -- SCont is blocked on a user-level concurrent 
                                            -- data structure (MVars and such)
                         BlockedInRTS |     -- SCont is blocked on a foreign call, blackhole, etc,.
                         Completed          -- SCont has run to completion

setSContSwitchReason :: SCont -> SContSwitchReason -> PTM ()
getSContStatus       :: SCont -> PTM SContStatus
```


Any attempt to switch to an SCont with status other than `SContSwitched Yielded` throws an exception. Primitive `setSContSwitchReason` updates the status of SCont. Since setSContSwitchReason is a PTM action, the effect of updating the status takes place when the transaction commits and the control has switched to another SCont. This avoids any race conditions that might be involved in reading the status of an SCont before it has switched. 


Before a switch operation, we expect the programmer to indicate the reason for switching through setScontSwitchReason. Exception is raised by the switch primitives if a switch reason has not been provided. When a switched SCont resumes execution, its status is automatically updated to `SContRunning`. 

## Abstracting the Scheduler


Concurrency substrate does not impose any structure on the user-level schedulers. The programmer might choose to implement a single scheduler for the entire system or a scheduler per capability. The schedulers might also be hierarchical, with pluggable load-balancing policies. However, we need a uniform interface such that the concurrency libraries, STM, asynchronous exceptions, safe-foreign calls, blackholes and other such mechanisms can interact with the user-level scheduler. For this purpose, we introduce the notion of `scheduler actions`, which is expected for every SCont. The substrate interface for scheduler actions is shown below:

```wiki
------ Schedule SCont Action :: SCont -> PTM () ------

getScheduleSContAction :: SCont -> PTM (SCont -> PTM ())
setScheduleSContAction :: SCont -> (SCont -> PTM ()) -> PTM ()

----------- Yield Control Action :: PTM () -----------

getYieldControlAction :: SCont -> PTM (PTM ())
setYieldControlAction :: SCont -> PTM () -> PTM ()
```


Abstractly, given an SCont, the scheduleSContAction appends the SCont to a scheduler. The yieldControlAction picks an SCont from a scheduler and switches to it. In order to make the ideas more concrete, let us assume that we have a very simple round-robin scheduler, implemented as a `PVar[SCont]`. One possible implementation of scheduler actions for this scheduler is given below.

```wiki
scheduleSContAction :: SCont -> PTM () 
scheduleSContAction sc = do
  sched :: PVar [SCont] <- -- get sched 
  contents :: [SCont] <- readPVar sched 
  setSContSwitchReason sc Yielded -- sc is ready to be run
  writePVar $ contents ++ [sc]


yieldControlAction :: PTM () 
yieldControlAction = do
  sched :: PVar [SCont] <- -- get sched 
  contents :: [SCont] <- readPVar sched 
  case contents of
    x:tail -> do { 
      writePVar $ contents tail; 
      switchTo x -- DOES NOT RETURN
    } 
    otherwise -> ...
```


The implementation is pretty straight-forward; scheduleSContAction appends the given scont to the back of the list, and yieldControlAction picks an SCont from the front of the list and switches to it. Having the scheduler actions as PTM actions ensures that the operations on the scheduler are always properly synchronized. Notice that scheduleSContAction returns while yieldControlAction does not. We expect every user-level thread (SCont) to be associated with a scheduler. Typically, when a new SCont is created, it is immediately associated with a scheduler.

## User-level Concurrency


Now that we have defined an abstract scheduler interface, lets look at how to construct user-level concurrency primitives using the scheduler actions. 

### Schedulers


Since our first goal is to implement GHC's concurrency support in Haskell, let us start with`forkIO` and `yield`. These two primitives are sufficient for a simple cooperatively scheduled lightweight thread system. In order to make the presentation cleaner, assume that we have the following helper functions.

```wiki
getSSA = getScheduleSContAction
setSSA = setScheduleSContAction
getYCA = getYieldControlAction
setYCA = setYieldControlAction
```


Primitive `yield` appends the current SCont to the scheduler, picks the next SCont from the scheduler and switches to it. We utilize the scheduler actions to achieve this. The implementation of yield is shown below. It is important to notice that yield does not assume anything about the implementation of the scheduler except for the scheduler actions. Hence, there is no need to re-implement yield primitive for every scheduler, thus minimizing the overhead of implementing new schedulers.

```wiki
yield :: IO ()
yield = atomically $ do
  s <- getCurrentSCont
  -- Append current SCont to scheduler
  ssa <- getSSA s
  enque :: PTM () <- ssa a
  enque
  -- Switch to next SCont from scheduler
  switchToNext :: PTM () <- getYCA s
  switchToNext
```


Primitive `forkIO` also follows the strategy of utilizing scheduler actions. The implementation of forkIO is shown below. 

```wiki
forkIO :: IO () -> IO SCont
forkIO f = do
  -- Switch to next thread after completion
  let epilogue = atomically $ do {
    sc <- getCurrentSCont;
    setSContSwitchReason Completed;
    switchToNext <- getYCA sc;
    switchToNext
  }
  ns <- newSCont (f >> epilogue)  
  atomically $ do {
    s <- getCurrentSCont;
    -- Initialize scheduler actions
    ssa <- getSSA s;
    setSSA ns ssa;
    yca <- getYCA s;
    setYCA ns yca;
    -- Append the new SCont to current SCont's scheduler
    appendAct <- ssa ns;
    appendAct
  }
  return ns
```


Here, the thread that invokes forkIO initializes the new SCont (`ns`) with its own scheduler actions, and appends it to the scheduler. After the newly created SCont finishes execution, the control must switch to another thread in the scheduler. This is captured by the `epilogue`.

### MVars

[MVars](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html) are one of the basic synchronization mechanisms exposed by GHC's concurrency library. A simple user-level implementation of MVar might look like:

```wiki
newtype MVar a = MVar (PVar (State a))
data State a = Full a [(a, PTM ())] | Empty [(PVar a, PTM ())]
```


MVar is either empty with a list of pending takers, or full with a value and a list of pending putters. The PTM () action in the full and empty list represents the logic necessary for waking up the pending putters and takers. The following snippet shows the implementation of `takeMVar`.

```wiki
takeMVar :: MVar a -> IO a 
takeMVar (MVar ref) = do
  hole <- atomically $ newPVar undefined 
  atomically $ do
    st <- readPVar ref 
    case st of
      Empty ts -> do 
        s <- getCurrentSCont 
        ssa <- getSSA s 
        wakeup <- ssa s 
        writePVar ref $ v
          where v = Empty $ ts++[(hole, wakeup)] 
        switchToNext <- getYCA s 
        switchToNext
      Full x ((x', wakeup):ts) -> do 
        writePVar hole x 
        writePVar ref $ Full x' ts 
        wakeup
      otherwise -> ... 
  atomically $ readPVar hole
```


Primitive `takeMVar` first creates a hole, which will contain the result. If the MVar happens to be empty, we fetch the scheduleSContAction for the current thread, and append append it along with the hole to the end of the queue. This enqueued PTM action, when executed, will append the current thread to its scheduler. Finally, the control switches to the next runnable thread using the yieldControlAction. All of these actions occur atomically within the same transaction.


If the MVar is full with a pending writer, we first fill the hole with the value. Then, MVar's status is updated with the enqueued value and the rest of the writers. Finally, we execute the dequeued PTM action to place the writer into its corresponding scheduler.


Notice that just like yield and forkIO, takeMVar is scheduler agnostic; the MVar implementation is cleanly separated from the scheduler implementation. Moreover, the same MVar might be shared between threads from different schedulers since they utilize the uniform scheduler interface. Since the scheduler actions are PTM actions, actions from different schedulers can be composed together elegantly and simplifies reasoning about synchronization. 


As an aside, the race condition in [swapMVar](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent-MVar.html#v%3AswapMVar) can be eliminated with the help of PTM abstraction. TODO show example. Thus, PTM abstraction makes it easy to construct correct concurrent data-structures. 

## Capabilities and Tasks


Whatever be the concurrency model, we would like to retain the non-programmatic control over parallelism (using +RTS -N). Just like in the current system, this runtime parameter controls the number of capabilities. Cores are system resources and hence, the control over their allocation to different processes should be a property of the context under which the programs are run. For example, in a multi-programmed environment, it might be wiser to run the programs on a fewer cores than available to avoid thrashing. At the very least, this will avoid the cases where a poorly written concurrency library would not bring down the performance of the entire system. 


We retain the task model of the current runtime system. There is a one-to-one mapping between tasks and system threads. Tasks are not exposed to the programmer and is transparently managed by the RTS.

### SCont Affinity


Every SCont is bound to a particular capability and only that capability is capable of running the SCont. Switching to an SCont that is not bound to the current capability raises a runtime error. SCont affinity interface is shown below.

```wiki
setSContCapability :: SCont -> Int -> IO ()
getSContCapability :: SCont -> PTM Int
```


A newly created SCont is bound to the current capability. Primitive `setSContCapability` is used to change the affinity of an SCont that belongs to the current capability. Trying to change the affinity of an SCont that belongs to a different capability throws a runtime error. 

### Bound SCont


Similar to [bound threads](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html#g:9) concurrency substrate supports bound SConts. The interface is shown below.

```wiki
newBoundSCont          :: IO () -> IO SCont
isCurrentSContBound    :: IO Bool
rtsSupportsBoundSConts :: Bool
```


Creating a bound SCont creates a new task, which is the only task capable of running the bound SCont. When switching to a bound SCont, the RTS transparently switches to the corresponding bound task. Similarly, when switching away from a bound SCont, the RTS suspends the current bound task, and switches to another appropriate task. However, an unbounded SCont (created through `newSCont` primitive) might be run on any unbounded task (referred to as worker tasks). New worker tasks might be created by the RTS on demand.

## Related Work

- [Concurrent Programming in GHC](lightweight-concurrency#)
- [ Lightweight Concurrent Primitives for GHC](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)
- [ Tackling the awkward squad](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/)
- [ Runtime Support for Multicore Haskell](http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf)