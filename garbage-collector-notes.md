# The GHC Garbage Collector Notes


These are my notes of the Glasgow Haskell Compiler’s Garbage Calloector made over my period of internship at Microsoft Research in Summer 2006. These notes are in process of constantly being updated as I study the system further. The objective of my work at MSRC is to implement a parallel GC for Haskell – one that will allow multiple threads to simultaneously garbage collect.  

## Capabilities


Lets dive right into the working of things. GHC has an abstraction called capabilities. A capability is a 1 or more OS threads. They may or maynot have processor affinities on multiprocessor machines. Each capability can run multiple Haskell threads. These Haskell threads are what are known as interpreter threads or green threads or user threads in the terminology of other systems. The OS is not aware of their presence and the switching of context between these threads is controlled purely by the Haskell runtime system. The runtime system, abbreviated as RTS is something we will keep referring to again and again. 

## Runtime System


The RTS is located in the folder “rts” of the ghc tree. It consists mostly of C code that is compiled into the resulting Haskell executable so that the required runtime services for the executable are packaged in. This is unlike .Net, JVM, Chez Scheme and other systems where the binaries rely heavily on the runtime support provided by their host VMs and thus the binaries cannot be independently deployed onto machines that that don’t have whole or part of the host VM services. Haskell executables are designed to be standalone executable requiring only standard OS services and do not usually require language support binaries. 


The tradeoff is in the fact that every Haskell binary has the RTS compiled into it, making Haskell binaries rather large. The RTS consists of facilities like the support of user threads (or Haskell threads), garbage collection etc. We are interested in focusing on Garbage Collection. However before we get into the GC, let us look at how that is connected to the rest of the system.

## The Scheduler


Most of the interesting things related to scheduling and multithreading in Haskell center around the function schedule() that is define in Schedule.c. This is the part of schedule that take a thread from the run and decides what to do with it. 

```wiki
static Capability * schedule (Capability *initialCapability, Task *task)
```


In schedule() is a pretty classical scheduler loop. I have stripped away several parts of the code here to get down to the essentials.

```
    t = popRunQueue(cap);
    prev_what_next = t->what_next;switch(prev_what_next){caseThreadKilled:caseThreadComplete:/* Thread already finished, return to scheduler. */
        ret = ThreadFinished;break;caseThreadRunGHC:{
        StgRegTable *r;
        r = StgRun((StgFunPtr) stg_returnToStackTop,&cap->r);
        cap = regTableToCapability(r);
        ret = r->rRet;break;}caseThreadInterpret:
        cap = interpretBCO(cap);
        ret = cap->r.rRet;break;default:
        barf("schedule: invalid what_next field");}
```


The scheduler picks up a thread off the run queand decides what to do with it. If it is runnable, then it calles the function StgRun() to run it. At the end of the code block, the variable “ret” is set to indicate why the the thread stopped. 


Haskell threads are not time-sliced via a timer (potentially a time rinterrupt) the way OS threads are \[cross check if there is some time sliced mechanism\]. Instead they are interreupted by certain commonly occuring events. Due to the lazy nature of Haskell thunks need to be created and values need to be computed very often. Hence the execution of a thread entails lots of of memory allocation. One of the ways the execution of a thread is interrupted is when a thread has run out of space in its current block - it then returns control back to the scheduler. 

>
> I stand corrected about the above - *We do have a time-slice mechanism: the timer interrupt (see Timer.c) sets the context_switch flag, which causes the running thread to return to the scheduler the next time a heap check fails (at the end of the current nursery block). When a heap check fails, the thread doesn't necessarily always return to the scheduler: as long as the context_switch flag isn't set, and there is another block in the nursery, it resets Hp and HpLim to point to the new block, and continues.*


A GHC block is a 4k page that is page aligned for the OS VM system.  


Here is what the scheduler does with the "ret" - 

```
switch(ret){caseHeapOverflow:
        ready_to_gc = scheduleHandleHeapOverflow(cap,t);break;caseStackOverflow:
        scheduleHandleStackOverflow(cap,task,t);break;caseThreadYielding:if(scheduleHandleYield(cap, t, prev_what_next)){// shortcut for switching between compiler/interpreter:
goto run_thread;}break;caseThreadBlocked:
        scheduleHandleThreadBlocked(t);break;caseThreadFinished:if(scheduleHandleThreadFinished(cap, task, t))return cap;
        ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);break;default:
      barf("schedule: invalid thread return code %d",(int)ret);}
```


The scheduleHandleHeapOverflow(cap,t) call decides to give the thread another block, (or a set of blocks if the thread was asking for allocation of a large object (a large object is one that is larger than a block). If the scheduleHandleHeapOverflow() function feels that there aren't enough free blocks left, it decides to Garbage Collect. This is the point at which everything else stops and the GC kicks in. 

## Stepping into the GC


The part that we are interested in is the Garbage Collector. The main entry point into the GC is the GarbageCollect() function  defined in GC.c.


The existing GC in GHC is a single threaded one. When the RTS detects memory pressure the GC stops all the Haskell threads and then one thread that does the garbage collection and then resumes all the other suspended threads. On a multiprocessor machine such a design is obviously a bottle neck and it is desirable to garbage collect using multiple parallel threads. 

## [GcDataStructures](gc-data-structures)

## Allocation

## Scavenging

## Copy

# [MotivationForParallelization](motivation-for-parallelization)

# [ProblemsCompilingGhc](problems-compiling-ghc)

---


Roshan James (rpjames \[at\] cs \[dot\] indiana \[dot\] edu)

---