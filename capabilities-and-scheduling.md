
Back to [GarbageCollectorNotes](garbage-collector-notes)

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
