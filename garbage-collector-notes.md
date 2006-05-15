# The GHC Garbage Collector Notes


These are my notes of the Glasgow Haskell Compiler’s Garbage Calloector made over my period of internship at Microsoft Research in Summer 2006. These notes are in process of constantly being updated as I study the system further. The objective of my work at MSRC is to implement a parallel GC for Haskell – one that will allow multiple threads to simultaneously garbage collect.  

## Capabilities


Lets dive right into the working of things. GHC has an abstraction called capabilities. A capability is a 1 or more OS threads. They may or maynot have processor affinities on multiprocessor machines. Each capability can run multiple Haskell threads. These Haskell threads are what are known as interpreter threads or green threads or user threads in the terminology of other systems. The OS is not aware of their presence and the switching of context between these threads is controlled purely by the Haskell runtime system. The runtime system, abbreviated as RTS is something we will keep referring to again and again. 

## Runtime System


The RTS is located in the folder “rts” of the ghc tree. It consists mostly of C code that is compiled into the resulting Haskell executable so that the required runtime services for the executable are packaged in. This is unlike .Net, JVM, Chez Scheme and other systems where the binaries rely heavily on the runtime support provided by their host VMs and thus the binaries cannot be independently deployed onto machines that that don’t have whole or part of the host VM services. Haskell executables are designed to be standalone executable requiring only standard OS services and do not usually require language support binaries. 


The tradeoff is in the fact that every Haskell binary has the RTS compiled into it, making Haskell binaries rather large. The RTS consists of facilities like the support of user threads (or Haskell threads), garbage collection etc. We are interested in focusing on Garbage Collection. However before we get into the GC, let us look at how that is connected to the rest of the system.

## The Scheduler


Most of the interesting things related to scheduling and multithreading in Haskell centers around the function “schedule” that is define in Schedule.c

```wiki
static Capability * schedule (Capability *initialCapability, Task *task)

In schedule is a pretty classical schedule loop. I have stripped several parts of the code here to get down to the essentials.

    t = popRunQueue(cap);
    prev_what_next = t->what_next;

    switch (prev_what_next) {
	
    case ThreadKilled:
    case ThreadComplete:
	/* Thread already finished, return to scheduler. */
	ret = ThreadFinished;
	break;
	
    case ThreadRunGHC:
    {
	StgRegTable *r;
	r = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
	cap = regTableToCapability(r);
	ret = r->rRet;
	break;
    }
    
    case ThreadInterpret:
	cap = interpretBCO(cap);
	ret = cap->r.rRet;
	break;
	
    default:
	barf("schedule: invalid what_next field");
    }
```


The scheduler picks up a thread off the run queand decides what to do with it. If it is runnable, then it called StgRun to run it. It then later examines the “ret” value that is set to see why the the thread stopped. 


Haskell threads are not time sliced via timer in the way that OS threads are. \[cross check is there is some time sliced mechanism\] Instead they are interreupted by certain commonly occuring events. Due to the lazy nature of Haskell thunks need to be created and values need to be computed very often. Hence the execution of a thread entails lots of of memory allocation. Whenever a thread has run out of space in its current block, it returns control back to the scheduler. 

```wiki
    switch (ret) {
    case HeapOverflow:
	ready_to_gc = scheduleHandleHeapOverflow(cap,t);
	break;

    case StackOverflow:
	scheduleHandleStackOverflow(cap,task,t);
	break;

    case ThreadYielding:
	if (scheduleHandleYield(cap, t, prev_what_next)) {
            // shortcut for switching between compiler/interpreter:
	    goto run_thread; 
	}
	break;

    case ThreadBlocked:
	scheduleHandleThreadBlocked(t);
	break;

    case ThreadFinished:
	if (scheduleHandleThreadFinished(cap, task, t)) return cap;
	ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
	break;

    default:
      barf("schedule: invalid thread return code %d", (int)ret);
    }
```


The scheduleHandleHeapOverflow(cap,t) call decides to give the thread another block, (or a set of blocks if the thread was asking for allocation of a large object – an object that is larger than a block). If however the scheduleHandleHeapOverflow() function feels that there is not enough free block left, then it decides to Garbage Collect.

## Stepping into the GC


The part that we are interested in is the Garbage Collector. The main entry point into the GC is the GarbageCollect() function  defined in GC.c.


The existing GC in GHC is a single threaded one. When the RTS detects memory pressure the GC stops all the Haskell threads and then one thread that does the garbage collection and then resumes all the other suspended threads. On a multiprocessor machine such a design is obviously a bottle neck and it is desirable to garbage collect using multiple parallel threads. 

## GC Data Structures

## Scavenging

## Copy

## Measurement of Block Distance while Scavenging

## Compiling GHC


I think it is useful to have a small section about compiling GHC. I ran into several magic problems getting GHC to build. I don’t fully understand the reasons for some of the fantastic sounding ones, however its worth mentioning them. Some of these are just general discipline guidelines, but are useful to keep in mind.

### Problem 1


Make sure that you do actually have the latest versions of everything involved. These include:

- Darcs (a version control system that GHC is shifting towards)
- GHC source (get it using Darcs)
- Alex (the Lexer generator)
- Happy (the Parser generator)


Since the compilation of Haskell is very Unix styled, on Windows (I work using a Win XP machine), one needs to add several unix tools to windows. I don’t know why GHC building doesn’t target SFU on windows yet – maybe that’s something that they just haven’t got down to doing yet. Here are the unix-ish components that you need:

- MinGW (The compiler set, please get the latest – they have a downloading installer available somewhere, I used that one. I had several magic problems with many packaged binaries that I found on the net).
- MSYS (These maybe downloaded as binaries from the Msys site).
- MSYS DTK. 


The GHC compiler notes explains all of this rather well. 

### Problem 2


Make sure your path is right. Make sure you get the right versions of things in your path. As an example, I have SFU binaries in my path before I had the MSYS ones they apparently don’t like each other too much.

### Problem 3


Watch out for what files you edit. There are some files in the build process are parsed by a simplistic C preprocessor and will not understand C++ style comments. Here are a list of these files:


\[get file list\]

### Problem 4


This is a classical magic problem. I got GHC to build on my desktop by it would not my laptop. As a matter of fact when I type in “autoreconf” the machine freezes up after about 30 seconds. Very puzzling. 


This behavior would often end with power cycling the laptop and was rather frustrating for a while. After studying the process with procexp, filemon and some standard monitoring tools it seemed like the “Logitech LVPrcSrv module” related to my Logitech webcam seems to hog the CPU. Since I wasn’t using the webcam, I killed the process and the build went through fine. At this point I can’t guess at what the relationship is or why there should be one. 

---


Roshan James (rpjames \[at\] cs \[dot\] indiana \[dot\] edu)
