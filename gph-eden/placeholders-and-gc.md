
This page mentions various aspects of the parallel RTS which have not found a home yet.

## Placeholder Creation


When we expect or globalise data, a BLACKHOLE closure (placeholder for data residing in another PEs heap) has to be added to the runtime system, to be called from a primitive operation (Eden) or from the packing code when globalising (GpH).


This function is implemented inside `rts/parallel/Pack.c`. Its name and prototype is `createBH(Capability* cap)`.

## Runtime tables


A runtime table in the Eden system tracks processes and their created placeholders for communication and termination.


The Eden-6.8.3 module has been taken as the basis. One major change: Since we have to avoid changing the data structure StgTSO, we do  not save message receiver and process ID there any more, but in hash tables maintained inside RTTables.  Still to be solved / implemented is the use case that a PE receives  a TERMINATE message and wants to terminate a known sender thread. With the new version, the thread has to be looked up by its ID only, and the  (threaded) system does not provide a single structure for doing this lookup. We can however follow the pattern found in garbage collection code (resurrecting threads) and the forkOS implementation (killing all threads).

## messages between PEs


Messages between the machines are identified by their message tags, defined in `rts/parallel/PEOpCodes.h`. Main message processing function is `processMessages()` inside Schedule.c, which blocks on receiving and uses DataComms for data messages (DATA or HEAD).


Messages fall in three categories:

1. system (READY, FINISH)
1. data (RFORK, DATA, HEAD, CONSTR)
1. management (CONNECT, TERMINATE)


System message are mostly hidden inside the implementation of MPSystem.h, only FINISH is exposed and results in immediate scheduler shutdown when processed.


Management messages manipulate the local runtime tables, and have straightforward implementations.


Functionality for data messages is implemented in a new module `rts/parallel/DataComms.c` (former HLComms.c), containing functions `sendMsg()` and a wrapper function for sending, and data message processing functionality.  In order to process a HEAD message (stream communication, this module uses a function `createListNode`, which resides in `rts/parallel/Pack.c` as `createBH()` (rationale: the latter module manipulates internal heap structures of GHC).

## GC and Process Termination


During GC, the runtime can detect placeholders which are not needed by the local computation. The senders sending to these inports will be informed by TERMINATE messages. In order for this to work, we have to keep alive all threads which are registered with a process in the runtime table, since threads may be blocked on a placeholder that looks like garbage to the local system, but will be updated from outside.



Functionality for this has been implemented inside the storage management code (due to the change in the runtime tables preventing direct evacuation of known threads). *The code needs a review*.



[--\> back to GpHEden](gp-h-eden)


