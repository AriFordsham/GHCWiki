## Introduction


GHC has a rich support for concurrency (forkIO, MVars, STM, Asynchronous exceptions, bound threads, safe FFI, transparent scaling on multicores, etc.) and a fast and robust runtime system. However, the concurrency support is implemented in C and baked into the RTS. The concurrency primitives interact among each other, and with the lightweight thread scheduler, non-trivially through a cascade of locks and condition variables. 
