# Using DTrace with GHC


GHC 6.13 includes DTrace probes in the runtime system.  Currently, these probes correspond to the events of the [EventLog](event-log) framework and are only available on Mac OS X.


It is straight forward to extend the current implementation with additional probes, and due to the lightweight nature of DTrace, new probes could inspect the runtime system and running Haskell program even in a more fine-grained manner.

## Portability


User-space DTrace probes are implemented differently on Mac OS X than in the original DTrace implementation; see under the heading **BUILDING CODE CONTAINING USDT PROBES** at [ http://developer.apple.com/mac/library/documentation/Darwin/Reference/ManPages/man1/dtrace.1.html](http://developer.apple.com/mac/library/documentation/Darwin/Reference/ManPages/man1/dtrace.1.html).  Nevertheless, it shouldn't be too hard to enable these probes on other platforms, too.
