# Using DTrace with GHC


GHC 6.13 includes DTrace probes in the runtime system.  Currently, these probes correspond to the events of the [EventLog](event-log) framework and are only available on Mac OS X (from Leopard onwards).


It is straight forward to extend the current implementation with additional probes, and due to the lightweight nature of DTrace, new probes could inspect the runtime system and running Haskell program in an even more fine-grained manner.

## Probe description


The following probes are available:

<table><tr><th>`create-thread (capability, tid)`</th>
<td>
Triggered when a new runtime thread is created.  Reports the capability on which the thread is created and the new thread's thread id.
</td></tr>
<tr><th>`run-thread (capability, tid)`</th>
<td>
Indicates that the given thread starts to run on the specified capability.
</td></tr>
<tr><th>`stop-thread (capability, tid)`</th>
<td>
The identified thread stops execution on the given capability.
</td></tr></table>

## Probe definition


The provider is defined as follows:

```wiki
provider HaskellEvent {

  // scheduler events
  probe create__thread (EventCapNo, EventThreadID);
  probe run__thread (EventCapNo, EventThreadID);
  probe stop__thread (EventCapNo, EventThreadID, EventThreadStatus);
  probe thread__runnable (EventCapNo, EventThreadID);
  probe migrate__thread (EventCapNo, EventThreadID, EventCapNo);
  probe run__spark (EventCapNo, EventThreadID);
  probe steal__spark (EventCapNo, EventThreadID, EventCapNo);
  probe shutdown (EventCapNo);
  probe thread_wakeup (EventCapNo, EventThreadID, EventCapNo);
  probe gc__start (EventCapNo);
  probe gc__end (EventCapNo);
  probe request__seq__gc (EventCapNo);
  probe request__par__gc (EventCapNo);
  probe create__spark__thread (EventCapNo, EventThreadID);

  // other events
  probe startup (EventCapNo);
  probe user__msg (EventCapNo, char *);
  probe gc__idle (EventCapNo);
  probe gc__work (EventCapNo);
  probe gc__done (EventCapNo);

};
```


where we have

```wiki
typedef uint32_t EventThreadID;
typedef uint16_t EventCapNo;
typedef uint16_t EventThreadStatus;
```


The two events `EVENT_LOG_MSG` and `EVENT_BLOCK_MARKER` are not supported.  The former doesn't appear to be used and the latter appears to be an artefact of the event log file format.

## Portability


User-space DTrace probes are implemented differently on Mac OS X than in the original DTrace implementation; see under the heading *BUILDING CODE CONTAINING USDT PROBES* in the [ Mac OS X dtrace man page](http://developer.apple.com/mac/library/documentation/Darwin/Reference/ManPages/man1/dtrace.1.html).  Nevertheless, it shouldn't be too hard to enable these probes on other platforms, too.
