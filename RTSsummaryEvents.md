This used to live at <http://trac.haskell.org/ThreadScope/wiki/RTSsummaryEvents>.


# Detailed design rationale for the changes in GHC for emulating +RTS -s through eventlog events

The following subsections are titled after the GHC commits they describe. The
remaining sections contain problem analysis, design discussion and drafts and
are retained to show the motivation and the history of the changes.

## Calculate the total memory allocated on a per-capability basis

In addition to the existing global method, we calculate the total memory
allocated on a per-capability basis. For now we do it both ways and assert they
give the same grand total. At some stage we can simplify the global method to
just take the sum of the per-cap counters.

## Change the presentation of parallel GC work balance in +RTS -s

We change the presentation of parallel GC work balance in +RTS -s and also we
rename internal variables to make the names match what they hold. The parallel
GC work balance is calculated using the total amount of memory copied by all GC
threads, and the maximum copied by any individual thread. You have serial GC
when the max is the same as copied, and perfectly balanced GC when
`total/max == n_caps`.

Previously we presented this as the ratio total/max and told users that the
serial value was 1 and the ideal value N, for N caps, e.g.
```
  Parallel GC work balance: 1.05 (4045071 / 3846774, ideal 2)
```
The downside of this is that the user always has to keep in mind the number of
cores being used. Our new presentation uses a normalised scale 0--1 as a
percentage. The 0% means completely serial and 100% is perfect balance, e.g.
```
  Parallel GC work balance: 4.56% (serial 0%, perfect 100%)
```
Some more examples of the new form from `nofib/parallel` on a 2-core machine with `-N2`:
```
ray:  Parallel GC work balance: 13.81% (serial 0%, perfect 100%)
gray:  Parallel GC work balance: 28.25% (serial 0%, perfect 100%)
prsa:  Parallel GC work balance: 29.16% (serial 0%, perfect 100%)
coins:  Parallel GC work balance: 62.43% (serial 0%, perfect 100%)
blackscholes:  Parallel GC work balance: 15.25% (serial 0%, perfect 100%)
minimax:  Parallel GC work balance: 79.86% (serial 0%, perfect 100%)
nbody:  Parallel GC work balance: 52.39% (serial 0%, perfect 100%)
```
and a couple examples from other parts of nofib:
```
nofib/gc/circsim:  Parallel GC work balance: 22.35% (serial 0%, perfect 100%)
nofib/gc/fibheaps:  Parallel GC work balance: 7.07% (serial 0%, perfect 100%)
nofib/spectral/clausify:  Parallel GC work balance: 3.48% (serial 0%, perfect 100%)
nofib/real/cacheprof:  Parallel GC work balance: 0.90% (serial 0%, perfect 100%)
```

## Add new eventlog events for various heap and GC statistics

They cover much the same info as is available via the `GHC.Stats` module or via
the `+RTS -s` textual output, but via the eventlog and with a better sampling
frequency.

We have three new generic heap info events and two very GHC-specific ones. (The
hope is the general ones are usable by other implementations that use the
same eventlog system, or indeed not so sensitive to changes in GHC itself.)

The general ones are:

* total heap mem allocated since prog start, on a per-HEC basis
* current size of the heap (MBlocks reserved from OS for the heap)
* current size of live data in the heap

Currently these are all emitted by GHC at GC time (live data only at major GC).

The GHC specific ones are:

* an event giving various static heap paramaters:
  * number of generations (usually 2)
  * max size if any
  * nursary size
  * MBlock and block sizes
* a event emitted on each GC containing:
  * GC generation (usually just 0,1)
  * total bytes copied
  * bytes lost to heap slop and fragmentation
  * the number of threads in the parallel GC (1 for serial)
  * the maximum number of bytes copied by any par GC thread
  * the total number of bytes copied by all par GC threads (these last three can be used to calculate an estimate of the work balance in parallel GCs)

## Adjust the eventlog description header for the spark counter event

The `EventLogFormat.h` described the spark counter fields in a different order
to that which ghc emits (the GC'd and fizzled fields were reversed). At this
stage it is easier to fix the `ghc-events` lib and to have ghc continue to emit
them in the current order.

## Move trace of cap delete from `shutdownCapability` to `freeCapability`

Will let us do final per-cap trace events from stat_exit(). Otherwise we would
end up with eventlogs with events for caps that have already been deleted.

## Emit final heap alloc events and rearrange code to calculate alloc totals

In `stat_exit` we want to emit a final `EVENT_HEAP_ALLOCATED` for each cap so
that we get the same total allocation count as reported via `+RTS -s`. To do so
we need to update the per-cap `total_allocated` counts.

Previously we had a single `calcAllocated(rtsBool)` function that counted the
large allocations and optionally the nurseries for all caps. The GC would
always call it with false, and the stat_exit always with true. The reason for
these two modes is that the GC counts the nurseries via clearNurseries() (which
also updates the per-cap total_allocated counts), so it's only the
`stat_exit()` path that needs to count them.

We now split the `calcAllocated()` function into two: `countLargeAllocated` and
`updateNurseriesStats`. As the name suggests, the latter now updates the
per-cap `total_allocated` counts, in additon to returning a total.

## Fix the timestamps in `GC_START` and `GC_END` events on the GC-initiating cap

There was a discrepancy between GC times reported in `+RTS -s` and the
timestamps of `GC_START` and `GC_END` events on the cap, on which +RTS -s stats
for the given GC are based. This is fixed by posting the events with exactly
the same timestamp as generated for the stat calculation. The calls posting the
events are moved too, so that the events are emitted close to the time instant
they claim to be emitted at. The `GC_STATS_GHC` was moved, too, ensuring it's
emitted before the moved `GC_END` on all caps, which simplifies tools code.

## Add the `GC_GLOBAL_SYNC` event marking that all caps are stopped for GC

The event indicates that we're doing a stop-the-world GC and all other HECs
should be between their `GC_START` and `GC_END` events at that moment. We don't
want to use `GC_STATS_GHC` for that, because `GC_STATS_GHC` is for extra
GHC-specific info, not something we have to rely on to be able to match the GC
pauses across HECs to a particular global GC.

# The analysis of the semantics of +RTS -s

Here is a sample output of `+RTS -s` that was used in the early design phases
for the corresponding events. The output is annotated with a discussion of new
events required to simulate it in ThreadScope? (for a user-selected time
interval). A list of the new required events is in the second part of this
page.
[Here](https://raw.githubusercontent.com/Mikolaj/ThreadScope/working/SummaryPanelMockup.png)
is a screenshot of what we can already do using the current set of
events. It so happens we can do as much for the whole runtime as for the
selected time intervals with the currently available events, but in general,
intervals require more kinds of events and more samples. Similarly,
when we visualize some of this as graphs and especially graphs of
rates of change of some values (e.g., memory usage), more frequent
sampling will be required.

The first line of +RTS -s follows.

     237,179,528 bytes allocated in the heap

We'd need an extra event, emitted at each GC, with the allocation since the
previous GC. (We really don't want an event for every memory allocation, that
would be impractical and very slow.)

      52,785,584 bytes copied during GC

An event with a summary of all copying done, emitted after the end of each GC.

      17,272,336 bytes maximum residency (5 sample(s))

A separate event for that, perhaps emitted only after major GC when we know how
much memory is really used by the program. The docs explain the "n samples"
above saying "only checked during major garbage collections".

       6,493,328 bytes maximum slop

We also need an extra event for slop, probably emitted rarely.

              45 MB total memory in use

The peak "total memory in use" to date is stored in the peak_mblocks_allocated
global var. It changes often, but we can't spam too much, so let's emit it only
after each GC, and not the peak value to date, but the current value.

                  (0 MB lost due to fragmentation)

Fragmentation is calculated in the RTS -s code as follows:

     (peak_mblocks_allocated * BLOCKS_PER_MBLOCK * BLOCK_SIZE_W - hw_alloc_blocks * BLOCK_SIZE_W) / (1024 * 1024 / sizeof(W_))

Note that it uses the peak and high-water values and we instead want the
current values. It's calculated as the difference of mblock and block
allocations, so we need an extra events for allocated block (mblocks are
already recorded in "total memory in use" above).
```
                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0       448 colls,     0 par    0.04s    0.04s     0.0001s    0.0016s
  Gen  1         5 colls,     0 par    0.07s    0.07s     0.0149s    0.0386s
```
The current GC events (in particular `RequestParGC`) seem to be enough to
distinguish between seq and par GC. We'd need to split the current GC events
into generations, though, to report for every generation separately. We may and
up with two tables for the same GC info: one aggregated by cap, another by
generations. Or, as long as there are only 2 generations, one table with both
caps and generations, with the following rows: cap0&gen0, cap0&gen1, cap1&gen0,
etc. Note that we don't want to report the CPU time, only the elapsed
time, and that's fine.

    Parallel GC work balance: 1.00 (6391526 / 6375794, ideal 2)

Let's ignore that one for now. JaffaCake says we probably don't care about
work balance and that he thinks it is computed in the simplest way. Detail are
in http://community.haskell.org/~simonmar/papers/parallel-gc.pdf. The current
computation seems to be the following: total words copied during parallel GCs
divided by the sum over all parallel GCs of the maximal number of words copied
by any thread in a given GC. Events needed: the events added above probably
suffice, but quite a bit of extra state will have to be maintained when reading
the events due to the rather complex formula for work balance.
```
                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    0.04s    (  0.38s)       0.13s    (  0.17s)
  Task  1 (worker) :    0.00s    (  0.55s)       0.00s    (  0.00s)
  Task  2 (bound)  :    0.41s    (  0.41s)       0.13s    (  0.13s)
  Task  3 (worker) :    0.00s    (  0.55s)       0.00s    (  0.00s)
```
JaffaCake says the task information has questionable usefulness, so let's
ignore that one for now. It's much more natural for us
to present the same info per cap, not per OS thread
(which the tasks basically are). Actually we do present
the GC info per cap (not only total, as in +RTS -s)
already and the total activity time per cap (which
includes the mutator time) is much better conveyed
by the graphs in ThreadScope?.

BTW, the time between events GCIdle and GCWork is still counted as GC time, so
we may ignore the events for calculating the times spent on GC. OTOH, a summary
of the GCIdle times, per hec, then the total, also as the percentage of all GC
time could be useful. Probably we can do that cheaply along the way since we
have to identify and sift out the GCIdle, GCDone and GCWork events anyway.

    SPARKS: 300001 (17739 converted, 282262 overflowed, 0 dud, 0 GC'd, 0 fizzled)

Tell JaffaCake that the example and description for the SPARKS count
[here](http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html#rts-options-gc).

needs updating (not sure for which GHC version, though). Otherwise, we have enough events for that (we calculate this using the SparkCounters? events, but we could also use the precise per-spark events).
```
  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.42s  (  0.41s elapsed)
  GC      time    0.29s  (  0.13s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.71s  (  0.55s elapsed)
```

(Note that there may be more times listed above, e.g., the time overhead of profiling.) We can sum up the GC time from GC events. We get the total of GC and MUT time (and PROF, etc.) as the time from the start of the first event to the (end of) the last event, so from the total and the GC time we can normally compute MUT. We can assume that INIT and EXIT are negligible (I wonder when they are not) and anyway they don't make sense for when we summarize an interval. If we insist on them, a separate event for each would be required.
```
%GC time      89.5%  (75.3% elapsed)
```
This line does not appear in threaded RTS, so we disregard it.
```
  Alloc rate    564,074,971 bytes per MUT second
```
The events added above should suffice, except that we use elapsed time, not CPU time.
```
  Productivity  59.0% of total user, 76.6% of total elapsed
```
The events added above should be enough. Again. we only do the elapsed case, so
we'd show elapsed/elapsed, while the figures above are cpu/cpu and cpu/elapsed.
JaffaCake thinks the latter mixture is OK. However, it mixes the productivity
of CPU mutation vs elapsed mutation with the productivity of mutation vs GC. In
this light, out figure will not be that bad, because it's consistent, even if
not as accurate as the equally consistent first figure above.

BTW, the fact that the second figure is higher (I can reproduce it), shows a
problem with clocks or some other problem. I'd guess the elapsed time should
always be higher than the CPU time, shouldn't it?

# The list of considered new events (the actual implementation closely follows this draft, even though not all details match)

There is a wealth of statistics around heaps and GC. Some of the stats are
reasonably common, shared by different implementations while many more are
highly specific to a particular implementation. Even if we ignore RTSs other
than GHC, we still have an issue of flexibility for future changes in GHC's
RTS.

Our solution is to split the stats into two groups, a general group that make
sense in many implementations, and a second that are highly GHC-specific.
Analyses and visualisations based on the first group are likely to be portable
to other RTS instances and changes in GHC's RTS. The second is likely to have
to change when GHC changes, but it does at least contain the less frequently
used info and does not need so much visualisation.

## New general memory stats events

 * `EVENT_HEAP_ALLOCATED (alloc_bytes)`: is the total bytes allocated over the whole run by this HEC. That is we count allocation on a per-HEC basis. This event is in principle not tied to GC, it could be emitted any time. 

 * `EVENT_HEAP_SIZE (heap_capset, size_bytes)`: is the current bytes allocated from the OS to use for the heap. For the current GHC RTS this is the MBlocks, kept in the mblocks_allocated var. Again, this in principle could be emitted any time. The maximum accuracy would be to emit the event exactly when MBlocks are allocated or freed. 

 * `EVENT_HEAP_LIVE (heap_capset, live_bytes)`: is the current amount of live/reachable data in the heap. This is almost certainly only known after a major GC. 

## New GHC-specific general memory stats events

 * `EVENT_HEAP_INFO_GHC (heap_capset, gens, max_size, nursary_size)`: various static parameters relating to the heap. In particular it tells us the number of generations. 

 * `EVENT_GC_STATS_GHC (heap_capset, gen, copied, slop, frag, was_par, max_copied, avg_copied)`: various less used GC stats. These are all GHC-specific, and specific to current GC design. It includes the generation of this GC so along with knowing the total number of generations we can identify major/minor GCs. We also include memory lost to slop and fragmentation. The final three are to do with parallel GC: the first is just a flag to indicate if this GC was a parallel GC, the ratio of the other two gives the parallel work balance (this ratio can be extended to multiple GCs to get an average figure). 

## Identifying heaps in eventlogs

In the above events, the "allocated since program start" is done per-HEC, but
the others apply to the heap as a whole, not a particular HEC.

For completeness explicitly identify heaps by identifying the heap to which the
events apply. (Remember that we can merge event logs from multiple processes,
so there is already no truly global notion of heap, implicitly it would be
the single heap belonging to the HEC that emits the event. We would also
have to make the assumption that there is a single heap per OS process (we
can already identify which HECs belong to the same OS process).
Alternatively we can explicitly identify heaps using the existing capset
(capability set) mechanism.)

The GHC RTS already puts all of its HECs into a capset for the OS process. We can reuse this capset.

If in future we allow multiple independent heaps in the same OS process (e.g. separate RTS instances) then this scheme would let us cope by making a separate capset. Similarly it'd cope with implementations like GdH which use a global heap spanning multiple OS processes.

# Problems with the order of events in the eventlog

In the implemented version, we sidestep the issues with event order by tightly
controlling the timestamps in the `GC_START` and `GC_END` events on the main GC cap
(the one that requests GC) and by adding the `GC_GLOBAL_SYNC` event. We also
treat the elapsed time delays needed to synchronise caps for parallel GC (both
at GC start and GC end) as MUT time, instead of GC time, because while they
are needed for GC, their cause is (slow) OS scheduling, just as for any other
delays when switching threads. In the subsection there are detailed discussions
of the original GC event order problems and about the initially considered
design choices.

## Summary of the problems

The main issue is how we measure the time spent doing GC. The `+RTS -s`
measurement is taken from the `stat_startGC()` to `stat_endGC()` on the
"primary" GC thread. In ThreadScope? the main GC events we have access to are
`GC_START` and `GC_END` on each capability.

It turns out that these different measurements are often quite different. In
particular there can often be a significant delay between when `+RTS -s` reports
and when the other capabilities start the mutator again (and hence emit
`GC_END`). Our guess is that this is because the other caps are suspended at
the end of the GC while the primary GC thread does its stats etc, then when it
releases them it can take a while for the other caps to be rescheduled by the
OS.

In this example, `END_GC` on one of the caps comes very late:
```
9378252000: cap 1: GC stats for heap capset 0: generation 0, 45160
bytes copied, 58296 bytes slop, 1757184 bytes fragmentation, 2 par
threads, 45080 bytes max par copied, 45112 bytes total par copied
9378253000: cap 1: finished GC
9378254000: cap 1: spark stats: 1913071 created, 1229807 converted,
221 remaining (0 overflowed, 0 dud, 58923 GC'd, 144981 fizzled)
9378255000: cap 1: waking up thread 3 on cap 1
9378257000: cap 1: running thread 2885
9378259000: cap 1: stopping thread 2885 (thread finished)
9378268000: cap 1: running thread 3
9378354000: cap 1: stopping thread 3 (blocked on black hole owned by
thread 2884)
9378355000: cap 1: creating thread 2886
9378363000: cap 0: finished GC
```
From the user's point of view it is perhaps more accurate to look at the `GC_START`/`GC_END` because that's when the caps really get back to the mutator.

This different GC vs mutator measurement affects several other statistics and the difference vs +RTS -s can build up to be quite large on larger eventlogs.

Additionally, if we only look at the `GC_START`/`GC_END` then it's not entirely
clear what the average or maximum pause time should mean, since we have
multiple capabilities. Is the max the max for any individual cap, or from when
the first cap does `GC_START` 'til when the last cap does `GC_END`.

Which brings up another issue: some of the GC statistics rely on a GC being
global across all capabilities, but strictly speaking the current GC events do
not let us associate a `GC_START`/`GC_END` on one cap with that on another. One core
can issue a new `REQUEST_SEQ_GC` before another has issued it's GC_END from the
previous GC. Add in the fact that not all cores have to take part in a GC and
it's tricky to associate GCs on each core to a global GC.

We have added a `GC_GLOBAL_SYNC` event which is emitted strictly between
`GC_START`/`GC_END` on all cores, used for associating GC across all cores. If
we could calculate stats in such a way that we do not need to associate GC
across all cores then that would be better (and more flexible e.g. to work with
your local GC branch).

## An implemented partial solution for RequestParGC

We can try to ensure that RequestSeqGC and RequestParGC are emitted before
their corresponding GC_START events, by using the following tip from
JaffaCake?: "actually you could try moving the interruptAllCapabilities() call
in Schedule.c:1500 down below the traceEvent calls". However, even with this
implemented in GHC HEAD, the events can still be emitted in arbitrary order and
it complicates analysis, especially for selected intervals (and complicates and
makes less strict the future validation of events).

Here are the results of implementing it:
```
18:13 < mikolaj> dcoutts, JaffaCake: I've tested the "moving the interruptAllCapabilities() 
call below the traceEvent calls" tweak and it helps in 99-ish % of cases
18:13 < mikolaj> that is, the order of GC events is much, much better now
18:14 < mikolaj> but there are still very rare cases, especially when the par GC request was 
from one HEC, and another, busy HEC is too slow to report finishing a previous GC
18:15 < mikolaj> so it does not help that much in validating eventlogs, but makes them 
so much more readable, if a user wants to inspect the event messages one by one
```

## More example eventlog fragments

The example of the event order issues below are on a system with 2 caps, on an eventlog with lots of small GCs. They show how GC times measured by "+RTS -s", that is from START_GC to the "GC stats" event, are different from TS times. The TS times are the average over all caps of the pause between START_GC, when the caps stop mutating, and END_GC, when they resume mutating. (Most of) the difference stems probably from the thread sleep/wake delays, some ending GC bookkeeping (including the time taken to emit GC and other events) and related overheads.

It seems the TS times are more useful to the programmer that is mostly concerned in how much time is spend mutating and not how the wasted time is divided between the actual GC and the cap switching overhead. Moreover "+RTS -s" just adds the cap switching overhead to the MUT time hiding the fact that it's caused by GC.

The differences accumulate and affect the "GC total elapsed time", "Avg pause" and "Max pause" columns of the GC table in "+RTS -s". They also influence the "Task" table (not reproduced in TS). They add up in total "GC time (elapsed)" and consequently in "MUT time (elapsed)" that is obtained by subtracting GC time from total time. In the result, they influence "Alloc rate" and "Productivity", too. The figures are seriously different between TS and "+RTS -s", except on small eventlogs.

It seems for all these figures the TS version is the right one. In particular, if by "pause" we mean the time between GC_START and GC_END, by "Max pause" we neither mean the "maximum over all GCs of (minimal pause over all caps)", similarly as in +RTS -s (but not exactly the same, because even the minimal GC_END is usually a tick after "GC stats"), nor "maximum over all GCs of (maximal pause over all caps)", which we could easily show in TS and which could make sense for a real-time system with specialized, non-interchangeable caps, but "maximum over all GCs of (average pause over all caps)".

Here's an example demonstrating `END_GC` for the previous GC following `REQUEST_PAR_GC` for the next GC on another cap. Note that the early `END_GC` on cap 1 still comes 1000 nanoseconds after the "GC stats" event. Even such small differences accumulate over all GCs:
```
9366536000: cap 1: GC stats for heap capset 0: generation 0, 77552
bytes copied, 226056 bytes slop, 696320 bytes fragmentation, 2 par
threads, 71736 bytes max par copied, 77520 bytes total par copied
9366537000: cap 1: finished GC
9366538000: cap 1: spark stats: 1910358 created, 1228256 converted,
1000 remaining (0 overflowed, 0 dud, 58306 GC'd, 144981 fizzled)
9366541000: cap 1: running thread 3
9367310000: cap 1: stopping thread 3 (heap overflow)
9367312000: cap 1: requesting parallel GC
9367316000: cap 0: finished GC
9367317000: cap 0: spark stats: 1071418 created, 1292489 converted, 0
remaining (0 overflowed, 0 dud, 41840 GC'd, 214904 fizzled)
9367318000: cap 0: starting GC
9368131000: cap 1: starting GC
```
Another example, this time there's a different event ("allocated on heap capset") on the late cap, at the same time as "GC stats" an another cap and before END_GC finally arrives. Not sure how this can happen. The `END_GC` on the early cap comes here a bit later that usually (2000 nanoseconds after the "GC stats" event), too:

```
836505000: cap 0: GC stats for heap capset 0: generation 0, 11584
bytes copied, 205232 bytes slop, 1007616 bytes fragmentation, 2 par
threads, 11472 bytes max par copied, 11536 bytes total par copied
836505000: cap 1: allocated on heap capset 0: 180717856 total bytes till now
836507000: cap 0: finished GC
836507000: cap 0: spark stats: 5893 created, 114894 converted, 228
remaining (0 overflowed, 0 dud, 13 GC'd, 22190 fizzled)
836509000: cap 0: waking up thread 3 on cap 0
836511000: cap 0: running thread 233
836514000: cap 0: stopping thread 233 (thread finished)
836521000: cap 1: finished GC
```
Finally an example where END_GC is quite late on both caps (3000 nanoseconds and 6000 nanoseconds):
```
67582000: cap 1: GC stats for heap capset 0: generation 1, 58248 bytes
copied, 40096 bytes slop, 1912832 bytes fragmentation, 2 par threads,
51920 bytes max par copied, 58208 bytes total par copied
67583000: cap 1: live data in heap capset 0: 82784 bytes
67585000: cap 1: finished GC
67585000: cap 1: spark stats: 15101 created, 2570 converted, 508
remaining (0 overflowed, 0 dud, 879 GC'd, 12 fizzled)
67588000: cap 0: finished GC
```
Discussions that led to the implemented `GC_START/GC_END/GC_GLOBAL_SYNC` solution

Here are two IRC logs with the discussion that led to the `GC_START/GC_END/GC_GLOBAL_SYNC` solution. There are a few more long IRC logs related to that, most of them on `#ghc`.

```
17:19 < dcoutts> and yes, mikolaj is right, I did mean to ask if it wouldn't make sense to include sequential GCs when calculating the parallel GC work balance
17:19 < dcoutts> I understand from your pov as implementer that you don't care, but doesn't it make sense from the user's pov
17:20 < dcoutts> that is, over a whole prog run, we sum up the total bytes copied and the max bytes copied by any individual GC thread
17:20 < dcoutts> +RTS -s only includes parallel GCs for this
17:24 < mikolaj_> yeah, more generally, the caps can be added and deleted during runtime, so seq GCs are just a special case, when only one cap runs at all, or only one works in this GC
17:25 < mikolaj_> which leads to a related quesion, which number of caps take as the divisor in "Parallel GC work balance"
17:27 < dcoutts> mikolaj_: yeah I was thinking about that. If we do want to take into account changing numbers of caps then we might want to divide by the active number of caps at each point, and combine the results
17:32 < JaffaCake> right, I'm not sure what to do about the "GC_END lag"
17:33 < JaffaCake> if the thread is descheduled at the end of GC, is it still in the GC or not?
17:33 < dcoutts> JaffaCake: from the user's pov I'd say yes
17:33 < dcoutts> from the GC implementer's pov, perhaps not
17:33 < JaffaCake> but if you have some threads in GC and some not, it makes no sense to ask what the wall-clock time spent in GC is
17:33 < dcoutts> that's true
17:34 < JaffaCake> this is the problem I had in the local GC, it doesn't measure wall-clock time spent in local GC
17:34 < dcoutts> well, unless you take the sum over all hecs
17:34 < dcoutts> right, what will wall clock time mean for local per-hec GC
17:34 < JaffaCake> (and BTW, CPU time in parallel GC is next to useless, because a lot of cycles are spent spinning)
17:35 < dcoutts> JaffaCake: that's also why I'd ideally like to calculate these things without having to associate GC events across all HECs
17:35 < dcoutts> since that would no longer make sense for local per-HEC GC
17:36 < JaffaCake> so just use the single GC_STATS event?
17:36 < mikolaj_> neither will maxPause make sense for per-HEC GC
17:36 < JaffaCake> right
17:36 < dcoutts> mikolaj: unless it's the max pause on any HEC
17:37 < dcoutts> rather than trying to measure for each GC, time between first HEC starting GC and last finishing
17:37 < JaffaCake> hmm, that might be useful
17:37 < dcoutts> "for any HEC, what was the longest time it paused"
17:38 < JaffaCake> I think that might give artificially long pauses
17:38 < dcoutts> why do you say it's artificial?
17:38 < dcoutts> because you're blaming the OS? :-)
17:39 < JaffaCake> if a HEC has nothing to do, then if it gets delayed a lot after GC that's not really a pause
17:39 < JaffaCake> and yes, there's a question about whether OS-induced pauses count
17:40 < JaffaCake> for me, I'd rather they weren't included
17:40 < dcoutts> JaffaCake: hmm, you mean it can block for a long time, not emit a GC_END and then block due to being idle?
17:40 < JaffaCake> yes
17:40 < JaffaCake> but, I'm not sure why HECs would get descheduled at this point, unless it's just unlucky
17:41 < JaffaCake> because we use spinlocks rather than mutexes
17:41 < dcoutts> JaffaCake: for the "end of GC" sync point you mean?
17:41 < JaffaCake> I suppose if there's other processes and we yield()
17:41 < JaffaCake> yes
17:42 < JaffaCake> so is there a problem with just taking the stats from the GC_STATS event?
17:42 < JaffaCake> then you'll match what GHC does
17:42 < dcoutts> JaffaCake: well two things, one is if it's really measuring what we want
17:43 < dcoutts> and the other is that it is quite ghc specific, it'd be nicer to calculate these things using more general events
17:43 < JaffaCake> why might it not be?
17:44 < dcoutts> JaffaCake: because from the user's pov, the GC vs mutator ratio is really mutator vs other, they don't care so much who's fault it is GC or OS
17:44 < JaffaCake> the problem with trying to be more general is you have to cope with GC that might be concurrent, and then various things don't make sense (e.g. wall-clock time)
17:44 < dcoutts> right yes, I'd like to distinguish which stats make sense generally
17:45 < dcoutts> and which can only make sense for stop-the-world
17:46 < JaffaCake> but if you have mutator overlapping with GC, then you can't ask how much wall-clock time was spent in either
17:46 < JaffaCake> I suppose you could give an average...
17:46 < dcoutts> right, only elapsed time * ncpus
17:47 < JaffaCake> but I don't think this is the most important issue, right?
17:47 < JaffaCake> we don't have any other compilers or GC architectures on the radar
17:47 < JaffaCake> if and when we do, then we could figure out what to do
17:47 < JaffaCake> it's hard to generalise from one example
17:48 < dcoutts> JaffaCake: true, we can just try and match exactly what +RTS -s does, and use the GC_STATS_GHC event
17:48 < JaffaCake> that would be fine by me
17:48 < dcoutts> I'm not convinced it's the "right thing" though it's the expedient thing
17:49 < JaffaCake> I agree, but sometimes worse is better :)
17:49 < dcoutts> mikolaj_: suppose we try to match exactly what +RTS -s gets, do we have any other remaining problems?
17:49 < dcoutts> we can match GCs across all HECs by using the GC_STATS event
17:49 < mikolaj_> "Parallel GC work balance"
17:49 < JaffaCake> is there a problem with that?
17:50 < mikolaj_> dcoutts: yes, we can and we'd need to recod the GC time in GC_STATS, instead of using GC_START and GC_END
17:50 < dcoutts> mikolaj_: so we can get the same number as +RTS -s if we filter out the samples where n_gc_threads = 1
17:50 < mikolaj_> recod/record
17:50 < dcoutts> mikolaj: ah yes, we would
17:50 < mikolaj_> dcoutts: oh, yes, if that's the general rule that we just stick to RTS -s then yes
17:51 < dcoutts> JaffaCake: right we'd have to ignore GC_START/END completely really
17:51 < dcoutts> and put GC elapsed time into the GC_STATS event
17:52 < dcoutts> since even if we look at the GC_START/END for the "primary" GC thread that emits GC_STATS, then the times are different than that HEC's GC_START/END
17:52 < JaffaCake> you could use GC_START/GC_END for just one HEC - the one that emitted the GC_STATS event (but that seems a bit horrible)
17:52 < dcoutts> JaffaCake: right, except it will give us different numbers
17:52 < JaffaCake> oh, does it?
17:53 < JaffaCake> because the events are emitted a little earlier or later than the timestamps we use for stats?
17:53 < dcoutts> because the event and the stats_startGC()/stats_endGC() happen at different points in the code from traceGcBegin()/End()
17:53 < JaffaCake> if it really mattered we could make them the same
17:53 < dcoutts> JaffaCake: right
17:53 < mikolaj_> dcoutts: with some luck, we could draw the time recorded in GC_STATS, starting from GC_START and show the remaining area, till GC_END as an extra shceduling cost, outlines in different colour
17:54 < JaffaCake> i.e. grab a timestamp and pass it to both stat_startGC() and traceGcBegin()
17:54 < mikolaj_> so the duplication would not be entirely wasted
17:54 < dcoutts> JaffaCake: mm
17:54 < mikolaj_> but it depends on GC_START being mostly syncronized across HECS, which it almost is, I think
17:54 < mikolaj_> it's GC_END that's out of sync a lot
17:55 < JaffaCake> I really do want to see the ragged GC_STARTs, because it tells me how long it takes to synchronise all the HECs
17:55 < dcoutts> mikolaj_: it's only out of sync on the HECs other than the primary one
17:56 < mikolaj_> JaffaCake: oh, so they are not in sync? I guess I didn't look close enough
17:56 < JaffaCake> funny if GC_END is more out of sync than GC_START, I don't remember seeing that before
17:56 < JaffaCake> mikolaj_: they won't occur at exactly the same time, no
17:57 < mikolaj_> JaffaCake: I may be wrong, or it may only be so in rare cases
17:57 < dcoutts> JaffaCake: this is the thing mikolaj_ was seeing, that GC_END can be quite significantly delayed on the "other" HECs
17:57 < JaffaCake> because we really do have to send a signal to all the cores
17:57 < JaffaCake> dcoutts__: so that could be something we should look into
17:57 < dcoutts> JaffaCake: I'd assumed that this was because we were using a mutex to hold the other HECs, but you say it's a spinlock so I cannot explain it.
17:58 < mikolaj_> anyway, ignore me, I didn't look at GC_START at all, just thinking aloud how we can use in TS the extra data from dupliated GC times
17:58 < JaffaCake> is it occurring when you're using all the cores?  the last core problem?
17:58 < dcoutts> mikolaj_: how many cores do you have?
17:58 < mikolaj_> JaffaCake: quite likely
17:58 < mikolaj_> 2
17:58 < JaffaCake> aaah
17:58 < mikolaj_> but the logs shows different scenarios
17:59 < JaffaCake> all kinds of bad things happen when one thread gets descheduled
17:59 < dcoutts> JaffaCake: so in this sense we already have mutator and GC overlapping
17:59 < mikolaj_> it's just a simple lag
17:59 < mikolaj_> *not
18:00 < JaffaCake> dcoutts: I don't think of it like that - there is a handshake at the end of GC, so the primary thread can only continue when it has received the ok from all the other HECs
18:00 < JaffaCake> so if one of the other HECs goes for a walk for a while after that, it is in a "limbo" state, not still doing GC
18:00 < dcoutts> right
18:01 < dcoutts> again it's just from the user pov, it's doing something other than running the mutator
18:01  * JaffaCake is out of time, must dash
18:01 < dcoutts> ok
18:01 < mikolaj_> thanks
18:01 < dcoutts> JaffaCake: yes, thanks, we'll think about what to do
18:01 < JaffaCake> dcoutts: in that case you would want to count all "descheduled" time as non-mutator
18:02 < JaffaCake> but we don't have a way to measure that yet
18:02 < dcoutts> aye
18:03 < dcoutts> mikolaj_: so perhaps we should make the GC_START/END times on the primary GC thread match the times emitted for +RTS -s
18:03 < dcoutts> mikolaj_: rather than duplicating the info in the GC_STATS
18:03 < mikolaj_> dcoutts: works for me (probably)
```

Below is the second log.

```
12:38 < dcoutts> JaffaCake: we didn't plan on adding a per-HEC GC stat event, do you think that would be useful? what info would we get?
12:39 < JaffaCake> CPU time in GC on that thread
12:39 < JaffaCake> not particularly useful, but it could be done
12:39 < dcoutts> as opposed to wall clock time between GC_START/END
12:39 < JaffaCake> right - you could get wall clock time too, but that's even less useful I think
12:39 < dcoutts> we get wall clock time already from GC_START/END
12:40 < JaffaCake> right
12:40 < mikolaj_> but it's mixed GC + scheduling overhead time
12:40 < dcoutts> right
12:40 < dcoutts> like we were discussing yesterday
12:43 < JaffaCake> GC time does include scheduling overhead already
12:43 < JaffaCake> it includes the time taken to sync with all the other HECs
12:47 < mikolaj_> right, so only the amount of post-GC scheduling overhead atrributed to GC varies (between RTS stats and GC_START/END)
12:48 < JaffaCake> I suppose so, but it's just random noise introduced by the OS
12:49 < JaffaCake> OS context switches can strike at any time
12:49 < JaffaCake> so if you take two timestamps in succession, there's a random chance that there will be a big delay between them
12:50 < JaffaCake> the discrepancy you see with GC_END is nothing more than that, I think
12:52  * mikolaj_ has a look at the log samples again
13:01 < mikolaj_> yes, quite probably so, plus perhaps a tiny error from the very act of emitting events, etc.
13:02 < mikolaj_> anyway, for the global GC stats, we just want to make sure we measure the same amount of OS noise between GC_START/END as RTS stats do, at least on the main GC HEC
13:04 < mikolaj_> that's why I asked about moving GC_END, but perhaps wait until dcoutts write the paper and you finish overhauling stat gathering
```
