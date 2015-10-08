# Status of DWARF work slated for GHC 8.0

## Notation


These are in various states of completion which I'll encode with the
following designations,

<table><tr><th>MERGED</th>
<td>Already merged to `master`, listed here for completeness
</td></tr></table>

<table><tr><th>READY</th>
<td>Believed to be finished with no expectation of major rework
being necessary. Should be in merge-worthy condition, pending
code review.
</td></tr></table>

<table><tr><th>RFC</th>
<td>Code done, builds, and tested to some extent; needs design
review.
</td></tr></table>

<table><tr><th>EXPLOR</th>
<td>Exploratory work, may not even build but included to document
the expected future direction of the work.
</td></tr></table>

<table><tr><th>IDEA</th>
<td>Just an idea, no implementation yet.
</td></tr></table>


I'll list the commits in the order of their logical progression,

## The Patches

### Basic DWARF support


These preparatory commits address a few bugs and deficiencies in the
current DWARF production implementation,

- \[MERGED\]  [ Phab:D1172](https://phabricator.haskell.org/D1172) Dwarf: Fix DW_AT_use_UTF8 attribute
- \[MERGED\]  [ Phab:D1173](https://phabricator.haskell.org/D1173) Dwarf: Produce {low,high}_pc attributes for compilation units
- \[MERGED\]  [ Phab:D1174](https://phabricator.haskell.org/D1174) Dwarf: Produce .dwarf_aranges section
- \[MERGED\]  [ Phab:D1220](https://phabricator.haskell.org/D1220) Dwarf: Ensure block length is encoded correctly


These introduce DWARF parsing and stack unwinding to the RTS by
introducing an optional dependency on `libdw`. This is the same library
used by `perf`.

- \[READY\]   [ Phab:D1196](https://phabricator.haskell.org/D1196): Libdw: Add libdw-based stack unwinding
- \[READY\]   [ Phab:D1197](https://phabricator.haskell.org/D1197): Signals: Print backtrace on SIGUSR2


With the RTS groundwork in place we can plumb things in for use by user
programs,

- \[READY\]   [ Phab:D1198](https://phabricator.haskell.org/D1198): Provide DWARF-based backtraces to Haskell-land


Unfortunately up until this point we have no ability to unwind out of
Haskell code back to the C stack. These commits introduce the ability to
unwind all the way back to `_start`,

- \[READY\]  [ Phab:D1224](https://phabricator.haskell.org/D1224): Dwarf: Preserve stack pointer register
- \[READY\]  [ Phab:D1225](https://phabricator.haskell.org/D1225): cmm: Expose machine's stack pointer and return address registers
- \[READY\]  [ Phab:D1223](https://phabricator.haskell.org/D1223): StgStartup: Add DWARF unwinding annotations for stg_stop_thread


This should be enough to get reasonable backtraces for error-handling
and reporting.

### Statistical Profiling


However, we also want profiling. For this, however, DWARF annotations
alone aren't sufficient. The plan here is to incorporate a more details
from the source notes produced by GHC into the DWARF vendor extension
DIEs.


The RTS then takes these DIEs during program initialiation and
emits a representation of them to the eventlog for later use by analysis
code,

- \[RFC\]    [ Phab:D1279](https://phabricator.haskell.org/D1279): Output source notes in extended DWARF DIEs
- \[RFC\]    [ Phab:D1280](https://phabricator.haskell.org/D1280): rts: Emit debug information about program to event log
- \[READY\]  [ Phab:D1281](https://phabricator.haskell.org/D1281): Support multiple debug output levels


Now we have everything necessary to add some basic statistical
profiling. Here we collect samples from heap checks and black hole block
events and emit them to the event log,

- \[RFC\]    [ Phab:D1215](https://phabricator.haskell.org/D1215): A simple statistical profiler
- \[RFC\]    [ Phab:D1216](https://phabricator.haskell.org/D1216): StatProfile: Heap and black-hole sampling


This all appears to work and I have some rather crude analysis tools
which I should really clean up a bit. Ideally someone would dust off
Peter's Threadscope integration as this would make for an extremely
compelling performance analysis story.


I also have yet to examine the impact of profiling on performance when
not enabled. In principle it should be cheap enough to compile in
unconditionally (although event log support is needed) but this needs to
be measured.


It would also be nice to support time- or cycle-based sampling.

- \[IDEA\]   Support basic timer-based sampled
- \[IDEA\]   Support sampling with Linux `perf_events` interface


In his prototype implementation Peter also had the lovely ability to
preserve simplified Core for later examination. This would be a nice
thing to have but probably won't happen for 8.0. The challenge here is
recording the Core fragments without introducing enormous amounts of
redundancy.

- \[EXPLOR\] [ Phab:D1213](https://phabricator.haskell.org/D1213): Core notes
- \[IDEA\]   Record tree of Core fragments into DWARF DIEs