# GHC status April 2010

## GHC 6.12


In the past 6 months we have made the first 2 releases from the 6.12 branch. 6.12.1 came out in December, while 6.12.2 was released in April. The 6.12.2 release fixes many bugs relative to 6.12.1, including closing 81 trac tickets. For full release notes, and to download it, see the GHC webpage ([http://www.haskell.org/ghc/download_ghc_6_12_2.html](http://www.haskell.org/ghc/download_ghc_6_12_2.html)). We plan to do one more release from this branch before creating a new 6.14 stable branch.


GHC 6.12.2 will also be included in the upcoming Haskell Platform release ([ http://hackage.haskell.org/platform/](http://hackage.haskell.org/platform/)). The Haskell platform is the recommended way for end users to get a Haskell development environment.

## Ongoing work


Meanwhile, in the HEAD, the last 6 months have seen more than 1000 patches pushed from more than a dozen contributors. As the following graph shows, tickets are still being opened faster than we can close them, with the total open tickets growing from around 700 to almost 800. We will be looking in the near future at improving the effectiveness of the way we use the bug tracker.

[](/trac/ghc/attachment/wiki/Status/Apr10/GHC_trac_tickets.png)

- Hoopl and the back end (incl LLVM).  **Simon PJ**.
- Containers: **Simon PJ and Milan**.
- TH quasi-quoting improvements (simpler syntax, types and decls): **Simon PJ**
- Type system generally **Simon PJ**
- Inliner changes (incl CONLIKE) **Simon PJ**
- Data parallel Haskell: **Manuel**
- The [ Threadscope](http://research.microsoft.com/threadscope) tool for visualising parallel execution was released.  The tool is ripe for improvement in many ways, if you're interested in helping let us know!

### Runtime system work (SimonM)


There has been a lot of restructuring in the RTS over the past few months, particularly in the area of parallel execution.  The biggest change is to the way "blackholes" work: these arise when one thread is evaluating a lazy computation (a "thunk"), and another thread or threads demands the value of the same thunk.  Previously, all threads waiting for the result of a thunk were kept in a single global queue, which was traversed regularly.  This lead to two performance problems.  Firstly, traversing the queue is O(n) in the number of blocked threads, and we recently encountered some benchmarks in which this was the bottleneck.  Secondly, there could be a delay between completing a computation and waking up the threads that were blocked waiting for it.  Fortunately, we found a design that solves both of these problems, while adding very little overhead.


We also fixed another pathalogical performance case: when a large numbers of threads are blocked on an MVar and become unreachable at the same time, reaping all these threads was an O(n<sup>2</sup>) operation.  A new representation for the queue of threads blocked on an MVar solved this problem.


At the same time, we rearchitected large parts of the RTS to move from algorithms involving shared data structures and locking to a message-passing style.  As things get more complex in the parallel RTS, using message-passing let us simplify some of the invariants and move towards having less shared state between the CPUs, which will improve scaling in the long run.


The GC has seen some work too: the goal here is to enable each processor ("capability" in the internal terminology) to collect its private heap independently of the other processors.  It turns out that this is quite tricky to achieve in the context of the current architecture, but we have made some progress in this direction by privatising more of the global state and simplifying the GC data structures by removing the concept of "steps", while keeping a simple aging policy which is what steps gave us previously.

### Nightly builds


For some time, it's been clear to us that Buildbot is not the perfect tool for our nightly builds. The main problem is that it is very susceptible to network wibbles, which means that many of our builds fail due to a network issue mid-build. Also, any customisation beyond that anticipated by the configuration options provided requires some messy python coding, poking around inside the buildbot classes. Additionally, we would like to implement a "validate-this" feature, where developers can request that a set of patches is validated on multiple platforms before being pushed. We couldn't see an easy way to do this with buildbot.


When the darcs.haskell.org hardware was upgraded, rather than installing buildbot on the new machine, we made the decision to implement a system that better matched our needs instead. The core implementation is now complete, and we have several machines using it for nightly builds.


We're always keen to add more build slaves; please see [ http://hackage.haskell.org/trac/ghc/wiki/Builder](http://hackage.haskell.org/trac/ghc/wiki/Builder) if you're interested. Likewise, patches for missing features are welcome! The (Haskell) code is available at [ http://darcs.haskell.org/builder/](http://darcs.haskell.org/builder/)