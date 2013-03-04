# Resource Limits


This page describes a proposed resource limits capabilities for GHC. The idea is to give users the ability to create and utilize cost-centres inside programs (so that cost-centres do not necessarily have to be tied to source code locations), and then provide in-program access to heap census and other information. The end result is the ability to impose resource limits (space and time) on fragments of Haskell code, as well as a side-effect of more precise profiling.

## Front-end changes


TBD


There are two choices on how to represent dynamic SCCs at the core level:

- Modify Tick so that it can take an optional argument (cost-centre); modify the type-checker appropriately. This is not so great because we’re making an already ad hoc addition to the Core language even more complicated, even if the extra typing rules are not that complicated.

- Add a new Tickish type, which has no impact on code-generation but still appropriately modifies optimization behavior, and introduce new prim-ops to actually set cost-centers.


Something you might wonder about is whether or not ordinary (source-generated) ticks could also be converted into prim-ops; while this sounds appealing, it gets complicated because true source-level SCCs need to be statically initialized (so the runtime system knows about them and can assign an integer ID to them), and moving them into hard-wired constants would complicate the late-stage STG passes. (It's possible, but probably loses out as far as overall complexity goes.)

## Runtime changes

## Garbage collector changes


One of the killer features of this machinery is the ability to say:

```wiki
    cc <- newCC "<user>"
    let x = scc cc someUserCode
    limitCC cc 100000
    
```

## Commentary

### Support currentCostCentre?


This is obviously fine to support if you are in IO; however, the situation is dicey when considering pure code; an expression `currentCostCentre :: CostCentre` is not referentially transparent. Rather, we want some semantics like implicit parameters, but no one really likes implicit parameters. Maybe it’s better just to note support it (and let someone `unsafePerformIO` if they reaaally  care.)

### Interaction with traditional profiling


Resource limits must be compiled with `-prof`; we’d like to treat profiling as semantics preserving but resource limits are anything but.  In the long term, it is probably desirable to consider these distinctive systems which happen to share a common mechanism. As a first draft, we don’t intend on supporting profiling and resource limits simultaneously; the dynamic SCC machinery can be used for enhanced profiling or for marking resource limits, but not both. It may be possible to extend the resource limit machinery to handle “superfluous” cost-centres, but this would be more complicated and costly, since a costs will now be spattered over many `CostCentre` objects and need to be recombined. Currently, the profiling machinery can perform this calculation, but only calculates inherited resource usage at the very end, so this could be expensive.


Since non-dynamic SCCs can interfere with accurate cost attribution, we add a new flag `-fprof-drop` which drops all `SCC` pragmas.

### Memory leaks


One of the most important intended use-cases of resource limits is when you are rapidly loading and unloading large amounts of untrusted code (think [ http://tryhaskell.org/](http://tryhaskell.org/)). So an important thing to get right is avoiding long term memory leakage, either from leftover objects from the untrusted code or related infrastructure.


On the unloading code front, one technique that could be employed is to replace all third-party closures with “error” frames upon unloading. Similar techniques are already being employed in GHC, and it is semantically sound even if another thread has already witnessed the full value of the data structure: one can imagine some supervisor process sending an asynchronous exception when some unloaded data is accessed. (XXX this may have bad interactions with mask and uninterruptibleMask).


On the cost centre front, the runtime currently assumes that cost centres are permanent and never deallocated. One technique for deallocating a cost-centre goes as follows. We first allocate a distinguished “catch-all” cost-centre which tracks all deallocated cost centres. When we would like to deallocate a cost centre, we mark the cost centre as killed, and upon the next major garbage collection, we look at the cost-centres pointed to by all of the heap objects and rewrite them if they correspond to a killed cost-centre.  We also donate all of the cost-centre’s statistics to the catch-all.  It is also necessary to rewrite any in-Haskell references to the cost-centre (so we need a new infotable to mark these references.)  Once this is done, we’ve removed all references to the cost-centre and it can be dropped.  (This is not quite true; CC_LIST and any cost-centre stacks also have to be updated.)

### Callback triple fault


Finalizer could trigger a new finalizer, ad infinitum.
