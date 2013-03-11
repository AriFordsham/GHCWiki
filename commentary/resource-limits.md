# Resource Limits


This page describes a proposed resource limits capabilities for GHC. The idea is to give users the ability to create and utilize cost-centres inside programs (so that cost-centres do not necessarily have to be tied to source code locations), and then provide in-program access to heap census and other information. The end result is the ability to impose resource limits on space usage, as well as a side-effect of more precise profiling.

## Front-end changes

```wiki
type CCS
type CC
type Listener

data ProfType = Residence | Allocated

ccsDynamic :: CCS

newCC :: IO CC
pushCC :: CCS -> CC -> IO CCS
setCCS :: CCS -> a -> IO ()
withCCS :: CCS -> IO a -> IO a
listenCCS :: CCS -> ProfType -> Int -> IO () -> IO Listener
unlistenCCS :: Listener -> IO ()
getCCSOf :: a -> IO CCS -- already exists
getCurrentCCS :: IO CCS -- already exists
queryCCS :: CCS -> ProfType -> Int
```


Listeners automatically deregister themselves when they trigger.


The general usage of this API goes like:

```wiki
f n =
    let xs = [1..n::Integer]
    in  sum xs * product xs

newCCS :: IO CCS
newCCS = pushCC ccsDynamic =<< newCC

main = do
  m <- newEmptyMVar
  forkIO $ do
    x <- newCCS
    tid <- myThreadId
    l <- listenCCS x 2000 (putStrLn "Too much memory is being used" >> killThread tid)
    withCCS x $ do
      evaluate (f 20000)
    unlistenCostCentreStack l
    putMVar m ()
  takeMVar m
```


Another use-case is more fine-grained SCCs based on runtime properties, not source-level features.


I am planning on providing semantics, based on GHC’s current profiling semantics. Notice that this current story says nothing about RETAINERS (so there is some careful library writing to be done, to prevent untrusted code from holding onto large system structures for too long).


Some points to bikeshed:

- Naming: CostCentreStack/CostCentre or CCS/CC? Old API used CCS/CC

- Instead of the current interface, we could publish STM variables which are updated by the GC; listening is then just an ordinary STM transaction. This might be tricky to implement.

## Runtime changes

- `Listener` is a new garbage collected object; we expect it can be implemented as a simple `PRIM` using techniques similar to `StgMVarTSOQueue`.
- Checks for listeners occur during heap census; you'll need to pass the `-hc` flag for this machinery to do anything. Actually, we added a new flag `-hl` which is `-hc` but without writing an `.hp` file. See also [\#7751](https://gitlab.haskell.org//ghc/ghc/issues/7751) which will dramatically improve performance. Right now, running heap census is very slow; if we can make censuses incremental their cost can be amortized with ordinary garbage collector behavior.

## Commentary

### Interaction with traditional profiling


Resource limits must be compiled with `-prof`; we’d like to treat profiling as semantics preserving but resource limits are anything but.  In the long term, it is probably desirable to consider these distinctive systems which happen to share a common mechanism. As a first draft, we don’t intend on supporting profiling and resource limits simultaneously; the dynamic SCC machinery can be used for enhanced profiling or for marking resource limits, but not both. It may be possible to extend the resource limit machinery to handle “superfluous” cost-centres, but this would be more complicated and costly, since a costs will now be spattered over many `CostCentre` objects and need to be recombined. Currently, the profiling machinery can perform this calculation, but only calculates inherited resource usage at the very end, so this could be expensive.


Since non-dynamic SCCs can interfere with accurate cost attribution, we add a new flag `-fprof-drop` which drops all `SCC` pragmas.

### Memory leaks


One of the most important intended use-cases of resource limits is when you are rapidly loading and unloading large amounts of untrusted code (think [ http://tryhaskell.org/](http://tryhaskell.org/)). So an important thing to get right is avoiding long term memory leakage, either from leftover objects from the untrusted code or related infrastructure.


On the unloading code front, one technique that could be employed is to replace all third-party closures with “error” frames upon unloading. Similar techniques are already being employed in GHC, and it is semantically sound even if another thread has already witnessed the full value of the data structure: one can imagine some supervisor process sending an asynchronous exception when some unloaded data is accessed. (XXX this may have bad interactions with mask and uninterruptibleMask).


On the cost centre front, the runtime currently assumes that cost centres are permanent and never deallocated. One technique for deallocating a cost-centre goes as follows. We first allocate a distinguished “catch-all” cost-centre which tracks all deallocated cost centres. When we would like to deallocate a cost centre, we mark the cost centre as killed, and upon the next major garbage collection, we look at the cost-centres pointed to by all of the heap objects and rewrite them if they correspond to a killed cost-centre.  We also donate all of the cost-centre’s statistics to the catch-all.  It is also necessary to rewrite any in-Haskell references to the cost-centre (so we need a new infotable to mark these references.)  Once this is done, we’ve removed all references to the cost-centre and it can be dropped.  (This is not quite true; CC_LIST and any cost-centre stacks also have to be updated.)

### Callback triple fault


Finalizer could trigger a new finalizer, ad infinitum. However, if you don't allocate a new finalizer in the callback, you should be fine.

### Discussion


These mailing list threads may be of interest:

- [ http://www.haskell.org/pipermail/ghc-devs/2013-March/000680.html](http://www.haskell.org/pipermail/ghc-devs/2013-March/000680.html) (first announce of this)
- [ http://www.haskell.org/pipermail/glasgow-haskell-users/2012-April/022258.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2012-April/022258.html) (earlier discussion about space resource limits)
