# Hoopl cleanup

*This page was created in August 2013 as a temporary place to store proposals about cleaning up Hoopl library. After these changes are implemented we should replace this page with either another page or a Note in the source code that would explain current design of Hoopl.*


Me (Jan Stolarek, JS) and Simon PJ recently had some discussion about cleaning up Hoopl to make its interface more consistent and less confusing. Here are some of the key proposals and ideas:

## The API for forward and backward analysis

### Observations about forward analysis

- Forwards analysis starts with a list of entry labels (typically just one), and it makes sense to have an in-flowing fact for each such label.  Yes, it could default to bottom, but it's probably a user error not to supply such a fact.

- If forwards analysis *does* is given a fact for each entry label, *Hoopl never needs to know the bottom of the lattice*; indeed there doesn't need to *be* a bottom.  Hoopl treats the block in dependency order, so it always has an in-flowing fact before it starts to analyse a block.  It needs still a join for the lattice, of course.

- For some analyses, it's quite clumsy to have a bottom element. Consider constant-propagation, where we want to transform

  ```wiki
    x := 3     ===>    x := 3
    ....               ...
    y = x+2            y = 3+2
  ```

  (We might then do constant folding and dead code elim, but ignore that for now.)  If we need a bottom in the lattice, our facts look like

  ```wiki
    data CPFact = Bot | CP (Map LocaReg Const)
  ```

  When a variable is not in the domain of the map it means it maps to top (ie the variable can hold different values on different control-flow paths).  This is all fine, but the join operation needs to deal with `Bot` etc.  And what is frustrating is that `Bot` is never, ever used!  I don't want to define it and manipulate it when it is never used!


Conclusion: for fwd analysis we don't need a bottom in the lattice, and it's a pain for (some) clients to supply one.

### Observations about backward analysis

- Backwards analysis currently takes a list of entry points, so
  that it find the reachable code and enumerate it in reverse
  order.  But that's *all* the entry point list does.  It'd be just fine
  to enumerate *all the blocks in the graph* in reverse order, and not supply
  a list of entry points.

- Backwards analysis (for a closed-on-entry graph) takes a `(Fact x f)` argument, for 
  a graph where `x` describes its open/closed on exit status.  So if x=O we pass one fact; 
  and that is entirely reasonable becuse it is the fact flowing backwards into the exit.
  But if x=C we pass a `FactBase`.  At first I thought that was stupid, but now I see 
  some sense in it: *these are facts labels outside (downstream successors of) the graph being analysed*.
  We'd better document this point.

- NB: returning to the first bullet, we can't just take code
  reachable from downstream successors (ie behave dually to fwd
  anal), because tail calls, returns, and infinite loops don't
  have any such downstream successors, but we jolly well want to
  analyse them.

- Backward analysis *does* need a bottom for the lattice, to initialise loops. Example:

  ```wiki
     L1: ...blah blah...
         CondBranch e L1 L2

     L2: blah blah
  ```

  When analysing L1 (backwards) we must join the facts flowing back from L2
  (which we will have analysed first) and L1; and on the first iteration, we don't 
  have any fact from L1.

> >
> > So for backwards analysis the client really must give us a bottom element.

### Conclusions


We could reflect these observations in the API for forwards and backward analysis, as follows.


Current situation:

```wiki
data FwdPass m n f
  = FwdPass { fp_lattice  :: DataflowLattice f
            , fp_transfer :: FwdTransfer n f
            , fp_rewrite  :: FwdRewrite m n f }

analyzeAndRewriteFwd
   :: forall m n f e x entries. (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => FwdPass m n f
   -> MaybeC e entries
   -> Graph n e x -> Fact e f
   -> m (Graph n e x, FactBase f, MaybeO x f)

data BwdPass m n f
  = BwdPass { bp_lattice  :: DataflowLattice f
            , bp_transfer :: BwdTransfer n f
            , bp_rewrite  :: BwdRewrite m n f }

analyzeAndRewriteBwd
   :: (CheckpointMonad m, NonLocal n, LabelsPtr entries)
   => BwdPass m n f
   -> MaybeC e entries -> Graph n e x -> Fact x f
   -> m (Graph n e x, FactBase f, MaybeO e f)
```


Possible refactoring:

```wiki
data FwdPass m n f
  = FwdPass { fp_join     :: JoinFun f   -- No "bottom" for fwd
            , fp_transfer :: FwdTransfer n f
            , fp_rewrite  :: FwdRewrite m n f }

analyzeAndRewriteFwd
   :: forall m n f e x entries. (CheckpointMonad m, NonLocal n)
   => FwdPass m n f
   -> Fact e f           -- Entry points plus a fact for each
   -> Graph n e x 
   -> m (Graph n e x, FactBase f, MaybeO x f)

data BwdPass m n f
  = BwdPass { bp_bot      :: f        -- Need "bottom" for bwd
            , bp_join     :: JoinFun f
            , bp_transfer :: BwdTransfer n f
            , bp_rewrite  :: BwdRewrite m n f }

analyzeAndRewriteBwd
   :: (CheckpointMonad m, NonLocal n)
   => BwdPass m n f
   -> Fact x f       -- Facts about successors
   -> Graph n e x
   -> m (Graph n e x, FactBase f, MaybeO e f)
```


The differences are not great. But the types are still nicely symmetrical; and they
say more precisely what is
actually necessary and useful.

## Smaller proposals

- (David Luposchainsky) Hoopl is the only library in GHC that defines its own `<*>` operation, 
  which will clash with the AMP. Hoopl's `<*>` is conceptually
  just `mappend`, so if you're doing a large-scale refactoring of the
  module maybe consider adding a suitable Monoid instance to replace `<*>`
  with `<>` (or something) before it even becomes a problem.

- Simon doesn't like the `joinInFacts` function, which is only called to possibly produce some debugging output from the join function.

- Jan doesn't like mess in Hoopl repo. There are unused modules (`Compiler.Hoopl.OldDataflow`, `Compiler.Hoopl.DataflowFold`), older versions of some modules (in `prototypes/` directory) or private correspondence with paper reviewers and between authors.

## A personal note by Jan Stolarek


(I think this may be subsumed by the above.)


On my first contact with Hoopl I was very confused by some of its behaviour. Here's [ a question I mailed to ghc-devs on 13th July 2013](http://www.haskell.org/pipermail/ghc-devs/2013-July/001757.html):

*In my algorithm I need to initialize all of the blocks in a graph with bottom element of a lattice, except for the entry block, which needs some other initial values. I've written something like this:*

```wiki
cmmCopyPropagation dflags graph = do
    let entry_blk = g_entry graph
    g' <- dataflowPassFwd graph [(entry_blk, (Top , Top))] $
            analRewFwd cpLattice cpTransfer cpRewrite
    return . fst $ g'

cpLattice = DataflowLattice "copy propagation" (Bottom, Bottom) cpJoin
```

*However, it seems that Bottom values passed to cpLattice are ignored - I could replace them with `undefined` and the code would still run without causing an error. Is there something obviously wrong in the way I pass initial fact values to dataflowPassFwd, or should I look for the problem in other parts of my code?*


I think this question resulted from Hoopl's current behaviour where it sometimes ignores bottom passed in by the user and sometimes does not.


When I did copy propagation pass I had data type that looked like this:

```wiki
data Facts = Bottom | Const (M.Map a CPFact)
```


and I wrote `join` function which analyzed all four possible cases of joining facts:

```wiki
join (Const f) (Const f) = ...
join (Const f) Bottom    = ...
join Bottom    (Const f) = ...
join Bottom    Bottom    = ...
```


But only one of them was ever used actually. While I expected this for the last two ones and replaced them with compiler panic, I certainly did not expect that `join (Const f) Bottom` will not be used. Only after some tiresome debugging and analyzing the source code did I realize that Hoopl optimizes away this kind of join. I think that being explicit about the redundance of bottom in forward analysis will make Hoopl easier to use for newcommers. (Note: if I were doing backward analysis I would still need to analyze all four cases).
