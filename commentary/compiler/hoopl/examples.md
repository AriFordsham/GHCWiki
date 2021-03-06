# Hoopl examples


This page gathers examples of Hoopl usage so that beginners can learn from them.

## Dead assignment removal


This was originally part of `CmmLive` module but was removed due to being too slow. According to Simon Marlow "a single `removeDeadAssigments` pass costs about 5% of compilation time, and in the vast majority of code does nothing over what `CmmSink` already does". 

```wiki
removeDeadAssignments :: DynFlags -> CmmGraph
                      -> UniqSM (CmmGraph, BlockEnv CmmLocalLive)
removeDeadAssignments dflags g =
   dataflowPassBwd g [] $ analRewBwd liveLattice (xferLive dflags) rewrites
   where rewrites = mkBRewrite3 nothing middle nothing
         -- SDM: no need for deepBwdRw here, we only rewrite to empty
         -- Beware: deepBwdRw with one polymorphic function seems more
         -- reasonable here, but GHC panics while compiling, see bug
         -- #4045.
         middle :: CmmNode O O -> Fact O CmmLocalLive -> CmmReplGraph O O
         middle (CmmAssign (CmmLocal reg') _) live
                 | not (reg' `elemRegSet` live)
                 = return $ Just emptyGraph
         -- XXX maybe this should be somewhere else...
         middle (CmmAssign lhs (CmmReg rhs))   _ | lhs == rhs
                 = return $ Just emptyGraph
         middle (CmmStore lhs (CmmLoad rhs _)) _ | lhs == rhs
                 = return $ Just emptyGraph
         middle _ _ = return Nothing

         nothing :: CmmNode e x -> Fact x CmmLocalLive -> CmmReplGraph e x
         nothing _ _ = return Nothing
```

## CmmRewriteAssignments optimization pass


The `CmmRewriteAssignments` pass was originally written by Edward Z. Yang to perform Cmm optimizations like inlining and sinking. However, it turned out to be too slow and was replaced with `CmmSink` pass written by Simon Marlow. `CmmSink` does almost the same things as `CmmRewriteAssignments`, the most notable difference being that the former does not handle loops. Code of `CmmRewriteAssignments` is available in [this attachment](/trac/ghc/attachment/wiki/Commentary/Compiler/Hoopl/Examples/CmmRewriteAssignments.hs).
