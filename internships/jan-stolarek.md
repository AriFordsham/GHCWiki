# Jan Stolarek's internship notes

# Various clean-up tasks

## Code

- `main/HscMain.lhs:1300`\`. Is:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT in

    let run_pipeline topSRT cmmgroup = do
          (topSRT, cmmgroup) <- cmmPipeline hsc_env topSRT cmmgroup
          return (topSRT,cmmgroup)

    in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```


The `<- / return` sequence in the definition of `run_pipeline` can be eliminated, which allows to remove the `do` notation, which allows to do eta-reduction, which (finally) allows to remove the `run_pipeline` binding and using `(cmmPipeline hsc_env)` instead:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT
    in do topSRT <- Stream.mapAccumL (cmmPipeline hsc_env) initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```

- `cmm/CmmLive.hs:106`. This function is not used (unless there is some auto-generated code that uses it, but I doubt it):

```wiki
removeDeadAssignments :: DynFlags -> CmmGraph
                      -> UniqSM (CmmGraph, BlockEnv CmmLocalLive)
```


It is however referenced in some of the comments!

- `cmm/CmmRewriteAssignments.hs` is not used at all?
- `cmm/CmmUtils.hs`, function `toBlockListEntryFirst` - perhaps it would be safer to return a tuple in this case? This would probably make the invariant more explicit.

## Wiki

- [NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline) has some outdated sections in the Cmm pipeline description: Add spill/reload, Rewrite assignments. So far I only marked them as OUTDATED
- [NewCodeGenModules](commentary/compiler/new-code-gen-modules) - mostly outdated. Mentioned data types and modules no longer exist.
