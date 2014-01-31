# Status of GHC 7.8 Boot Libraries

TODO Update [Version History](commentary/libraries/version-history) when GHC 7.8 release reaches RC phase.

## GHC-owned Libraries


Common tasks:

- TODO clean-up `.cabal` files
- TODO add changelogs
- TODO add `/Since: x.y.z/` annotations

### [ array](http://hackage.haskell.org/package/array)

[](https://travis-ci.org/ghc/packages-array)

- performed deprecation-removals & major version bump
- released as [ array-0.5.0.0](http://hackage.haskell.org/package/array-0.5.0.0)!

### [ base](http://hackage.haskell.org/package/base)

##
    Results (1 - 10 of 76)

12 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 2, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)3 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 3, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)4 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 4, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)5 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 5, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)6 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 6, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)7 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 7, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)8 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 8, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)→ (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 2, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>[\#913](https://gitlab.haskell.org//ghc/ghc/issues/913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[instance Ord (StableName a)](https://gitlab.haskell.org//ghc/ghc/issues/913)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>ekarttun</th></tr>
<tr><th>[\#3399](https://gitlab.haskell.org//ghc/ghc/issues/3399)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      proposal
                    </th>
<th>[Generalize the type of Data.List.{deleteBy, deleteFirstsBy}](https://gitlab.haskell.org//ghc/ghc/issues/3399)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>iago</th></tr>
<tr><th>[\#9795](https://gitlab.haskell.org//ghc/ghc/issues/9795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Debug.Trace.trace is too strict](https://gitlab.haskell.org//ghc/ghc/issues/9795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>jcpetruzza</th></tr>
<tr><th>[\#9936](https://gitlab.haskell.org//ghc/ghc/issues/9936)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data.Fixed truncates 5.17 to 5.16](https://gitlab.haskell.org//ghc/ghc/issues/9936)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>singpolyma</th></tr>
<tr><th>[\#10055](https://gitlab.haskell.org//ghc/ghc/issues/10055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Offer PolyKinded instances for Data.Fixed.HasResolution](https://gitlab.haskell.org//ghc/ghc/issues/10055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th>
<th>redneb</th></tr>
<tr><th>[\#10084](https://gitlab.haskell.org//ghc/ghc/issues/10084)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Data.List should have a takeLastN function](https://gitlab.haskell.org//ghc/ghc/issues/10084)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>leonbaum2</th></tr>
<tr><th>[\#10169](https://gitlab.haskell.org//ghc/ghc/issues/10169)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[bracket not running the final action on termination through SIGTERM](https://gitlab.haskell.org//ghc/ghc/issues/10169)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>Kritzefitz</th></tr>
<tr><th>[\#10266](https://gitlab.haskell.org//ghc/ghc/issues/10266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Split base for Backpack](https://gitlab.haskell.org//ghc/ghc/issues/10266)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>ezyang</th>
<th>ezyang</th></tr>
<tr><th>[\#10484](https://gitlab.haskell.org//ghc/ghc/issues/10484)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hPutBuf crashes when trying to write a large string to stdout (resource exhausted)](https://gitlab.haskell.org//ghc/ghc/issues/10484)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>bayloff</th></tr>
<tr><th>[\#11009](https://gitlab.haskell.org//ghc/ghc/issues/11009)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Errors reading stdin on Windows](https://gitlab.haskell.org//ghc/ghc/issues/11009)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>ncreep</th></tr></table>

12 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 2, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)3 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 3, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)4 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 4, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)5 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 5, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)6 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 6, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)7 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 7, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)8 (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 8, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)→ (Ticket query: status: !closed, component: libraries%2Fbase, max: 10, page: 2, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: id)

- TODO make sure all recent additions are `/Since:/`-annotated
- TODO Deprecated functions since at least GHC 7.4:

  ```
  moduleControl.Concurrent.Chan{-# DEPRECATED unGetChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-}-- deprecated in 7.0{-# DEPRECATED isEmptyChan "if you need this operation, use Control.Concurrent.STM.TChan instead. See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-}-- deprecated in 7.0moduleData.Typeable.Internal{-# DEPRECATED tyConString "renamed to tyConName; tyConModule and tyConPackage are also available." #-}-- deprecated in 7.4moduleDebug.Trace{-# DEPRECATED putTraceMsg "Use Debug.Trace.traceIO" #-}-- deprecated in 7.4moduleGHC.Exts{-# DEPRECATED traceEvent "Use Debug.Trace.traceEvent or Debug.Trace.traceEventIO" #-}-- deprecated in 7.4
  ```

- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ deepseq](http://hackage.haskell.org/package/deepseq)

[](https://travis-ci.org/ghc/packages-deepseq)

- cleaned up
- relased as [ deepseq-1.3.0.2](http://hackage.haskell.org/package/deepseq-1.3.0.2)!

### [ directory](http://hackage.haskell.org/package/directory)

[](https://travis-ci.org/ghc/packages-directory)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fdirectory, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- cleaned up; almost ready for release
- TODO get \[cdc415a1fb/directory\] code-reviewed

### [ filepath](http://hackage.haskell.org/package/filepath)

[](https://travis-ci.org/ghc/packages-filepath)

### [ ghc-prim](http://hackage.haskell.org/package/ghc-prim)

- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ haskell2010](http://hackage.haskell.org/package/haskell2010)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fhaskell2010, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- ready for release
- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ haskell98](http://hackage.haskell.org/package/haskell98)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fhaskell98, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- ready for release
- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ hoopl](http://hackage.haskell.org/package/hoopl)

[](https://travis-ci.org/ghc/packages-hoopl)

- TODO changelog entry for 3.10.0.0

### [ hpc](http://hackage.haskell.org/package/hpc)

<table><tr><th>Ticket (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: Code+Coverage, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>[\#1853](https://gitlab.haskell.org//ghc/ghc/issues/1853)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hpc mix files for Main modules overwrite each other](https://gitlab.haskell.org//ghc/ghc/issues/1853)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th>
<th>guest</th></tr>
<tr><th>[\#2075](https://gitlab.haskell.org//ghc/ghc/issues/2075)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[hpc should render information about the run in its html markup](https://gitlab.haskell.org//ghc/ghc/issues/2075)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>andy@…</th>
<th>dons</th></tr>
<tr><th>[\#2224](https://gitlab.haskell.org//ghc/ghc/issues/2224)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[-fhpc inteferes/prevents rewrite rules from firing](https://gitlab.haskell.org//ghc/ghc/issues/2224)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>andy@…</th>
<th>dons</th></tr>
<tr><th>[\#10367](https://gitlab.haskell.org//ghc/ghc/issues/10367)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["ghc: panic! (the 'impossible' happened)"](https://gitlab.haskell.org//ghc/ghc/issues/10367)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>bgwines</th></tr>
<tr><th>[\#10504](https://gitlab.haskell.org//ghc/ghc/issues/10504)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC panics with dsImpSpecs on SPECIALISE pragma with -fhpc enabled](https://gitlab.haskell.org//ghc/ghc/issues/10504)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>nh2</th></tr>
<tr><th>[\#10951](https://gitlab.haskell.org//ghc/ghc/issues/10951)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[HPC program has poor error reporting / strange CLI in general](https://gitlab.haskell.org//ghc/ghc/issues/10951)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th>
<th>mgsloan</th></tr>
<tr><th>[\#10952](https://gitlab.haskell.org//ghc/ghc/issues/10952)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Use IPids instead of package keys in HPC tix files](https://gitlab.haskell.org//ghc/ghc/issues/10952)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th>
<th>mgsloan</th></tr>
<tr><th>[\#12631](https://gitlab.haskell.org//ghc/ghc/issues/12631)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\`hpc report\` silently ignore non-existent modules](https://gitlab.haskell.org//ghc/ghc/issues/12631)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>quabla</th></tr>
<tr><th>[\#13448](https://gitlab.haskell.org//ghc/ghc/issues/13448)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Make HPC use an RTS option to select the tix file](https://gitlab.haskell.org//ghc/ghc/issues/13448)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dfeuer</th>
<th>dfeuer</th></tr>
<tr><th>[\#13452](https://gitlab.haskell.org//ghc/ghc/issues/13452)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Lock .tix file](https://gitlab.haskell.org//ghc/ghc/issues/13452)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>dfeuer</th></tr>
<tr><th>[\#14711](https://gitlab.haskell.org//ghc/ghc/issues/14711)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Machine readable output of coverage](https://gitlab.haskell.org//ghc/ghc/issues/14711)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>jproyo</th>
<th>Koterpillar</th></tr>
<tr><th>[\#15498](https://gitlab.haskell.org//ghc/ghc/issues/15498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[HPC: do notation marks () as non-covered](https://gitlab.haskell.org//ghc/ghc/issues/15498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>tom-bop</th></tr>
<tr><th>[\#15776](https://gitlab.haskell.org//ghc/ghc/issues/15776)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Untested modules excluded from hpc coverage report](https://gitlab.haskell.org//ghc/ghc/issues/15776)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>ramirez7</th></tr>
<tr><th>[\#15932](https://gitlab.haskell.org//ghc/ghc/issues/15932)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[DeriveFunctor and GeneralizedNewtypeDeriving instances never reporting as covered](https://gitlab.haskell.org//ghc/ghc/issues/15932)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th>
<th>davean</th></tr>
<tr><th>[\#16380](https://gitlab.haskell.org//ghc/ghc/issues/16380)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[HPC's CLI is awkward](https://gitlab.haskell.org//ghc/ghc/issues/16380)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th>
<th>MaxGabriel</th></tr></table>

[](https://travis-ci.org/ghc/packages-hpc)

- ready for release 
- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ integer-gmp](http://hackage.haskell.org/package/integer-gmp)

- Candidate is on Hackage

- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ old-locale](http://hackage.haskell.org/package/old-locale)

[](https://travis-ci.org/ghc/packages-old-locale)

- ready for release
- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ old-time](http://hackage.haskell.org/package/old-time)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fold-time, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- ready for release
- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [ parallel](http://hackage.haskell.org/package/parallel)

[](https://travis-ci.org/ghc/packages-parallel)

- Note: **extra** library (i.e. not part of normal distribution)

- uploaded [ parallel-3.2.0.4](http://hackage.haskell.org/package/parallel-3.2.0.4) to Hackage!

- For next major version: contains many `DEPRECATED` functions in `Control.Parallel.Strategies` since at least 2010:

  ```
  {-# DEPRECATED Done "The Strategy type is now a -> Eval a, not a -> Done" #-}{-# DEPRECATED demanding "Use pseq or $| instead" #-}{-# DEPRECATED sparking "Use par or $|| instead" #-}{-# DEPRECATED (>|) "Use pseq or $| instead" #-}{-# DEPRECATED (>||) "Use par or $|| instead" #-}{-# DEPRECATED rwhnf "renamed to rseq" #-}{-# DEPRECATED seqTraverse "renamed to evalTraversable" #-}{-# DEPRECATED parTraverse "renamed to parTraversable" #-}{-# DEPRECATED parListWHNF "use (parList rseq) instead" #-}{-# DEPRECATED seqList "renamed to evalList" #-}{-# DEPRECATED seqPair "renamed to evalTuple2" #-}{-# DEPRECATED parPair "renamed to parTuple2" #-}{-# DEPRECATED seqTriple "renamed to evalTuple3" #-}{-# DEPRECATED parTriple "renamed to parTuple3" #-}{-# DEPRECATED unEval "renamed to runEval" #-}
  ```

### [ process](http://hackage.haskell.org/package/process)

[](https://travis-ci.org/ghc/packages-process)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fprocess, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- released as [ process-1.2.0.0](http://hackage.haskell.org/package/process-1.2.0.0)!

### [ stm](http://hackage.haskell.org/package/stm)

[](https://travis-ci.org/ghc/packages-stm)

- Note: this is an **extra** library (i.e. not part of binary distribution); not critical for release; [ stm-2.4.2](http://hackage.haskell.org/package/stm-2.4.2) still builds fine with GHC HEAD.
- cleaned up; repo contains unreleased 2.4.2.1 state
- TODO find out when/whether to release an updated 2.4.2.1 version

### [ template-haskell](http://hackage.haskell.org/package/template-haskell)

- cleaned up and ready for release
- wait till RC at least for Hackage upload

### [ unix](http://hackage.haskell.org/package/unix)

[](https://travis-ci.org/ghc/packages-unix)

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Funix, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>[\#16099](https://gitlab.haskell.org//ghc/ghc/issues/16099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[expose st_blksize field from fstat syscall](https://gitlab.haskell.org//ghc/ghc/issues/16099)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th>
<th>flip101</th></tr></table>

- released as [ unix-2.7.0.0](http://hackage.haskell.org/package/unix-2.7.0.0)!

- TODO for next major version bump, in `System.Posix.Process.Common` module:

  ```
  {-# DEPRECATED createProcessGroup "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use createProcessGroupFor instead." #-}-- deprecated in 7.2{-# DEPRECATED setProcessGroupID "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use setProcessGroupIdOf instead." #-}-- deprecated in 7.2
  ```

### DPH

TODO

## 3rd Party Libraries


Note: Libraries with a "<sup>1</sup>" marker are used internally (e.g. by `ghci` or `haddock`) and not exposed in the (boot-)package database. Libraries marked "<sup>2</sup>" are used by DPH (and only installed when `InstallExtraPackages=YES`)

### Cabal

- upstream: [ http://github.com/haskell/cabal.git](http://github.com/haskell/cabal.git)
- TODO ell Johan when to cut Cabal-1.18.1.3 release

### Win32

- upstream: [ http://github.com/haskell/win32.git](http://github.com/haskell/win32.git)
- TODO upstream needs to cut release

### binary

- upstream: [ http://github.com/kolmodin/binary.git](http://github.com/kolmodin/binary.git)
- synced up cleanly to [ binary-0.7.1.0](http://hackage.haskell.org/package/binary-0.7.1.0) release [\[25f1bda7/ghc\]](/trac/ghc/changeset/25f1bda7/ghc)

### bytestring

- upstream: [ http://github.com/haskell/bytestring.git](http://github.com/haskell/bytestring.git)
- synced up cleanly to [ bytestring-0.10.4.0](http://hackage.haskell.org/package/bytestring-0.10.4.0) release [\[82456db5/ghc\]](/trac/ghc/changeset/82456db5/ghc)

### containers

- upstream: [ http://github.com/haskell/containers.git](http://github.com/haskell/containers.git)
- synced up cleanly to [ containers-0.5.4.0](http://hackage.haskell.org/package/containers-0.5.4.0) release
- TODO AMP-warning fix pending (maybe)

### haskeline<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/haskeline.git](http://git.haskell.org/darcs-mirrors/haskeline.git)
- unclean state
- contacted maintainer on 31.8. wrt to unmerged patches; will merge as soon as his dev machine has been repaired
- patches have been merged upstream
- New Haskeline GitHub repo at [ https://github.com/judah/haskeline.git](https://github.com/judah/haskeline.git)
- synced up to [ haskeline-0.7.1.2](http://hackage.haskell.org/package/haskeline-0.7.1.2)

### pretty

<table><tr><th>Ticket (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, desc: 1, order: id)</th>
<th>Type (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: type)</th>
<th>Summary (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: summary)</th>
<th>Priority (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: priority)</th>
<th>Owner (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: !closed, component: libraries%2Fpretty, max: 0, col: id, col: type, col: summary, col: priority, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

- upstream: [ http://github.com/haskell/pretty.git](http://github.com/haskell/pretty.git)
- synced up to [ pretty-1.1.1.1](http://hackage.haskell.org/package/pretty-1.1.1.1)[\[f275522e/ghc\]](/trac/ghc/changeset/f275522e/ghc)

### primitive<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/primitive.git](http://git.haskell.org/darcs-mirrors/primitive.git)
- Jan is working on patches to integrate the new bool primops
- upstream repo moved to [ https://github.com/haskell/primitive](https://github.com/haskell/primitive)
- synced up to `v0.5.1.0` + patches [\[5e2f145a37/ghc\]](/trac/ghc/changeset/5e2f145a37/ghc)
- TODO proper `0.5.2.0` release pending

### random<sup>2</sup>

- TODO GHC's repo is slightly **ahead** of upstream

### terminfo<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/terminfo.git](http://git.haskell.org/darcs-mirrors/terminfo.git)
- synced up to [ terminfo-0.4.0.0](http://hackage.haskell.org/package/terminfo-0.4.0.0)[\[9642716f30/ghc\]](/trac/ghc/changeset/9642716f30/ghc)

### time

- upstream: [ http://git.haskell.org/darcs-mirrors/time.git](http://git.haskell.org/darcs-mirrors/time.git)
- synced up cleanly to [ time-1.4.1](http://hackage.haskell.org/package/time-1.4.1) release \[d55a4f3/ghc\]

### transformers

- upstream: [ http://git.haskell.org/darcs-mirrors/transformers.git](http://git.haskell.org/darcs-mirrors/transformers.git)
- Newly added in GHC 7.8
- still at [ transformers-0.3.0.0](http://hackage.haskell.org/package/transformers-0.3.0.0) since GHC 7.6.3
- asked upstream; shall remain at [ transformers-0.3.0.0](http://hackage.haskell.org/package/transformers-0.3.0.0) for GHC 7.8.1

### vector<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/vector.git](http://git.haskell.org/darcs-mirrors/vector.git)
- coupled with `primitive` library; see notes there
- upstream repo moved to [ https://github.com/haskell/vector](https://github.com/haskell/vector)
- synced up to [ vector-0.10.9.1](http://hackage.haskell.org/package/vector-0.10.9.1)[\[5e2f145a37/ghc\]](/trac/ghc/changeset/5e2f145a37/ghc)

### xhtml<sup>1</sup>

- upstream: [ http://github.com/haskell/xhtml](http://github.com/haskell/xhtml)
- unmodified/clean-sync at [ xhtml-3000.2.1](http://hackage.haskell.org/package/xhtml-3000.2.1) release since GHC 7.6.3 release
