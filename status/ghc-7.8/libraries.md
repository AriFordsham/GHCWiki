# Status of GHC 7.8 Boot Libraries

TODO Update [Version History](commentary/libraries/version-history) when GHC 7.8 release reaches RC phase.

## GHC-owned Libraries


Common tasks:

- TODO clean-up `.cabal` files
- TODO add changelogs
- TODO add `/Since: x.y.z/` annotations

### [array](http://hackage.haskell.org/package/array)

https://travis-ci.org/ghc/packages-array

- performed deprecation-removals & major version bump
- released as [array-0.5.0.0](http://hackage.haskell.org/package/array-0.5.0.0)!

### [base](http://hackage.haskell.org/package/base)






- TODO make sure all recent additions are `/Since:/`-annotated
- TODO Deprecated functions since at least GHC 7.4:

  ```

  module Control.Concurrent.Chan
  {-# DEPRECATED unGetChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-} -- deprecated in 7.0
  {-# DEPRECATED isEmptyChan "if you need this operation, use Control.Concurrent.STM.TChan instead. See http://hackage.haskell.org/trac/ghc/ticket/4154 for details" #-} -- deprecated in 7.0

  module Data.Typeable.Internal
  {-# DEPRECATED tyConString "renamed to tyConName; tyConModule and tyConPackage are also available." #-} -- deprecated in 7.4

  module Debug.Trace
  {-# DEPRECATED putTraceMsg "Use Debug.Trace.traceIO" #-} -- deprecated in 7.4

  module GHC.Exts
  {-# DEPRECATED traceEvent "Use Debug.Trace.traceEvent or Debug.Trace.traceEventIO" #-} -- deprecated in 7.4

  ```

- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [deepseq](http://hackage.haskell.org/package/deepseq)



https://travis-ci.org/ghc/packages-deepseq


- cleaned up
- released as [deepseq-1.3.0.2](http://hackage.haskell.org/package/deepseq-1.3.0.2)!

### [directory](http://hackage.haskell.org/package/directory)



https://travis-ci.org/ghc/packages-directory




  
  
  
  
  
    
  
  

  



- cleaned up; almost ready for release
- TODO get \[cdc415a1fb/directory\] code-reviewed

### [filepath](http://hackage.haskell.org/package/filepath)



https://travis-ci.org/ghc/packages-filepath


- released as [filepath-1.3.0.2](http://hackage.haskell.org/package/filepath-1.3.0.2)

### [ghc-prim](http://hackage.haskell.org/package/ghc-prim)


- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [haskell2010](http://hackage.haskell.org/package/haskell2010)




  
  
  
  
  
    
  
  




- ready for release
- released as [haskell2010-1.1.2.0](http://hackage.haskell.org/package/haskell2010-1.1.2.0)

### [haskell98](http://hackage.haskell.org/package/haskell98)




  
  
  
  
  
    
  
  

  



- ready for release
- released as [haskell98-2.0.0.3](http://hackage.haskell.org/package/haskell98-2.0.0.3)

### [hoopl](http://hackage.haskell.org/package/hoopl)



https://travis-ci.org/ghc/packages-hoopl


- released as [hoopl-3.10.0.0](http://hackage.haskell.org/package/hoopl-3.10.0.0)

### [hpc](http://hackage.haskell.org/package/hpc)




  
  
  
  
  
    
  
  

  




https://travis-ci.org/ghc/packages-hpc

- released as [hpc-0.6.0.1](http://hackage.haskell.org/package/hpc-0.6.0.1)

### [integer-gmp](http://hackage.haskell.org/package/integer-gmp)

- Candidate is on Hackage

- TODO upload a non-candidate to hackage at latest when GHC 7.8.1 is released

### [old-locale](http://hackage.haskell.org/package/old-locale)

https://travis-ci.org/ghc/packages-old-locale

- released as [old-locale-1.0.0.6](http://hackage.haskell.org/package/old-locale-1.0.0.6)

### [old-time](http://hackage.haskell.org/package/old-time)




  
  
  
  
  
    
  
  


  



- released as [old-time-1.1.0.2](http://hackage.haskell.org/package/old-time-1.1.0.2)

### [parallel](http://hackage.haskell.org/package/parallel)

https://travis-ci.org/ghc/packages-parallel

- Note: **extra** library (i.e. not part of normal distribution)

- uploaded [parallel-3.2.0.4](http://hackage.haskell.org/package/parallel-3.2.0.4) to Hackage!

- For next major version: contains many `DEPRECATED` functions in `Control.Parallel.Strategies` since at least 2010:

  ```
  {-# DEPRECATED Done "The Strategy type is now a -> Eval a, not a -> Done" #-}
  {-# DEPRECATED demanding "Use pseq or $| instead" #-}
  {-# DEPRECATED sparking "Use par or $|| instead" #-}
  {-# DEPRECATED (>|) "Use pseq or $| instead" #-}
  {-# DEPRECATED (>||) "Use par or $|| instead" #-}
  {-# DEPRECATED rwhnf "renamed to rseq" #-}
  {-# DEPRECATED seqTraverse "renamed to evalTraversable" #-}
  {-# DEPRECATED parTraverse "renamed to parTraversable" #-}
  {-# DEPRECATED parListWHNF "use (parList rseq) instead" #-}
  {-# DEPRECATED seqList "renamed to evalList" #-}
  {-# DEPRECATED seqPair "renamed to evalTuple2" #-}
  {-# DEPRECATED parPair "renamed to parTuple2" #-}
  {-# DEPRECATED seqTriple "renamed to evalTuple3" #-}
  {-# DEPRECATED parTriple "renamed to parTuple3" #-}
  {-# DEPRECATED unEval "renamed to runEval" #-}
  ```

### [process](http://hackage.haskell.org/package/process)



https://travis-ci.org/ghc/packages-process




  
  
  
  
  
    
  
  


  



- released as [process-1.2.0.0](http://hackage.haskell.org/package/process-1.2.0.0)!

### [stm](http://hackage.haskell.org/package/stm)

https://travis-ci.org/ghc/packages-stm

- Note: this is an **extra** library (i.e. not part of binary distribution); not critical for release; [stm-2.4.2](http://hackage.haskell.org/package/stm-2.4.2) still builds fine with GHC HEAD.
- cleaned up; repo contains unreleased 2.4.2.1 state
- TODO find out when/whether to release an updated 2.4.2.1 version

### [template-haskell](http://hackage.haskell.org/package/template-haskell)

- cleaned up and ready for release
- wait till RC at least for Hackage upload

### [unix](http://hackage.haskell.org/package/unix)



https://travis-ci.org/ghc/packages-unix




  
  
  
  
  
    
  
  



  



- released as [unix-2.7.0.0](http://hackage.haskell.org/package/unix-2.7.0.0)!

- TODO for next major version bump, in `System.Posix.Process.Common` module:

  ```
  {-# DEPRECATED createProcessGroup "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use createProcessGroupFor instead." #-} -- deprecated in 7.2

  {-# DEPRECATED setProcessGroupID "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use setProcessGroupIdOf instead." #-} -- deprecated in 7.2
  ```

### DPH

TODO

## 3rd Party Libraries


Note: Libraries with a "<sup>1</sup>" marker are used internally (e.g. by `ghci` or `haddock`) and not exposed in the (boot-)package database. Libraries marked "<sup>2</sup>" are used by DPH.

### Cabal

- upstream: [http://github.com/haskell/cabal.git](http://github.com/haskell/cabal.git)
- using [Cabal-1.18.1.3](http://hackage.haskell.org/package/Cabal-1.18.1.3)

### Win32

- upstream: [http://github.com/haskell/win32.git](http://github.com/haskell/win32.git)
- ghc-7.8 branch is at [Win32-2.3.0.1](http://hackage.haskell.org/package/Win32-2.3.0.1)
- TODO update ghc-7.8 branch to [Win32-2.3.0.2](http://hackage.haskell.org/package/Win32-2.3.0.2) (GHC HEAD already has it)

### binary

- upstream: [http://github.com/kolmodin/binary.git](http://github.com/kolmodin/binary.git)
- synced up cleanly to [binary-0.7.1.0](http://hackage.haskell.org/package/binary-0.7.1.0) release [\[25f1bda7/ghc\]](/trac/ghc/changeset/25f1bda7/ghc)

### bytestring

- upstream: [http://github.com/haskell/bytestring.git](http://github.com/haskell/bytestring.git)
- synced up cleanly to [bytestring-0.10.4.0](http://hackage.haskell.org/package/bytestring-0.10.4.0) release [\[82456db5/ghc\]](/trac/ghc/changeset/82456db5/ghc)

### containers

- upstream: [http://github.com/haskell/containers.git](http://github.com/haskell/containers.git)
- synced up cleanly to [containers-0.5.5.1](http://hackage.haskell.org/package/containers-0.5.5.1) release

### haskeline<sup>1</sup>

- upstream: [http://git.haskell.org/darcs-mirrors/haskeline.git](http://git.haskell.org/darcs-mirrors/haskeline.git)
- unclean state
- contacted maintainer on 31.8. wrt to unmerged patches; will merge as soon as his dev machine has been repaired
- patches have been merged upstream
- New Haskeline GitHub repo at [https://github.com/judah/haskeline.git](https://github.com/judah/haskeline.git)
- synced up to [haskeline-0.7.1.2](http://hackage.haskell.org/package/haskeline-0.7.1.2)

### pretty




  
  
  
  
  
    
  
  



  



- upstream: [http://github.com/haskell/pretty.git](http://github.com/haskell/pretty.git)
- synced up to [pretty-1.1.1.1](http://hackage.haskell.org/package/pretty-1.1.1.1) [\[f275522e/ghc\]](/trac/ghc/changeset/f275522e/ghc)

### primitive<sup>2</sup>

- upstream: [http://git.haskell.org/darcs-mirrors/primitive.git](http://git.haskell.org/darcs-mirrors/primitive.git)
- Jan is working on patches to integrate the new bool primops
- upstream repo moved to [https://github.com/haskell/primitive](https://github.com/haskell/primitive)
- synced up to [primitive-0.5.2.1](http://hackage.haskell.org/package/primitive-0.5.2.1)

### random<sup>2</sup>

- TODO GHC's repo has diverged from upstream

### terminfo<sup>1</sup>


- upstream: [http://git.haskell.org/darcs-mirrors/terminfo.git](http://git.haskell.org/darcs-mirrors/terminfo.git)
- synced up to [terminfo-0.4.0.0](http://hackage.haskell.org/package/terminfo-0.4.0.0) [\[9642716f30/ghc\]](/trac/ghc/changeset/9642716f30/ghc)

### time

- upstream: [http://git.haskell.org/darcs-mirrors/time.git](http://git.haskell.org/darcs-mirrors/time.git)
- synced up cleanly to [time-1.4.2](http://hackage.haskell.org/package/time-1.4.2) release

### transformers

- upstream: [http://git.haskell.org/darcs-mirrors/transformers.git](http://git.haskell.org/darcs-mirrors/transformers.git)
- Newly added in GHC 7.8
- still at [transformers-0.3.0.0](http://hackage.haskell.org/package/transformers-0.3.0.0) since GHC 7.6.3
- asked upstream; shall remain at [transformers-0.3.0.0](http://hackage.haskell.org/package/transformers-0.3.0.0) for GHC 7.8.1

### vector<sup>2</sup>

- upstream: [http://git.haskell.org/darcs-mirrors/vector.git](http://git.haskell.org/darcs-mirrors/vector.git)
- coupled with `primitive` library; see notes there
- upstream repo moved to [https://github.com/haskell/vector](https://github.com/haskell/vector)
- synced up to [vector-0.10.9.1](http://hackage.haskell.org/package/vector-0.10.9.1) [\[5e2f145a37/ghc\]](/trac/ghc/changeset/5e2f145a37/ghc)

### xhtml<sup>1</sup>

- upstream: [http://github.com/haskell/xhtml](http://github.com/haskell/xhtml)
- unmodified/clean-sync at [xhtml-3000.2.1](http://hackage.haskell.org/package/xhtml-3000.2.1) release since GHC 7.6.3 release
