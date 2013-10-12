# Status of GHC 7.8 Boot Libraries

TODO Update [Version History](commentary/libraries/version-history) when GHC 7.8 release reaches RC phase.

## GHC-owned Libraries


Common tasks:

- TODO clean-up `.cabal` files

### [ array](http://hackage.haskell.org/package/array)

- performed deprecation-removals & major version bump
- TODO make release

### [ base](http://hackage.haskell.org/package/base)

- TODO make sure all recent additions are `/Since:/`-annotated

### [ deepseq](http://hackage.haskell.org/package/deepseq)

- cleaned up
- TODO make release

### [ directory](http://hackage.haskell.org/package/directory)

### [ filepath](http://hackage.haskell.org/package/filepath)

### [ ghc-prim](http://hackage.haskell.org/package/ghc-prim)

### [ haskell2010](http://hackage.haskell.org/package/haskell2010)

### [ haskell98](http://hackage.haskell.org/package/haskell98)

### [ hoopl](http://hackage.haskell.org/package/hoopl)

### [ integer-gmp](http://hackage.haskell.org/package/integer-gmp)

- not yet on Hackage, but planned to upload in order to have Haddock docs there

### [ old-locale](http://hackage.haskell.org/package/old-locale)

### [ old-time](http://hackage.haskell.org/package/old-time)

### [ parallel](http://hackage.haskell.org/package/parallel)

- TODO contains many `DEPRECATED` functions in `Control.Parallel.Strategies` since at least 2010:

  ```
  {-# DEPRECATED Done "The Strategy type is now a -> Eval a, not a -> Done" #-}{-# DEPRECATED demanding "Use pseq or $| instead" #-}{-# DEPRECATED sparking "Use par or $|| instead" #-}{-# DEPRECATED (>|) "Use pseq or $| instead" #-}{-# DEPRECATED (>||) "Use par or $|| instead" #-}{-# DEPRECATED rwhnf "renamed to rseq" #-}{-# DEPRECATED seqTraverse "renamed to evalTraversable" #-}{-# DEPRECATED parTraverse "renamed to parTraversable" #-}{-# DEPRECATED parListWHNF "use (parList rseq) instead" #-}{-# DEPRECATED seqList "renamed to evalList" #-}{-# DEPRECATED seqPair "renamed to evalTuple2" #-}{-# DEPRECATED parPair "renamed to parTuple2" #-}{-# DEPRECATED seqTriple "renamed to evalTuple3" #-}{-# DEPRECATED parTriple "renamed to parTuple3" #-}{-# DEPRECATED unEval "renamed to runEval" #-}
  ```

### [ process](http://hackage.haskell.org/package/process)

### [ stm](http://hackage.haskell.org/package/stm)

### [ template-haskell](http://hackage.haskell.org/package/template-haskell)

### [ unix](http://hackage.haskell.org/package/unix)

- cleaned up; almost ready for a 2.7.0.0 release
- TODO open issue in `System.Posix.Process.Common` module, remove those functions or leave them?

  ```
  {-# DEPRECATED createProcessGroup "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use createProcessGroupFor instead." #-}-- deprecated in 7.2{-# DEPRECATED setProcessGroupID "This function is scheduled to be replaced by something different in the future, we therefore recommend that you do not use this version and use setProcessGroupIdOf instead." #-}-- deprecated in 7.2
  ```

### DPH

TODO

## 3rd Party Libraries


Note: Libraries with a "<sup>1</sup>" marker are used internally (e.g. by `ghci` or `haddock`) and not exposed in the (boot-)package database. Libraries marked "<sup>2</sup>" are used by DPH (and only installed when `InstallExtraPackages=YES`)

### Cabal

- upstream: [ http://github.com/haskell/cabal.git](http://github.com/haskell/cabal.git)
- synced up cleanly to final [ Cabal-1.18.1](http://hackage.haskell.org/package/Cabal-1.18.1) release [\[2fbfa11c/ghc\]](/trac/ghc/changeset/2fbfa11c/ghc)

### Win32

- upstream: [ http://github.com/haskell/win32.git](http://github.com/haskell/win32.git)
- TODO

### binary

- upstream: [ http://github.com/kolmodin/binary.git](http://github.com/kolmodin/binary.git)
- synced up cleanly to [ binary-0.7.1.0](http://hackage.haskell.org/package/binary-0.7.1.0) release [\[25f1bda7/ghc\]](/trac/ghc/changeset/25f1bda7/ghc)

### bytestring

- upstream: [ http://github.com/haskell/bytestring.git](http://github.com/haskell/bytestring.git)
- synced up cleanly to [ bytestring-0.10.4.0](http://hackage.haskell.org/package/bytestring-0.10.4.0) release [\[82456db5/ghc\]](/trac/ghc/changeset/82456db5/ghc)

### containers

- upstream: [ http://github.com/haskell/containers.git](http://github.com/haskell/containers.git)
- synced up cleanly to [ containers-0.5.3.1](http://hackage.haskell.org/package/containers-0.5.3.1) release \[8a8cfb2/ghc\]
- TODO AMP-warning fix pending (maybe)

### haskeline<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/haskeline.git](http://git.haskell.org/darcs-mirrors/haskeline.git)
- unclean state
- contacted maintainer on 31.8. wrt to unmerged patches; will merge as soon as his dev machine has been repaired
- TODO

### pretty

- upstream: [ http://github.com/haskell/pretty.git](http://github.com/haskell/pretty.git)
- not pointing to proper release version
- no significant delta to upstream HEAD state
- TODO

### primitive<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/primitive.git](http://git.haskell.org/darcs-mirrors/primitive.git)
- Jan is working on patches to integrate the new bool primops
- upstream repo moved to [ https://github.com/haskell/primitive](https://github.com/haskell/primitive)
- synced up to `v0.5.1.0` + patches [\[5e2f145a37/ghc\]](/trac/ghc/changeset/5e2f145a37/ghc)
- TODO proper `0.5.1.1` release pending

### terminfo<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/terminfo.git](http://git.haskell.org/darcs-mirrors/terminfo.git)
- TODO

### time

- upstream: [ http://git.haskell.org/darcs-mirrors/time.git](http://git.haskell.org/darcs-mirrors/time.git)
- synced up cleanly to [ time-1.4.1](http://hackage.haskell.org/package/time-1.4.1) release \[d55a4f3/ghc\]

### transformers

- upstream: [ http://git.haskell.org/darcs-mirrors/transformers.git](http://git.haskell.org/darcs-mirrors/transformers.git)
- Newly added in GHC 7.8
- still at 0.3.0.0 since GHC 7.6.3
- asked upstream; shall remain at [ transformers-0.3.0.0](http://hackage.haskell.org/package/transformers-0.3.0.0) for GHC 7.8.1

### vector<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/vector.git](http://git.haskell.org/darcs-mirrors/vector.git)
- coupled with `primitive` library; see notes there
- upstream repo moved to [ https://github.com/haskell/vector](https://github.com/haskell/vector)
- synced up to [ vector-0.10.9.1](http://hackage.haskell.org/package/vector-0.10.9.1)[\[5e2f145a37/ghc\]](/trac/ghc/changeset/5e2f145a37/ghc)

### xhtml<sup>1</sup>

- upstream: [ http://github.com/haskell/xhtml](http://github.com/haskell/xhtml)
- unmodified/clean-sync at [ xhtml-3000.2.1](http://hackage.haskell.org/package/xhtml-3000.2.1) release since GHC 7.6.3 release
