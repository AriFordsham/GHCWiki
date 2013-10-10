# Status of GHC 7.8 Boot Libraries

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
