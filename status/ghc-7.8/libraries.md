# Status of GHC 7.8 Boot Libraries

## 3rd Party Libraries


Note: Libraries with a "<sup>1</sup>" marker are used internally (e.g. by `ghci` or `haddock`) and not exposed in the (boot-)package database. Libraries marked "<sup>2</sup>" are used by DPH (and only installed when `InstallExtraPackages=YES`)

### Cabal

- upstream: [ http://github.com/haskell/cabal.git](http://github.com/haskell/cabal.git)
- synced up cleanly to final 1.18.0 release [\[eb304bd9/ghc\]](/trac/ghc/changeset/eb304bd9/ghc)
- synced up cleanly to 1.18.1 snapshot [\[fccb5c65/ghc\]](/trac/ghc/changeset/fccb5c65/ghc)
- Probably, a 1.18.1 release will occur shortly after Sep 15<sup>th</sup>.

### Win32

- upstream: [ http://github.com/haskell/win32.git](http://github.com/haskell/win32.git)
- **TODO**

### binary

- upstream: [ http://github.com/kolmodin/binary.git](http://github.com/kolmodin/binary.git)
- synced up cleanly to binary-0.7.1.0 \[25f1bda/ghc\]

### bytestring

- upstream: [ http://github.com/haskell/bytestring.git](http://github.com/haskell/bytestring.git)
- unclean state; new bytestring release for 7.8.1 being worked on by simon and duncan
- **TODO**

### containers

- upstream: [ http://github.com/haskell/containers.git](http://github.com/haskell/containers.git)
- synced up cleanly to containers-0.5.3.1 release \[8a8cfb2/ghc\]
- AMP-warning fix pending
- **TODO**

### haskeline<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/haskeline.git](http://git.haskell.org/darcs-mirrors/haskeline.git)
- unclean state
- contacted maintainer on 31.8. wrt to unmerged patches; will merge as soon as his dev machine has been repaired
- **TODO**

### pretty

- upstream: [ http://github.com/haskell/pretty.git](http://github.com/haskell/pretty.git)
- not pointing to proper release version
- no significant delta to upstream HEAD state
- **TODO**

### primitive<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/primitive.git](http://git.haskell.org/darcs-mirrors/primitive.git)
- **critical** major release behind hackage/platform version
- Jan is working on patches to integrate the new bool primops
- **TODO**

### terminfo<sup>1</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/terminfo.git](http://git.haskell.org/darcs-mirrors/terminfo.git)
- **TODO**

### time

- upstream: [ http://git.haskell.org/darcs-mirrors/time.git](http://git.haskell.org/darcs-mirrors/time.git)
- synced up cleanly to time-1.4.1 release \[d55a4f3/ghc\]

### transformers

- upstream: [ http://git.haskell.org/darcs-mirrors/transformers.git](http://git.haskell.org/darcs-mirrors/transformers.git)
- Newly added in GHC 7.8
- still at 0.3.0.0 since GHC 7.6.3
- asked upstream; shall remain at 0.3.0.0 for GHC 7.8.1

### vector<sup>2</sup>

- upstream: [ http://git.haskell.org/darcs-mirrors/vector.git](http://git.haskell.org/darcs-mirrors/vector.git)
- **critical** major release behind hackage/platform version
- coupled with `primitive` library; see notes there
- **TODO**

### xhtml<sup>1</sup>

- upstream: [ http://github.com/haskell/xhtml](http://github.com/haskell/xhtml)
- unmodified at clean 3000.2.1 release since GHC 7.6.3 release
