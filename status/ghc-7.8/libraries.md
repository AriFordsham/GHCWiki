# Status of GHC 7.8 Boot Libraries

## 3rd Party Libraries

### Cabal

- upstream: [ http://github.com/haskell/cabal.git](http://github.com/haskell/cabal.git)
- synced up cleanly to final 1.18.0 release [\[eb304bd9/ghc\]](/trac/ghc/changeset/eb304bd9/ghc)
- Probably, a 1.18.1 release will be made during the 7.8.1 RC phase

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

### haskeline

- upstream: [ http://git.haskell.org/darcs-mirrors/haskeline.git](http://git.haskell.org/darcs-mirrors/haskeline.git)
- unclean state
- contacted maintainer on 31.8. wrt to unmerged patches; will merge as soon as his dev machine has been repaired
- **TODO**

### pretty

- upstream: [ http://github.com/haskell/pretty.git](http://github.com/haskell/pretty.git)
- **TODO**

### primitive

- upstream: [ http://git.haskell.org/darcs-mirrors/primitive.git](http://git.haskell.org/darcs-mirrors/primitive.git)
- **critical** major release behind hackage/platform version
- Jan is working on patches to integrate the new bool primops
- **TODO**

### terminfo

- upstream: [ http://git.haskell.org/darcs-mirrors/terminfo.git](http://git.haskell.org/darcs-mirrors/terminfo.git)
- **TODO**

### time

- upstream: [ http://git.haskell.org/darcs-mirrors/time.git](http://git.haskell.org/darcs-mirrors/time.git)
- synced up cleanly to time-1.4.1 release \[d55a4f3/ghc\]

### transformers

- upstream: [ http://git.haskell.org/darcs-mirrors/transformers.git](http://git.haskell.org/darcs-mirrors/transformers.git)
- still at 0.3.0.0 since GHC 7.6.3
- asked upstream; shall remain at 0.3.0.0 for GHC 7.8.1

### vector

- upstream: [ http://git.haskell.org/darcs-mirrors/vector.git](http://git.haskell.org/darcs-mirrors/vector.git)
- **critical** major release behind hackage/platform version
- coupled with `primitive` library; see notes there
- **TODO**

### xhtml

- upstream: [ http://github.com/haskell/xhtml](http://github.com/haskell/xhtml)
- **TODO**