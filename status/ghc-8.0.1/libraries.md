# GHC 8.0.1 Library Status


For general guidelines regarding boot library maintenance see [WorkingConventions/BootLibraries](working-conventions/boot-libraries).

### For GHC 8.0.1 RC1


A package bundled with the upcoming GHC 8.0.1 RC1 (and exposed via `ghc-pkg list`) shall either

- have its submodule point to the commit matching an existing release on Hackage, *or*
- point to the commit representing the unreleased release-candidate state planned for inclusion in GHC 8.0.1


In the latter case, the unreleased candidate's advertised package version number shall be distinct from any officially released package version available on Hackage, and have the appropriate version bump according to the PVP (i.e. patch-level, minor, or major) relative to previous releases.

### For GHC 8.0.1 final


All `ghc-pkg`-exposed packages must match their officially released Hackage release.

## 3rd Party Packages needing a release (candidate)



GHC HQ controlled `base`/`array`/`integer-gmp`/`template-haskell`/etc. are not listed here


### `Cabal`



http://hackage.haskell.org/package/Cabal



TODO The `1.24` branch has been created, please use it for GHC 8 RC3. Cabal `1.24.0.0` final will be released at some point between GHC 8 RC3 and GHC 8 final.


### `binary`



http://hackage.haskell.org/package/binary



TODO `0.8.2.1` has just been released, please use that for RC3. GHC 8.0.1 final should ship the `0.8.2.2` release. Biggest change is that includes a Binary instance for `ShortByteString`.


### `bytestring`



http://hackage.haskell.org/package/bytestring



TODO most likely: RC1 will ship with the odd (unreleased) version `0.10.7.0`, whereas GHC 8 final will ship with the upcoming `0.10.8.0` release


### `containers`



http://hackage.haskell.org/package/containers



DONE Freshly released `0.5.7.1` should be used for GHC 8 (instead of previously announced `0.5.7.0`).


### `deepseq`



http://hackage.haskell.org/package/deepseq



TODO the upcoming `deepseq-1.4.2.0` release will be used for GHC 8


### `directory`



http://hackage.haskell.org/package/directory



DONE `directory-1.2.6.2` should be used.


### `filepath`

http://hackage.haskell.org/package/filepath

DONE released `filepath-1.4.1.0` shall be used until further notice

### `haskeline`

http://hackage.haskell.org/package/haskeline

DONE The release `0.7.2.2` should be used.

### `hoopl`

http://hackage.haskell.org/package/hoopl

DONE The 3.10.2.1 release shall be used

### `hpc`

http://hackage.haskell.org/package/hpc

TODO (hvr) upload `hpc-0.6.0.3` release to Hackage

### `pretty`

http://hackage.haskell.org/package/pretty

DONE The `1.1.3.3` release should be used.

### `process`

http://hackage.haskell.org/package/process

DONE The existing v1.4.1.0 release shall be used for GHC 8.0.1 according to Michael

### `terminfo`

http://hackage.haskell.org/package/terminfo

DONE The release `0.4.0.2` should be used.

### `time`

http://hackage.haskell.org/package/time

DONE RC1: Use released `1.6`.

TODO RC2/FINAL: Use yet to be released `1.6.0.1` containing minor tweaks

### `transformers`

http://hackage.haskell.org/package/transformers

DONE The tagged `transformers-0.5.0.0` version is intended to be used for GHC 8.0

### `unix`

http://hackage.haskell.org/package/unix

TODO current plan: use unreleased patch-level v2.7.1.1 containing only fixes but no API changes; release 2.7.1.1 with GHC8 RC2

### `Win32`

http://hackage.haskell.org/package/Win32

DONE The tagged `v2.3.1.1` version is to be used for GHC 8.0.1

### `xhtml`

http://hackage.haskell.org/package/xhtml

DONE use existing `xhtml-3000.2.1` release

TODO (currently points to released version -- no need to act unless maintainer wishes to)

## What's currently in GHC HEAD


As of **7.11.20151216**, `ghc-pkg list` reports (on Linux, i.e. `Win32` is missing):

```wiki
    Cabal-1.23.0.0
    array-0.5.1.0
    base-4.9.0.0
    binary-0.8.0.0
    bytestring-0.10.7.0
    containers-0.5.6.3
    deepseq-1.4.2.0
    directory-1.2.5.0
    filepath-1.4.1.0
    ghc-7.11.20151216
    ghc-boot-0.0.0.0
    ghc-prim-0.5.0.0
    haskeline-0.7.2.1
    hoopl-3.10.2.0
    hpc-0.6.0.2
    integer-gmp-1.0.0.0
    pretty-1.1.3.2
    process-1.4.1.0
    rts-1.0
    template-haskell-2.11.0.0
    terminfo-0.4.0.1
    time-1.5.0.1
    transformers-0.5.0.0
    unix-2.7.1.1
    xhtml-3000.2.1
```


and `git submodule status` says

```wiki
 cb855f34676ccdc2b0967a60ffe90b60ffeb1816 Cabal (Cabal-1.22.0.0-release-1113-gcb855f3)
 fec966e6d77a5e7f4a586de6096954137a1fe914 Win32 (Win32-2.3.1.0-release-1-gfec966e)
 6551ad9edaca1634a8149ad9c27a72feb456d4e1 array (v0.5.1.0-9-g6551ad9)
 69915d0a26ae9eaa6b34367989ee8ed356ed13bb binary (0.8.2.1-9-g69915d0)
 3d6d0f60ac25736cc87a6f598886fe77e7b6ad90 bytestring (0.10.6.0-42-g3d6d0f6)
 d08e47bf6895da8c8b5a7dd62496a2f4fe73631e containers (v0.5.7.1)
 40d4db0a4e81a07ecd0f1bc77b8772088e75e478 deepseq (v1.4.1.1-15-g40d4db0)
 33ce1ca6bef97b60957e4763b046eac9a982ead0 directory (v1.2.5.1)
 33eb2fb7e178c18f2afd0d537d791d021ff75231 dph (2009-06-25-1149-g33eb2fb)
 f510e50feefe9995334769dd5e26c79edbe6fdc1 filepath (v1.4.1.0)
 5e53651b2683f31bf5efc842c33f07afc05ec287 haskeline (0.7.2.2-7-g5e53651)
 b4477e825a93373124ec5cf29b9850df9608f5bd hoopl (v3.10.2.1)
 7cead32fc1f9929d69861d1b09e710bd6d374363 hpc (v0.6.0.2-11-g7cead32)
 dbb8d9e36276f1ced95c8761ddbc3b157742bd34 parallel (v3.2.1.0)
 56bc78e2c2cfcc850f6fec87fe79743750d4c8b4 pretty (v1.1.3.3)
 1af89788d5c9ab7a0a93ff6764e1770e6c80d957 primitive (primitive-0.5.2.1-release-58-g1af8978)
 296cbce6294316d6534b4449fc7ab0f0d3f5775b process (v1.4.1.0-9-g296cbce)
 cfdfe6f09ad414fde5b855cc5f90207533413241 random (v1.1)
 844f84c21f94282187f35a6684d3c3c9f32cf2df stm (v2.4.4.1-1-g844f84c)
 140ca44db6fc734cfc0388e82f9e5270f31475d8 terminfo (0.4.0.2)
 a73564c366b15f7057b614188662d7b7a8eaab19 time (time-1.6-release-6-ga73564c)
 10348c4bbf60debbfc82463e1035aca1cb7b51bc transformers (0_5_2_0)
 ff1c16d4ee0c4ca043bd99a5d6741ea2d53e7000 unix (v2.7.1.0-50-gff1c16d)
 6c17dd6fadc5e7e3e09f7892380ce1339f296efd vector (0_7-245-g6c17dd6)
 fb9e0bbb69e15873682a9f25d39652099a3ccac1 xhtml (3000.2.1)

```


The `git describe`-version reported in brackets (e.g. `(v0.5.1.0-7-g4b43c95)`) tells us whether a commit points to an annotated Git tag (and thus most likely corresponds to a released package version). Version descriptions with ah `-g[0-9a-f]+` suffix denote commits in between Git tags!
