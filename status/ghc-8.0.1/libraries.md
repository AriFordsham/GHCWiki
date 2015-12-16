# GHC 8.0.1 Library Status

### For GHC 8.0.1 RC1


A package bundled with the upcoming GHC 8.0.1 RC1 (and exposed via `ghc-pkg list`) shall either

- have its submodule point to the commit matching an existing release on Hackage, *or*
- point to the commit representing the unreleased release-candidate state planned for inclusion in GHC 8.0.1


In the latter case, the unreleased candidate's advertised package version number shall be distinct from any officially released package version available on Hackage, and have the appropriate version bump according to the PVP (i.e. patch-level, minor, or major) relative to previous releases.

### For GHC 8.0.1 final


All `ghc-pkg`-exposed packages must match their officially released Hackage release.

#### Git tagging the release


For tagging the release in Git, **annotated Git tags** shall be used, e.g.

```wiki
git tag -a -s v1.2.3.4 -m "mypackage 1.2.3.4"
```


Leave off the `-s` flag if you don't have a GPG-key to sign the tag with.


If you leave off the `-m`-argument, a text-editor will be started to allow you to compose a "tag message" (it's the equivalent of a commit message for tag objects).


Here's [ an example](https://git.haskell.org/packages/deepseq.git/tag/c32a156c8dafaea05e91563afe2f72ad3590f57b) of a signed Git tag object.

## 3rd Party Packages needing a release (candidate)


GHC HQ controlled `base`/`array`/`integer-gmp`/`template-haskell`/etc. are not listed here

### `Cabal`

[](http://hackage.haskell.org/package/Cabal)

TODO

### `binary`

[](http://hackage.haskell.org/package/binary)

TODO`0.8.0.0` is pending to be released. Kolmodin hopes to have it released in time for GHC 8.0.1RC1.

### `bytestring`

[](http://hackage.haskell.org/package/bytestring)

TODO

### `containers`

[](http://hackage.haskell.org/package/containers)

TODO

### `deepseq`

[](http://hackage.haskell.org/package/deepseq)

TODO

### `directory`

[](http://hackage.haskell.org/package/directory)

TODO

### `filepath`

[](http://hackage.haskell.org/package/filepath)

TODO

### `haskeline`

[](http://hackage.haskell.org/package/haskeline)

TODO

### `hoopl`

[](http://hackage.haskell.org/package/hoopl)

TODO

### `hpc`

[](http://hackage.haskell.org/package/hpc)

TODO

### `pretty`

[](http://hackage.haskell.org/package/pretty)

DONE The `1.1.3.2` release should be used.

### `process`

[](http://hackage.haskell.org/package/process)

DONE The existing v1.4.1.0 release shall be used for GHC 8.0.1 according to Michael

### `terminfo`

[](http://hackage.haskell.org/package/terminfo)

TODO

### `time`

[](http://hackage.haskell.org/package/time)

TODO

### `transformers`

[](http://hackage.haskell.org/package/transformers)

TODO The upcoming `transformers-0.5.0.0` version is intended to be used for GHC 8.0; only blocker so far is a pending decision on [\#11135](https://gitlab.haskell.org//ghc/ghc/issues/11135)

### `unix`

[](http://hackage.haskell.org/package/unix)

TODO

### `xhtml`

[](http://hackage.haskell.org/package/xhtml)

TODO

## What's currently in GHC HEAD


As of **7.11.20151214**, `ghc-pkg list` reports (on Linux, i.e. `Win32` is missing):

```wiki
    Cabal-1.23.0.0
    array-0.5.1.0
    base-4.9.0.0
    binary-0.7.5.0
    bytestring-0.10.7.0
    containers-0.5.6.3
    deepseq-1.4.2.0
    directory-1.2.5.0
    filepath-1.4.1.0
    ghc-7.11.20151214
    ghc-boot-0.0.0.0
    ghc-prim-0.5.0.0
    haskeline-0.7.2.1
    hoopl-3.10.2.0
    hpc-0.6.0.2
    integer-gmp-1.0.0.0
    pretty-1.1.2.0
    process-1.4.1.0
    rts-1.0
    template-haskell-2.11.0.0
    terminfo-0.4.0.1
    time-1.5.0.1
    transformers-0.4.3.0
    unix-2.7.1.1
    xhtml-3000.2.1
```


and `git submodule status` says

```wiki
 d602f63e7daf426514e38492bfdeeb4f33bd361d libraries/Cabal (Cabal-1.22.0.0-release-687-gd602f63)
 3b573ee058560d1199a19efab10c016278dff252 libraries/Win32 (Win32-2.3.0.2-release-19-g3b573ee)
 4b43c95af80ed7e1567244527e5e459912d3e504 libraries/array (v0.5.1.0-7-g4b43c95)
 86e4c9a6125cdddb0592a653f48c699a574ccf7b libraries/binary (binary-0.7.5.0-release)
 84d041649c39e7dc0fe8d348da10d6ed1679a8f9 libraries/bytestring (0.10.6.0-38-g84d0416)
 6405653480afa675eec804616547b8625244bc7c libraries/containers (containers-0.5.6.3-release-29-g6405653)
 40d4db0a4e81a07ecd0f1bc77b8772088e75e478 libraries/deepseq (v1.4.1.1-15-g40d4db0)
 298529bf8adc38ed602eab300c63bbc68510e5a3 libraries/directory (v1.2.2.0-93-g298529b)
 33eb2fb7e178c18f2afd0d537d791d021ff75231 libraries/dph (2009-06-25-1149-g33eb2fb)
 564c5a78d028ebde6a27f61ff9f686fce490c809 libraries/filepath (v1.4.0.0-12-g564c5a7)
 06349b7ffc503ba0c8bdf0293d1adc6026da97bd libraries/haskeline (0.7.2.1-11-g06349b7)
 b4f47611084f9e22aeecb4d49659e900848e157e libraries/hoopl (hoopl-3.10.0.2-release-43-gb4f4761)
 5123582f48b46efc3d27424bc475125a1de78e2e libraries/hpc (v0.6.0.2-9-g5123582)
 ec04d059b13fc348789d87adfbabb9351f8574db libraries/parallel (v3.2.0.6-13-gec04d05)
 7eb7c6c01be4596da3dae9ca57d8adac37cc33fc libraries/pretty (pretty-1.1.2.0-release)
 83d3d23d2fa1583ecd1b59464cc889924e1b5fff libraries/primitive (primitive-0.5.2.1-release-57-g83d3d23)
 e594712a8fe49c94ff43ab016739e0fa63f0de00 libraries/process (v1.4.0.0-23-ge594712)
 cfdfe6f09ad414fde5b855cc5f90207533413241 libraries/random (random-1.0.1.1-release-44-gcfdfe6f)
 9870cf156e5e7e21785b236da41f2466bf9f4b29 libraries/stm (stm-2.4.4-release-5-g9870cf1)
 15ca1cb3b1e1b0d410544abd69e7ffcf727fc970 libraries/terminfo (0.4.0.1-4-g15ca1cb)
 8d3c90a420c8985dcc439766c028821cea7dc848 libraries/time (time-1.5.0.1-release)
 34fba39b1279936a739ca8857e9592cc9a44c34e libraries/transformers (0_4_3_0-33-g34fba39)
 59edb0a0a0d91ecfe938029b3b00a0c99dcb8481 libraries/unix (v2.7.1.0-13-g59edb0a)
 6c17dd6fadc5e7e3e09f7892380ce1339f296efd libraries/vector (0_7-245-g6c17dd6)
 fb9e0bbb69e15873682a9f25d39652099a3ccac1 libraries/xhtml (3000.2.1)
```


The `git describe`-version reported in brackets (e.g. `(v0.5.1.0-7-g4b43c95)`) tells us whether a commit points to an annotated Git tag (and thus most likely corresponds to a released package version). Version descriptions with ah `-g[0-9a-f]+` suffix denote commits in between Git tags!
