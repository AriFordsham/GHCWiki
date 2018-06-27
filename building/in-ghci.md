
\# Loading GHC into GHCi


This page says how you can load GHC into GHCi for more iterative development. Csongor Kiss was the first person to record this feat. 

1. Put this .ghci file in compiler/

```wiki
:set -ibackpack
:set -ibasicTypes
:set -icmm
:set -icodeGen
:set -icoreSyn
:set -ideSugar
:set -ighci
:set -ihsSyn
:set -iiface
:set -illvmGen
:set -imain
:set -inativeGen
:set -iparser
:set -iprelude
:set -iprofiling
:set -irename
:set -isimplCore
:set -isimplStg
:set -ispecialise
:set -istgSyn
:set -istranal
:set -itypecheck
:set -itypes
:set -iutils
:set -ivectorise
:set -I../compiler
:set -I../compiler/stage2
:set -I../compiler/stage2/build
:set -i../compiler/stage2/build
:set -I../includes
:set -I../includes/dist-derivedconstants/header
:set -package=ghc-boot-th
:set -fobject-code
:set -DSTAGE=2
:set -XNoImplicitPrelude
:load GHC
```

1. Run `../inplace/bin/ghc-stage2 --interactive -odir tmp -hidir tmp -j<n> +RTS -A128m` where \<n\> is the number of cores on your machine


from inside compiler/


It may take a while and require a little bit of memory but in the end
all 500 or so modules will be loaded.


It can also be used with ghcid.

```wiki
ghcid -c "../inplace/bin/ghc-stage2 --interactive -odir tmp -hidir tmp -j4 +RTS-A128m"
```