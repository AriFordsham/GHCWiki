## **Setting up a [OpenIndiana](building/preparation/open-indiana) system for building GHC**

>
> These instructions have only been checked for GHC 7.6.3 on [OpenIndiana](building/preparation/open-indiana) 151a8. They should also apply to earlier and later versions of GHC. 

## **Installing pre-compiled GHC 7.2.2 binary package **


In order to build GHC on [OpenIndiana](building/preparation/open-indiana) we'll be using already precompiled version of ghc 7.2.2 from sfe repository

```wiki
pfexec pkg install runtime/ghc 
```

## *Configuring system for building*


To configure system for building we need to specify correct gmp include directory

```wiki
./configure --with-gmp-includes=/usr/include/gmp
```

## **Bulding GHC**


In order to build GHC you need to use GNU's gmake command instead of [OpenIndiana](building/preparation/open-indiana)'s make which is

```wiki
gmake
```