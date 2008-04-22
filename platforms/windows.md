
This page is aimed at developers building GHC on the Windows platform (on x86 hardware).  Please add to this page.

# Versions supported


We no longer support Windows 95, 98, ME or NT. We do support 2000, XP and Vista. We do not support 64-bit windows operating systems ([\#1884](https://gitlab.haskell.org//ghc/ghc/issues/1884)).

# Building GHC on Windows


First places to look is the Building Guide.  Specifically:

- [Stuff about Cyginw and MSYS](http://www.haskell.org/ghc/docs/latest/html/building/platforms.html#cygwin-and-mingw).  GHC builds many, many times faster if you use MSYS than Cygwin.
- [ Instructions for building under Windows](http://hackage.haskell.org/trac/ghc/wiki/Building/Windows)

## Common problems


If you get a message like this:

```wiki
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
autoreconf: /usr/bin/autoconf failed with exit status: 1
```


then you have probably not got `automake` installed (or at least findable).
