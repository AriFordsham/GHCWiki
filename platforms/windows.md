
This page is aimed at developers building GHC on the Windows platform (on x86 hardware).  Please add to this page.

# Building GHC on Windows


First places to look is the Building Guide.  Specifically:

- [Stuff about Cyginw and MSYS](http://www.haskell.org/ghc/docs/latest/html/building/platforms.html#cygwin-and-mingw).  GHC builds many, many times faster if you use MSYS than Cygwin.
- [Instructions for building under Windows](http://www.haskell.org/ghc/docs/latest/html/building/winbuild.html)

## Common problems


If you get a message like this:

```wiki
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.

Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.

autoreconf: /usr/bin/autoconf failed with exit status: 1
```


then you have probably not got `automake` installed (or at least findable).
