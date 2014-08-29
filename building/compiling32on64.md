# Compiling for 32 bits on 64 bits


This is cross-compilation-light. It doesn't involve building GHC with different host and target, but does involve running 
a GHC with i386 target (and host) on a by-default x86_64 OS. This should be possible on any OS that has multi-arch capabilities,
but the instructions will be only for Ubuntu (tested on Ubuntu precise, LTS 12.04.4). Some of the problems are similar
as when really cross-compiling. See [\#9421](https://gitlab.haskell.org//ghc/ghc/issues/9421) for some more context and common problems.

TODO