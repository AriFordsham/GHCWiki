**Known issues**:

- Mavericks suffers from some preprocessing bugs. We're going to try using `cpphs` as an alternative.
- The Linux binary builds require glibc 2.15 (Ubuntu 12.04.) RC2 will use glibc 2.13 (Debian 7/stable) instead.
- [hackage:cabal-install-1.18.0.2](http://hackage.haskell.org/package/cabal-install-1.18.0.2) is not build-able with 7.8.1; if you want to build `cabal-install` with RC1 you need check out latest version from the [ Cabal 1.18 branch](https://github.com/haskell/cabal/tree/1.18) until [ hackage:cabal-install-1.18.0.3](http://hackage.haskell.org/package/cabal-install-1.18.0.3) is released.

## Tickets scheduled for 7.8.1

See the %7.8.1 milestone.

Note however, that not all tickets with a 7.8.1 milestone will be addressed for the final 7.8.1 release.


