# Setting up a Linux system for building GHC


If you're on a recent Linux system, then you should be able to get a working build environment by installing the following packages using your system's package manager:

- `glibc-devel`
- `libedit-devel`
- `ncurses-devel`
- `gmp-devel`
- `autoconf`
- `automake`
- `libtool`
- `gcc`
- `make`
- `perl`
- `python` (only needed for the testsuite)
- `ghc` (recent stable version of ghc, not a development version)
- `happy`
- `alex`


To be able to build the documentation (User's Guide and Cabal guide):

- `docbook-utils`
- `docbook-utils-pdf`
- `docbook-style-xsl`


other packages that are useful for development:

- `strace`
- `patch`
- `libcurl-devel` and `zlib-devel` (for building darcs)
