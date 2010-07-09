# Setting up a Linux system for building GHC


If you're on a recent Linux system, then you should be able to get a working build environment by installing the following packages using your system's package manager.

## Fedora

- `glibc-devel`
- `libedit-devel` (6.10 only)
- `ncurses-devel`
- `gmp-devel`
- `autoconf`
- `automake`
- `libtool`
- `gcc`
- `make`
- `perl`
- `python`
- `ghc`
- `happy`
- `alex`
- `darcs`


For building the documentation: (User's Guide and Cabal guide):

- `docbook-utils`
- `docbook-utils-pdf`
- `docbook-style-xsl`


other packages that are useful for development:

- `strace`
- `patch`
- `libcurl-devel` and `zlib-devel` (for building darcs)

## Debian-based systems (e.g. Debian, Ubuntu)

- `libc6-dev`
- `libedit-dev` (6.10 only)
- `libncurses5-dev`
- `libgmp3-dev`
- `autoconf`
- `automake`
- `libtool`
- `gcc`
- `make`
- `perl`
- `python`
- `ghc6`
- `happy`
- `alex`
- `darcs`
- `libffi-dev`


For building the documentataion: (User's Guide and Cabal guide):

- `docbook-utils`
- `docbook-utils-pdf`
- `docbook-style-xsl`


other packages that are useful for development:

- `strace`
- `patch`
- `libcurl-dev` and `zlib-dev` (for building darcs)
