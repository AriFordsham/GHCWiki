# Setting Up a FreeBSD system for Building GHC


Building GHC on FreeBSD is currently supported on 8.1-RELEASE or later, on `i386` (x86) and `amd64` (x86_64) architectures.  One might be able to build GHC on different architectures and earlier versions but they are not maintained actively.  Note that 8.1-RELEASE is used for the FreeBSD nightly builds ([ amd64 head](http://darcs.haskell.org/ghcBuilder/builders/pgj/), [ i386 head](http://darcs.haskell.org/ghcBuilder/builders/pgj2/), [ amd64 stable](http://darcs.haskell.org/ghcBuilder/builders/pgj-freebsd-amd64-stable/), [ i386 stable](http://darcs.haskell.org/ghcBuilder/builders/pgj-freebsd-i386-stable/)).

## Required Ports


In order to be able to build GHC from source, the following ports have to be installed.

- `converters/libiconv` (GNU libiconv)
- `devel/autoconf` (GNU Autoconf)
- `devel/git` (for getting and managing the GHC sources)
- `devel/gmake` (GNU Make)
- `devel/hs-alex` (alex)
- `devel/hs-happy` (happy)
- `ftp/curl` (HTTP support for git)
- `lang/ghc` (bootstrap compiler, usually tracking Haskell Platform specifications)
- `lang/perl5.14` (Perl)
- `lang/python27` (Python, for the test suite)
- `math/gmp` (GNU GMP)


Probably it is possible to use a [vanilla binary distribution](http://www.haskell.org/ghc/download_ghc_7_6_2#freebsd) to bootstrap the build, but one must note that it is built on 8.1-RELEASE, hence it will require installing `misc/compat8x` in order to make it work on 9.x and later systems.

## Optional Ports


In addition to the required ones, there are some further ports to install for extras.

- `devel/binutils` (for a more up-to-date version of the GNU toolchain, see Notes)
- `devel/libffi` (for dynamic linking)
- `devel/llvm` (for LLVM code generation)
- `devel/hs-hasktags` (for generating HTAGS files)
- `devel/ncurses` (for a more up-to-date version for the `terminfo` library, see Notes)
- `lang/gcc` (for GCC 4.6 or later, see Notes)
- `print/hs-hscolour` (for HsColourized Haddock documentation)
- `textproc/dblatex` (for PDF documentation)
- `textproc/docbook-xsl` (for XML documentation)
- `textproc/libxslt` (for XML documentation)

## Running `configure`


FreeBSD stores locally installed third-party software (i.e. the ports mentioned above) under a separate prefix, called `LOCALBASE` here, which is `/usr/local/` by default.  Thus the preferred way of invoking the `configure` script is as follows.

```wiki
$ ./configure \
  --with-iconv-includes=$LOCALBASE/include --with-iconv-libraries=$LOCALBASE/lib \
  --with-gmp-includes=$LOCALBASE/include   --with-gmp-libraries=$LOCALBASE/lib
```


To use a more recent GCC (this is `gcc46` here) and GNU toolchain from the Ports Collection, it should be added as well:

```wiki
  --with-gcc=$LOCALBASE/bin/gcc46 --with-gcc-4.2=$LOCALBASE/bin/gcc46 \
  --with-ld=$LOCALBASE/bin/ld --with-ar=$LOCALBASE/bin/ar \
  --with-ranlib=$LOCALBASE/bin/ranlib
```


To use `libffi` from the Ports Collection:

```wiki
  --with-system-libffi \
  --with-ffi-includes=$LOCALBASE/include --with-ffi-libraries=$LOCALBASE/lib
```

## Building the Sources


After `configure` ran successfully, invoke GNU make as usual.  Note that GNU make is called `gmake` under FreeBSD as `make` is the BSD make.

```wiki
$ gmake
```

## Notes


Here is a random list of thoughts about things that are good to know when working on FreeBSD.

- binutils / gcc
- ncurses
- gmp
- tmpfs
