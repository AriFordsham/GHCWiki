# Setting Up a FreeBSD System for Building GHC


Building GHC on FreeBSD is currently supported on 8.1-RELEASE or later, on `i386` (x86) and `amd64` (x86_64) architectures.  One might be able to build GHC on different architectures and earlier versions but they are not maintained actively.  Note that 8.1-RELEASE is used for the FreeBSD nightly builds ([ amd64 head](http://darcs.haskell.org/ghcBuilder/builders/pgj/), [ i386 head](http://darcs.haskell.org/ghcBuilder/builders/pgj2/), [ amd64 stable](http://darcs.haskell.org/ghcBuilder/builders/pgj-freebsd-amd64-stable/), [ i386 stable](http://darcs.haskell.org/ghcBuilder/builders/pgj-freebsd-i386-stable/)).

**Note that this section is primarily intended for developers and early adopters.***If you are just want to install GHC on your system, simply use the [ devel/hs-haskell-platform](http://www.freshports.org/devel/hs-haskell-platform) port instead.  This port does all the things described below for you.  It is usually kept updated to match the latest Haskell Platform specifications.*

## Required Ports


In order to be able to build GHC from source, the following ports have to be installed.

- [ converters/libiconv](http://www.freshports.org/converters/libiconv/) (GNU libiconv)
- [ devel/autoconf](http://www.freshports.org/devel/autoconf) (GNU Autoconf)
- [ devel/git](http://www.freshports.org/devel/git) (for getting and managing the GHC sources)
- [ devel/gmake](http://www.freshports.org/devel/gmake) (GNU Make)
- [ devel/hs-alex](http://www.freshports.org/devel/hs-alex) (alex)
- [ devel/hs-happy](http://www.freshports.org/devel/hs-happy) (happy)
- [ ftp/curl](http://www.freshports.org/ftp/curl) (HTTP support for git)
- [ lang/ghc](http://www.freshports.org/lang/ghc) (bootstrap compiler, usually tracking Haskell Platform specifications)
- [ lang/perl5.14](http://www.freshports.org/lang/perl5.14) (Perl)
- [ lang/python27](http://www.freshports.org/lang/python27) (Python, for the test suite)
- [ math/gmp](http://www.freshports.org/math/gmp) (GNU GMP)


Probably it is possible to use a [vanilla binary distribution](http://www.haskell.org/ghc/download_ghc_7_6_2#freebsd) to bootstrap the build, but one must note that it is built on 8.1-RELEASE, hence it will require installing [ misc/compat8x](http://www.freshports.org/misc/compat8x) in order to make it work on 9.x and later systems.

## Optional Ports


In addition to the required ones, there are some further ports to install for extras.

- [ devel/binutils](http://www.freshports.org/devel/binutils) (for a more up-to-date version of the GNU toolchain, see Notes)
- [ devel/libffi](http://www.freshports.org/devel/libffi) (for dynamic linking)
- [ devel/llvm](http://www.freshports.org/devel/llvm) (for LLVM code generation)
- [ devel/hs-haskell-platform](http://www.freshports.org/devel/hs-haskell-platform) (the complete Haskell Platform)
- [ devel/hs-hasktags](http://www.freshports.org/devel/hs-hasktags) (for generating HTAGS files)
- [ devel/ncurses](http://www.freshports.org/devel/ncurses) (for a more up-to-date version for the `terminfo` library, see Notes)
- [ lang/gcc](http://www.freshports.org/lang/gcc) (for GCC 4.6 or later, see Notes)
- [ print/hs-hscolour](http://www.freshports.org/print/hs-hscolour) (for HsColourized Haddock documentation)
- [ textproc/dblatex](http://www.freshports.org/textproc/dblatex) (for PDF documentation)
- [ textproc/docbook-xsl](http://www.freshports.org/textproc/docbook-xsl) (for XML documentation)
- [ textproc/libxslt](http://www.freshports.org/textproc/libxslt) (for XML documentation)

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

- The FreeBSD base system contains GCC and the GNU toolchain (at least for the time being) but they are not or only slowly updated.  GCC is technically stuck at version 4.2.1 which may not be optimal for building GHC these days.  Hence it is highly recommended to use the toolchain ([ devel/binutil](http://www.freshports.org/devel/binutils)) and GCC ([ lang/gcc](http://www.freshports.org/lang/gcc)) from the Ports Collection instead.

- The FreeBSD base system is shipped with a version of `ncurses` but this may not be the latest.  Unfortunately, when [ devel/ncurses](http://www.freshports.org/devel/ncurses) is installed one should add some extra lines to `mk/build.mk` to tell GNU make we want to use `ncurses` from `$LOCALBASE` (see above) instead, otherwise `terminfo` (which uses `ncurses`) becomes linked to `ncurses` in the base:

```wiki
SRC_HC_OPTS += -I$LOCALBASE/include -L$LOCALBASE/lib
libraries/terminfo_CONFIGURE_OPTS += --configure-option=--with-curses-includes=$LOCALBASE/include --configure-option=--with-curses-libraries=$LOCALBASE/lib
```

>
> See [\#7472](https://gitlab.haskell.org//ghc/ghc/issues/7472) for possible symptoms.

- The GHC source code have an in-tree version of `libffi` and `gmp` which may work by accident -- especially if the version of `libgmp.so` and `libffi.so` matches the version installed by the ports.  But using them is not recommended as they could result in various strange build and run-time errors.  See the `configure` options to work around them.

- In an extremely high-speed environment it might happen that `gmake` restarts multiple times, c.f. [\#7592](https://gitlab.haskell.org//ghc/ghc/issues/7592).  This is because timestamp precision for the VFS layer is set to a POSIX-friendly value by default. Adjusting the `vfs.timestamp_precision` sysctl(3) variable can help.

- Building GHC sources and Haskell sources in general could be sped up by setting up a [ tmpfs(5)](http://www.freebsd.org/cgi/man.cgi?query=tmpfs&apropos=0&sektion=0&manpath=FreeBSD+9.1-stable&arch=default&format=html) partition.  (This is not created by the default install.)  Just replace the `/tmp` partition with a tmpfs-backed entry in `/etc/fstab`:

```wiki
tmpfs /tmp tmpfs rw,mode=777 0 0
```

>
> and use your original `/tmp` partition as a swap device (substitute `ada0s1` with your device):

```wiki
/dev/ada0s1d none swap sw 0 0
```