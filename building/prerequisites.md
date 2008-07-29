## Prerequisites for Buiding GHC


Here are the gory details about some utility programs you may need;
`GHC`, `perl`, `gcc`, `happy` and `alex` are the
only important ones. The `configure` script will tell you if you
are missing something.  If you're on a Linux system, skip to "Preparing a Linux System" below.

<table><tr><th>GHC</th>
<td>
GHC is required to build GHC, because GHC itself is
written in Haskell, and uses GHC extensions.  It is possible
to build GHC using just a C compiler, and indeed some
distributions of GHC do just that, but it isn't the best
supported method, and you may encounter difficulties.  Full
instructions are in [Porting GHC](building/porting).
  
GHC can be built using either an earlier released
version of GHC (currently 6.0 and later are supported), or
bootstrapped using a GHC built from exactly the same
sources.  Note that this means you cannot in general build
GHC using an arbitrary development snapshot, or a build from
say last week.  It might work, it might not - we don't
guarantee anything.  To be on the safe side, start your
build using the most recently released stable version of
GHC.
</td></tr></table>

<table><tr><th>Perl</th>
<td>*You must have Perl to proceed! *
Perl version 5 at least is required.  GHC has been known to
tickle bugs in Perl, so if you find that Perl crashes when
running GHC try updating (or downgrading) your Perl
installation.  Versions of Perl before 5.6 have been known to have
various bugs tickled by GHC, so the configure script
will look for version 5.6 or later.
  
Perl should be put somewhere so that it can be invoked
by the `#!` script-invoking mechanism.
</td></tr></table>

<table><tr><th>GNU C (`gcc`)</th>
<td>
Most GCC versions should work with the most recent GHC
sources.  Expect trouble if you use a recent GCC with
an older GHC, though (trouble in the form of mis-compiled code,
link errors, and errors from the `ghc-asm`
script).

If your GCC dies with "internal error"" on
some GHC source file, please let us know, so we can report
it and get things improved.  (Exception: on x86
boxes, you may need to fiddle with GHC's
`-monly-N-regs` option; see the User's
Guide).
</td></tr></table>

<table><tr><th>GNU Make</th>
<td>
The GHC build system makes heavy use of features
specific to GNU `make`, so you must have
this installed in order to build GHC.

NB. it has been reported that version 3.79 no longer
works to build GHC, and 3.80 is required.
</td></tr></table>

<table><tr><th>[ Happy](http://www.haskell.org/happy)</th>
<td>
Happy is a parser generator tool for Haskell, and is
used to generate GHC's parsers.

If you start from a source tarball of GHC (i.e. not a darcs
checkout), then you don't need Happy, because we supply the
pre-processed versions of the Happy parsers.  If you intend to
modify the compiler and/or you're using a darcs checkout, then you
need Happy.

Happy version 1.15 is currently required to build GHC.
Grab a copy from
[ Happy's Web Page](http://www.haskell.org/happy/).
</td></tr></table>

<table><tr><th>[ Alex](http://www.haskell.org/alex/)</th>
<td>
Alex is a lexical-analyser generator for Haskell,
which GHC uses to generate its lexer.

Like Happy, you don't need Alex if you're building GHC from a
source tarball, but you do need it if you're modifying GHC and/or
building a darcs checkout.

Alex is
written in Haskell and is a project in the darcs repository.
Alex distributions are available from 
[ Alex's Web Page](http://www.haskell.org/alex/).
</td></tr></table>

<table><tr><th>`autoconf` and `automake`</th>
<td>
These are needed if you intend to build from the
darcs sources, they are *not* needed if you
just intend to build a standard source distribution.

Version 2.52 or later of the autoconf package is required.
NB. version 2.13 will no longer work, as of GHC version
6.1.  Version 1.9 of automake is known to work, use others at
your own risk.
`autoreconf` (from the autoconf package)
recursively builds `configure` scripts from
the corresponding `configure.ac` and
`aclocal.m4` files.  If you modify one of
the latter files, you'll need `autoreconf` to
rebuild the corresponding `configure`.
</td></tr></table>

<table><tr><th>`sed`</th>
<td>
Most Unix installations and Cygwin/MSYS on
Windows already come with `sed`, so you're probably OK.
GNU sed version 2.0.4 is no good!  It has a bug
in it that is tickled by the build-configuration.  2.0.5 is
OK. Others are probably OK too (assuming we don't create too
elaborate configure scripts.)
</td></tr></table>

<table><tr><th>`diff`</th>
<td>
Most installations should have this by default, but inexplicably 
Cygwin does not bundle it by default.
</td></tr></table>

<table><tr><th>Python</th>
<td>
Required for [running the testsuite](building/running-tests).
Version 2.5.2 or later is preferred, because you'll get support for
running the testsuite in parallel.
</td></tr></table>

## Preparing a Linux system


If you're on a recent Linux system, then you should be able to get a working build environment by installing the following packages using your system's package manager:

- glibc-devel
- editline-devel
- ncurses-devel
- gmp-devel
- autoconf
- automake
- gcc
- make
- perl
- python
- ghc
- happy
- alex
- haddock

**Note:** currently you'll also need to install an older version of [ Haddock](http://www.haskell.org/haddock) (0.9) manually, because the version of Haddock you'll get by default is 2.0.0.0 which doesn't work with GHC's build system yet.


To be able to build the documentation (User's Guide):

- docbook-utils
- docbook-utils-pdf
- docbook-style-xsl


other packages that are useful for development:

- strace
- libcurl-devel and zlib-devel (for building darcs)
