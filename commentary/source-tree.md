# GHC Source Tree Roadmap


The top level of a GHC source tree looks like this:

### Documentation

<table><tr><th>`ANNOUNCE`</th>
<td></td></tr>
<tr><th>`HACKING`</th>
<td></td></tr>
<tr><th>`LICENSE`</th>
<td></td></tr>
<tr><th>`README`</th>
<td></td></tr></table>

### Configuration


The GNU autoconf machinery:

<table><tr><th>`aclocal.m4`</th>
<td></td></tr>
<tr><th>`config.guess`</th>
<td></td></tr>
<tr><th>`config.sub`</th>
<td></td></tr>
<tr><th>`configure.ac`</th>
<td></td></tr>
<tr><th>`install-sh`</th>
<td></td></tr></table>

### The Build System


See Commentary/BuildSystem?.

<table><tr><th>`Makefile`</th>
<td>
Top-level `Makefile`; `make` by itself does a full 2-stage
bootstrap of GHC, there are also targets for building source and
binary distributions.  GHC requires
[ GNU make](http://www.gnu.org/software/make/).
</td></tr></table>

<table><tr><th>`mk/`</th>
<td>
The guts of the build system itself.
</td></tr></table>

### The Code

<table><tr><th>`compat/`</th>
<td>
A library of compatibility code used when bootstrapping GHC using an
older version of GHC.  For example, we compile up the version of
Cabal from `libraries/Cabal` and include it in `libcompat`,
this means that the GHC source code can assume the most recent
version of Cabal.
</td></tr></table>

<table><tr><th>compiler/?</th>
<td>[The Compiler](commentary/compiler) itself: all Haskell code.
</td></tr></table>

<table><tr><th>`driver/`</th>
<td>
Historically this contained the Perl script known as the GHC
"driver"; in GHC 5.00 the driver was rewritten in Haskell and
incorporated into GHC itself when we added GHCi and `--make`.
This directory still contains the mangler?
and the splitter? Perl scripts, and a couple
of wrappers used to invoke GHC on Windows.  Also the package
database constructed during a GHC build is stored in here.
</td></tr></table>

<table><tr><th>libraries/?</th>
<td>
The libraries that are built and distributed with GHC.
</td></tr></table>

<table><tr><th>[includes/](commentary/source-tree/includes)</th>
<td>
Header files for the Runtime System and for compiling Haskell via C.
</td></tr></table>

<table><tr><th>docs/?</th>
<td>
GHC documentation.
</td></tr></table>

<table><tr><th>`quickcheck/`</th>
<td>
Some quickcheck tests for the compiler (may go away).
</td></tr></table>

<table><tr><th>[rts/](commentary/source-tree/rts)</th>
<td>
The [Runtime System](commentary/rts).
</td></tr></table>

<table><tr><th>utils/?</th>
<td>
Various utility programs, either used during the build itself or
distributed with GHC.
</td></tr></table>


These two are optional, available as separate darcs repositories:

<table><tr><th>testsuite/?</th>
<td>
The test suite.
</td></tr>
<tr><th>nofib/?</th>
<td>
The NoFib benchmark suite (optional separate darcs repo).
</td></tr></table>

### Distribution

<table><tr><th>`darcs-all`</th>
<td>
a script for operating on the collection of darcs
repositories that makes up the GHC source tree (see GettingTheSources).
</td></tr></table>

<table><tr><th>`distrib/`</th>
<td>
miscellany for building distributions.
</td></tr></table>

<table><tr><th>`ghc.spec.in`</th>
<td>
RPM spec file
</td></tr></table>

<table><tr><th>`InstallShield`,`WindowsInstaller`</th>
<td>
Windows installer bits
</td></tr></table>