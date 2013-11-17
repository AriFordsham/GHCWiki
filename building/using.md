# Using the Build System


    
This part of the guide is about *using* the build system, that is,
how to interact with the build system when you're developing some part
of GHC, its libraries or tools.  The section aims to be comprehensive;
for a quick start, read [Building/Hacking](building/hacking) first.

## Source trees and build trees


Sometimes we want to separate the build tree from the source tree.
There are a few advantages to doing this:

- You can make multiple different builds from the same sources,
  perhaps for testing different build settings, or for building
  on different platforms.

- You might want to put the source tree on a remote, backed-up,
  filesystem, but keep your build tree on a local fast unbacked-up
  drive (this is a configuration we use regularly at GHC HQ).  It
  doesn't matter if you lose the build tree: it can easily be
  regenerated.

- It's easy to blow away a build tree and start again, without
  modifying your source tree.  `make maintainer-clean` is usually
  good for this too, but it can miss files that it doesn't know
  about, or files that are remnants from older versions of GHC.

- It helps to avoid mistakes whereby you edit a file that happens
  to be automatically generated, instead of the original source
  file (e.g. editing `config.mk` instead of `config.mk.in`).  If
  you only edit files in the source tree, then this can't happen.


However, if you just want to build the software once on a single
platform, then your source tree can also be your build tree, and you
can skip the rest of this section.

**Windows users**: so far as we know, symbolic links do not work right on MSYS at least, so we never use separate source and build trees on Windows.

**Mac OS X 10.8 users**: Apple no longer includes X11 with Xcode (which provided `lndir`). Install [ XQuartz](http://xquartz.macosforge.org/landing/) \>= 2.7.2 or build it directly in `utils/lndir` (as below).


A *build tree* is just an exact copy of the source tree, except that
every file in it is a symbolic link to the appropriate file in the
source tree.  There are "standard" Unix utilities that make such
copies, so standard that they go by different names: `lndir` and
`mkshadowdir` are two (If you don't have either, the GHC source
tree contains sources for the X11 `lndir` check out
`utils/lndir`).  To create a separate build tree, the typical sequence is something like this:

```wiki
  $ mkdir ghc-build
  $ cd ghc-build
  $ lndir <source>
  $ ln -s <source>/.git .
```


Where `<source>` is the directory containing your source tree.  Note the last step: GHC's `configure` script likes to see the `.git` directory, and by default `lndir` will not link `.git` directories.  Things will still work if you omit this step, but the GHC version number for your build won't contain the date (i.e. it will be "7.7" instead of something like "7.7.20121218").


You need to be a bit careful when using a build tree, that any new files you create
(if you do any development work) are in the source tree, not the build
tree!  This is especially easy to mess up when creating new tests, so watch out.

## Booting, configuring, cleaning


GHC uses the `autoconf` tools in the standard Unixy way, described in more detail in subsequent subsections on this page:

```wiki
      ---------------
      |             |
      | Source code | <--------------------\
      |             |                      |
      ---------------            maintainer-clean
        |                                  |
    perl boot (runs autoconf)              | 
        |                                  |
        v                                  |   
      ----------------------------         |
      | Source tar-ball          |>-------/|
      |  ./configure             |         |
      |  libraries/*/GNUmakefile |         |
      |  libraries/*/ghc.mk      |<-\      |
      ----------------------------  |      |
        |                           |      |
      ./configure               distclean  |
        |                           |      |
        v                           |      |
      ------------------            |      |         
      | Configured     |>----------/|      |
      |   mk/config.mk |            |      |
      |                |>-----------------/|
      ------------------            |      |
        |                           |      |
       make                         |      |
        |                           |      |
        v                           |      |
      ------------------            |      |
      | Built          |>-----------/      |
      |                |                   |
      |                |>------------------/
      ------------------
```


See also [standard targets](building/architecture/idiom/standard-targets) and [Commentary/SourceTree](commentary/source-tree).

## Steps to prepare for building


We gave a quick getting-started introduction to building GHC in
[Building/QuickStart](building/quick-start), the following sections describe each step
in more detail.


In the build system, the path to the top of your build tree is
referred to as `$(TOP)`, and we will use that convention in the
following sections.

### Generate configure scripts


NOTE: if you're starting from a source distribution that you
downloaded from the GHC web site, rather than git sources, you can
skip this step.  Source distributions come with configure scripts
pre-generated for you.


Change directory to `$(TOP)` and issue the command

```wiki
$ perl boot
```


(Note: the `$` is the prompt.  You don't type that bit.)  This runs
`autoreconf` on the various `configure.ac` scripts in the GHC build
tree, generating `configure` scripts and other miscellaneous files.


You have to re-do this step if (and only if) you ever change one of
the files that `autoconf` uses to generate the `configure` scripts,
such as `configure.ac`, or `aclocal.m4`.

### Run the configure script


Run the `configure` script, thus:

```wiki
$ ./configure <args>
```

`configure`'s mission is to scurry round your computer working out
what architecture it has, what operating system, whether it has the
`vfork` system call, where `tar` is kept, whether `gcc` is
available, where various obscure `#include` files are, whether
it's a leap year, and what the systems manager had for lunch.  It
communicates these snippets of information in two ways:

- It translates various files such as `mk/config.mk.in` to
  `mk/config.mk`, substituting for things between "`@`"
  brackets.  So, "`@HaveGcc@`" will be replaced by "`YES`" or
  "`NO`" depending on what `configure` finds.
  `mk/config.mk` is included by every Makefile (directly or
  indirectly), so the configuration information is thereby
  communicated to all Makefiles.

- It translates `mk/config.h.in` to `mk/config.h`.  The
  latter is `#include`d by various C source files, which can
  thereby make use of configuration information.

`configure` takes some optional arguments.  Use `./configure --help`
to get a list of the available arguments.  Here are some of
the ones you might need:

<table><tr><th>`--host=<platform>`</th>
<td>
Set the "host platform" (see [platform names](building/architecture/idiom/platform-names)).
Usually only necessary if cross-compiling.
</td></tr></table>

<table><tr><th>`--target=<platform>`</th>
<td>
Set the "target platform" (see [platform names](building/architecture/idiom/platform-names)).
Usually only necessary if cross-compiling.
</td></tr></table>

<table><tr><th>`--with-ghc=<path>`</th>
<td>
Specifies the path to an installed GHC which you would like to use.
This compiler will be used for compiling GHC-specific code (eg. GHC
itself).  This option *cannot* be specified using `build.mk`
(see later), because `configure` needs to auto-detect the
version of GHC you're using.  The default is to look for a compiler
named `ghc` in your `$PATH`.
</td></tr></table>

<table><tr><th>`--with-gcc=<path>`</th>
<td>
Specifies the path to the installed GCC. This compiler will be used
to compile all C files, *except* any generated by the installed
Haskell compiler, which will have its own idea of which C compiler
(if any) to use.  The default is to use `gcc`.  On Windows, this
should be set to the gcc that comes with MinGW, which by default
is `c:/mingw/bin/gcc`.
</td></tr></table>

<table><tr><th>`--with-ld=<path>`</th>
<td>
Specifies which `ld` program to use.  Normally only necessary on
Windows, where you should set it to the `ld` that comes with MinGW,
which is usually `c:/mingw/bin/ld`.
</td></tr></table>

<table><tr><th>`--prefix`</th>
<td>
Tells the build system where you would like GHC to be eventually
installed.  You don't have to install GHC in order to use it: it is
entirely possible to work on GHC and test it without ever modifying
anything outside the build tree.  However, if you do want to install
GHC, then the `--prefix` directory is the root of the install tree.
Typically on Unix systems the default for `--prefix` is
`/usr/local`.  See also [Building/Installing](building/installing) for more details.
</td></tr></table>

### Build configuration


Next, you say how this build of GHC is to differ from the standard
defaults by creating a new file `mk/build.mk`*in the build tree*.
This file is the one and only file you edit in the build tree,
precisely because it says how this build differs from the source.
(Just in case your build tree does die, you might want to keep a
private directory of `build.mk` files, and use a symbolic link in each
build tree to point to the appropriate one.)

`mk/build.mk` is purely for overriding settings that are found in
`mk/config.mk`.  You should never edit `mk/config.mk` directly, since
it is automatically generated from `mk/config.mk.in` by `configure`.
Neither should you edit `mk/config.mk.in`; just provide your settings
in `mk/build.mk`.


We provide a sample file, `mk/build.mk.sample`, which you can copy to
`mk/build.mk` and edit.  It provides a number of pre-defined
configurations, such as a "fast build" or a "development build".  Take
a look in the file itself for details.


To understand more about what you can put in `mk/build.mk`, read on.

#### Common build.mk options


The following are some common variables that you might want to set in
your `mk/build.mk`.  For other variables that you can override,
take a look in [mk/config.mk.in](/trac/ghc/browser/ghc/mk/config.mk.in).

<table><tr><th>`SRC_HC_OPTS`</th>
<td>
(default: `-H32m -O`)

Options passed to GHC for all Haskell compilations.
</td></tr></table>

<table><tr><th>`GhcHcOpts`</th>
<td>
(default: `-Rghc-timing`)

Options added when compiling GHC (all
[stages](building/architecture/idiom/stages))
</td></tr></table>

<table><tr><th>`GhcStage1HcOpts`</th>
<td>
(default: *empty*)

Options added when compiling the stage 1 GHC.
</td></tr></table>

<table><tr><th>`GhcStage2HcOpts`</th>
<td>
(default: `-O2`)

Options added when compiling the stage 2 GHC.
</td></tr></table>

<table><tr><th>`GhcStage3HcOpts`</th>
<td>
(default: `-O2`)

Options added when compiling the stage 3 GHC.
</td></tr></table>

<table><tr><th>`GhcLibHcOpts`</th>
<td>
(default: `-O2 -XGenerics`)

Options added when compiling the libraries.
</td></tr></table>

<table><tr><th>`GhcProfiled`</th>
<td>
(default: `NO`)

Set to `YES` to enable profiling for GHC itself (stage 2).  You'll also need to add `GhcLibWays += p` to make this work.
</td></tr></table>

<table><tr><th>`GhcDebugged`</th>
<td>
(default: `NO`)

Set to `YES` to pass `-debug` when building GHC (stage 2).
</td></tr></table>

<table><tr><th>`GhcLibWays`</th>
<td>
(default: `v p`)

Ways in which to build the libraries.  Must contain
at least `v` ([the vanilla way](building/architecture/idiom/vanilla-way)).  Also contains `p` by default (profiling).  For other
ways, see `mk/ways.mk`.
</td></tr></table>

<table><tr><th>`SplitObjs`</th>
<td>
(default: `YES` if supported, `NO` otherwise)

When set to `YES`, static library object files are split into smaller
pieces.  This means that less of the library code needs to be linked
into the final application, which makes smaller binaries.  It takes
longer to build libraries this way, though.
</td></tr></table>

<table><tr><th>`LAX_DEPENDENCIES`</th>
<td>
(default: `NO`)

When set to `YES`, dependencies on the ghc executable will be turned into
order-only dependencies (c.f. [ relevant make documentation](http://www.gnu.org/software/make/manual/html_node/Prerequisite-Types.html)).
What this means in practice is that less needless recompilation will be
done while you are making changes to ghc's sources, but for certain types
of changes it means the build will fail. 
</td></tr></table>

<table><tr><th>`BUILD_DOCBOOK_HTML`</th>
<td></td></tr>
<tr><th>`BUILD_DOCBOOK_PS`</th>
<td></td></tr>
<tr><th>`BUILD_DOCBOOK_PDF`</th>
<td>
(default: `YES` if supported, `NO` otherwise)

When set to `YES`, these build the docbook documentation (e.g. the
users guide) as HTML, PS and PDF respectively.
</td></tr></table>

<table><tr><th>`INTEGER_LIBRARY`</th>
<td>
By default this is set to `integer-gmp`, which means Integer is implemented
on top of the C GMP library. If you set it to `integer-simple` then a
simple, BSD-licensed Haskell implementation will be used instead.
</td></tr></table>

#### How to make GHC build quickly


The GHC build tree is set up so that, by default, it builds a compiler
ready for installing and using.  That means full optimisation, and the
build can take a *long* time.  If you unpack your source tree and
right away say `./configure; make`, expect to have to wait a while.
For hacking, you want the build to be quick - quick to build in the
first place, and quick to rebuild after making changes.  Tuning your
build setup can make the difference between several hours to build
GHC, and less than an hour.  


Here are the `build.mk` settings that we use to build fast:

```wiki
# My build settings for hacking on stage 2
SRC_HC_OPTS     = -H32m -O -fasm -Rghc-timing
GhcStage1HcOpts = -O -fasm
GhcStage2HcOpts = -O0 -DDEBUG -Wall
GhcLibHcOpts    = -O -fasm -XGenerics
GhcLibWays      = v
SplitObjs       = NO
```


What do these options do?

<table><tr><th>`SRC_HC_OPTS = -H32m -O -fasm -Rghc-timing`</th>
<td>
These options are added to the command line for all Haskell
compilations.  We turn on `-fasm`, because that halves compilation
time at the expense of a few percent performance. `-Rghc-timing`
prints out a line of timing info about each compilation.  It's handy
to keep an eye on.  `-Wall` turns on all the warnings; GHC is
meant to be warning-clean with `-Wall`.
</td></tr></table>

<table><tr><th>`GhcStage1HcOpts = -O -fasm`</th>
<td>
Build stage 1 optimised: we're going to be rebuilding stage 2 a lot,
so we want the compiler that does the building to be fast.
</td></tr></table>

<table><tr><th>`GhcStage2HcOpts = -O0 -DDEBUG -Wall`</th>
<td>
We turn off optimisation here, assuming you'll be modifying and
testing stage 2.  With optimisation off, rebuilding GHC after
modifying it will be *much* quicker, not only because the
individual compilations will be quicker, but also there will be
fewer dependencies between modules, so much less stuff is recompiled
after each modification.

Also we turn on `-DDEBUG`, because that enables assertions and
debugging code in the compiler itself.  Turning on DEBUG makes
the compiler about 30% slower.
</td></tr></table>

<table><tr><th>`GhcLibHcOpts = -O -fasm -XGenerics`</th>
<td>
You almost certainly want optimisation *on* when building
libraries, otherwise the code you build with this compiler
goes really slowly.  `-XGenerics` adds generics support to the
libraries - you can turn this off if you like (it'll make the
libraries a bit smaller), but you won't be able to use Generics in
the code you build against these libraries.
</td></tr></table>

<table><tr><th>`GhcLibWays = v`</th>
<td>
Normally the profiled libraries are built.  Setting `GhcLibWays` to
just "v" disables this, so you only build the normal libs.
</td></tr></table>

<table><tr><th>`SplitObjs = NO`</th>
<td>
Object splitting causes each module to be split into smaller
pieces in the final library, to reduce executable sizes when
linking against the library.  It can be quite time and
memory-consuming, so turn it off when you're hacking.
</td></tr></table>

## Building things


At this point you have made yourself a fully-configured build tree, so
you are ready to start building real things.


The first thing you need to know is that *you must use GNU
`make`*.  On some systems (eg. FreeBSD) this is called
`gmake`, whereas on others it is the standard `make` command.
In this document we will always refer to it as `make`; please
substitute with `gmake` if your system requires it.  If you use a
the wrong `make` you will get all sorts of error messages (but no
damage) because the GHC `Makefiles` use GNU `make`'s
facilities extensively.


To just build the whole thing, `cd` to the top of your build tree and
type `make`.  This will prepare the tree and build the various parts
in the correct order, resulting in a complete build of GHC that can
even be used directly from the tree (as `inplace/bin/ghc-stage2`),
without being installed first.

### Order of the build


Here is a high level view of what happens when you build GHC:

- First, we build a few packages that GHC itself depends on, such as
  `Cabal`, and `filepath`, using your installed GHC.  These packages
  are under `libraries`, and each is built in a subdirectory
  `dist-boot`; for example the bootstrap build of Cabal will be in
  `libraries/Cabal/dist-boot`.

- Then we build package `ghc`, still using the installed GHC.
  The `ghc` package is a perfectly ordinary package.  The source
  code for the package is all the files in `compiler/`.

- Now, still using the installed GHC, we build the `ghc` executable,
  from source code in `ghc/`.  This source code is just a small Haskell
  program that depends on package `ghc`. 

>
> The resulting executable is
> called the "stage 1" compiler (see
> [stages](building/architecture/idiom/stages)).  You can run the
> stage 1 compiler by invoking `inplace/bin/ghc-stage1`.  The stage 1
> build of GHC happens in `compiler/stage1`.

- The stage 1 compiler is now used to build all the packages in the
  `libraries` subdirectory, and the runtime system in `rts`.

- Finally, the stage 1 compiler is used to build GHC itself again,
  this time against the libraries we just built.  This GHC is called
  stage 2, and can be invoked as `inplace/bin/ghc-stage2`.


There's an optional final stage, using the stage 2 compiler to build a
stage 3 compiler, but this isn't strictly necessary, and is only used
to check that the stage 2 compiler is working properly.

### What to do after `make` has finished


If `make` completes successfully, then it should have created
`inplace/bin/ghc-stage2`, which is a GHC binary you can run directly.
It supports all the usual features, including GHCi if you pass the
`--interactive` flag.  


In fact, the `inplace` directory looks a lot like an installed copy of
GHC (see [Building/Installing](building/installing)): there is a `bin`
subdirectory containing various programs that can be run, including
`ghc-pkg`, Haddock and `hsc2hs`.


You can now run the testsuite, see [Building/RunningTests](building/running-tests).
 
You can now install GHC, by typing `make install`.

### What to do if you get a build failure


GHC is a complex system, with many platform-dependent components.  We
try our best to make sure it builds out of the box as often as
possible, but build failures are not uncommon.  If you get some kind
of failure, don't panic.


The chances are, someone else already encountered the same problem as
you and has reported it on a mailing list or as a ticket.  Search for
the error message using your favourite search engine, and if that
doesn't turn anything up then search directly on this Trac.  Finally,
if you don't find a solution:

- Ask someone: [MailingListsAndIRC](mailing-lists-and-irc)
- or just [ReportABug](report-a-bug)

## Developing in a GHC build tree


This section describes how to make changes to the sources of GHC, or
its libraries and tools, and use the build system to build and test
those changes.

### Bringing the whole tree up to date


The easy way to bring the tree up to date after making some changes is
simply to go to the top of the tree and say `make`.  The build system
has enough dependencies encoded into it that this should rebuild
everything that needs to be rebuilt, in the correct order.  The
downside is that it will build everything right though to the stage2
compiler and including all the documentation, which might be overkill
if all you wanted to do was to test a small change to GHC.

### Building a single sub-component


Each subdirectory of the source tree has a
[stub makefile](building/architecture/idiom/stub-makefiles),
most of which follow this pattern:

```wiki
dir = libraries/base
TOP = ../..
include $(TOP)/mk/sub-makefile.mk
```


the main purpose of the stub makefile is to invoke `make` at the
top-level of the tree: GHC's build system is
[non-recursive](building/architecture/idiom/non-recursive-make), so
in effect there is really just one `Makefile`, at the top level.
However, the stub makefile has another purpose: it passes a target to
the top-level `Makefile`, telling it to build just the components of
the system in this directory.  For example, when you say `make` in the
`rts` directory, this is actually equivalent to

```wiki
$ make -C .. all_rts
```


where "`-C ..`" tells make to invoke the `Makefile` in the directory "`..`", and `all_rts` is the target that makes everything in the `rts` subdirectory.
Equivalently, `make all_libraries/base` at the top level would build
everything in the `libraries/base` subdirectory.  To understand how
the `all` targets are defined, see
[standard targets](building/architecture/idiom/standard-targets).


You can also clean a single component of the tree, just by saying
`make clean` in a subdirectory.  Again this is equivalent to issuing a
command at the top of the tree of the form `make clean_libraries/base`.

### Rebuilding the GHC binary after making changes


Suppose you want to make a small change to GHC itself and test it.
Assuming that your tree is already up to date, the best way to do this
is as follows:

```wiki
$ cd ghc
$ make stage=2
```


Note that the first command above takes you to the `ghc` subdirectory of the source tree, not into the source tree (which is also named `ghc` if you did a `git clone`).  So if you did a `git clone` from your home directory, you'll be in `~/ghc/ghc/`, not `~/ghc/`.  Many of the compiler-building `make` commands must be performed from this subdirectory, not from the root of the source tree.


This will bring the stage 2 compiler up to date only.  Setting `stage=2` has the effect of disabling all the
rules that build the stage 1 compiler, so the build system will ignore the fact that the stage 1 compiler is also out of date, and hence all the libraries are also potentially out of date.  If you just did `make`
from the top-level, all of these dependencies would be taken into
account, and a lot of rebuilding would probably ensue.  There's another target
that takes an even quicker shortcut:

```wiki
$ cd ghc
$ make 2
```


This is like `make stage=2`, except that it omits the dependency-building phase (`make 2` is in fact just shorthand for `make stage=2 FAST=YES`; see [Fast Rebuilding](building/using#fast-rebuilding) below).  If you have changed the imports in any modules, those new dependencies will not be taken into account by the build system, so you might get a build failure.  On the other hand, this shortcut usually works and the few seconds it saves can make GHC development a much more interactive experience.  There are also targets

- `make 1`
- `make 3`


to make the stage 1 and stage 3 compilers respectively.  These targets work in both the `ghc` and `compiler` subdirectories ([Commentary/SourceTree](commentary/source-tree)).


In addition, the `re1`, `re2`, and `re3` rules quickly rebuilds the stage 1, 2, and 3 compiler executables (e.g. `re2` is equivalent to `rm $PATH_TO_STAGE2_EXECUTABLE; make 2`). This is useful for relinking the compiler after a change like `GhcDebugged=YES`.


Note that if you’ve never built stage3 before, you will need to create dependencies for it using `make stage=3`. This is because a normal build will skip building the stage3 compiler. You will then be able to run `make 3` as usual.

### Freezing stage 1


Often when working on GHC we find ourselves doing `make 2` a lot.  If we accidentally say `make` at some point, that will start building stage 1 (because presumably something in the GHC source code has changed), which has many knock-on effects: all the libraries will be reconfigured, rebuilt, and then stage 2 will be completely rebuilt.  To prevent this from happening, we can "freeze" stage 1 by adding a line to `mk/build.mk`:

```wiki
stage = 2
```


this prevents stage 1 from being rebuilt until this line is removed or commented-out again.  It's a handy trick when you're working on GHC.

### Building a single file


It's possible to tell make to build a single file, from any subdirectory in the tree.  For example, suppose I want to build just the module `Control.Monad` in the `base` package, I can do it like this:

```wiki
$ make libraries/base/dist-install/build/Control/Monad.o
```


(you have to know that `dist-install` is the distdir for a package, and object files are put in the subdirectory `build`).  It's also possible to do this from the `libraries/base` subdirectory:

```wiki
$ cd libraries/base
$ make dist-install/build/Control/Monad.o
```


suppose you wanted to build this module with a few extra flags, perhaps because you want to see what GHC's optimiser is doing on this module:

```wiki
$ rm dist-install/build/Control/Monad.o
$ make dist-install/build/Control/Monad.o EXTRA_HC_OPTS=-dcore-lint
```


you could also cut-and-paste the command-line to add flags, but sometimes the `EXTRA_HC_OPTS` method is more convenient.

### Fast rebuilding


Often when you're working in a particular part of the tree, the rest of the tree is up to date, and you just want to rebuild the component you're working on after making changes.  To speed things up, we'd like to avoid having `make` check that everything else in the tree is up-to-date before building the component we're working on.  So the GHC build system provides a couple of ways to do this:

<table><tr><th>`make FAST=YES`</th>
<td>
Only has an effect in a subdirectory.  Setting `FAST=YES` causes `make` to omit rebuilding the `.depend` file (if any),
and also omits some of the [phases](building/architecture/idiom/phase-ordering).  `FAST=YES` is allowed in conjunction
with any other target; for example, it makes sense when rebuilding a single file, as in the previous section.
</td></tr></table>

<table><tr><th>`make fast`</th>
<td>
Shorthand for `make all FAST=YES`.
</td></tr></table>


Another useful trick is

<table><tr><th>`make stage=0`</th>
<td>
Does not build any GHC stages at all.  `stage=0` can be used in combination with other targets and settings.
</td></tr></table>

## Verbose build


By default, the build log is printed in a compact form, where each invocation
of the compiler looks something like:

```wiki
HC [stage 0] compiler/stage1/build/Constants.o
```


This makes it easier to see the build progress, but sometimes it can be useful
to inspect the full command lines that are being used.


To enable full output, run `make` with the `V=1` option:

```wiki
make V=1
```


You can also put the "`V=1`" in your `build.mk` or `validate.mk` file.

## Installing extra packages


The [boot libraries](commentary/libraries) are built as part of building GHC; they are built with the stage1 compiler, and imported when the stage2 compiler is compiled with stage1.


All other libraries are stand-alone Cabal packages, and the build system knows nothing about them.  Nevertheless, it is common to want to install extra packages for the GHC in your build tree.  Here are [instructions for how to do so](debugging/installing-packages-inplace).
