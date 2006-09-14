# Quick start for developers


This section is for those who want to do more than just build & install GHC.  It
is for those who want to actually modify parts of GHC, and perhaps distribute those
modifications to others.  This section contains a few nuggets of information
that will help get you started right away.  For more detailed documentation
on the build system, read on to the later sections.

## Setting up your build


The GHC build tree is set up so that, by default, it builds a compiler
ready for installing and using.  That means full optimisation, and the
build can take a *long* time.  If you unpack your source tree and
right away say `./configure; make`, expect to have to wait a while.


For hacking, you want the build to be quick - quick to build in the
first place, and quick to rebuild after making changes.  Tuning your
build setup can make the difference between several hours to build
GHC, and less than an hour.  Here's how to do it.

`mk/build.mk` is a GNU makefile that contains all your build settings.
By default, this file doesn't exist, and all the parameters are set to
their defaults in `mk/config.mk` (`mk/config.mk` is the place to look for
*all* the things you might want to tune).


A good `mk/build.mk` to start hacking on GHC is:

```wiki
SRC_HC_OPTS     = -H32m -O -fasm -Rghc-timing
GhcStage1HcOpts = -O0 -DDEBUG
GhcLibHcOpts    = -O -fgenerics
GhcLibWays      =
SplitObjs       = NO
```


What do these options do?

<table><tr><th>`SRC_HC_OPTS = -H32m -O -fasm -Rghc-timing`</th>
<td>
These options are added to the command line for all Haskell
compilations.  We turn on `-fasm`, because that halves compilation
time at the expense of a few percent performance.  `-Rghc-timing`
prints out a line of timing info about each compilation.  It's handy
to keep an eye on.
</td></tr></table>

<table><tr><th>`GhcStage1HcOpts = -O0 -DDEBUG`</th>
<td>
The options for building the stage1 compiler (these come after
SRC_HC_OPTS, so you can override settings from there).  We turn off
optimisation here, assuming you'll be modifying and testing stage1.
With optimisation off, rebuilding GHC after modifying it will be
*much* quicker, not only because the individual compilations will be
quicker, but also there will be fewer dependencies between modules,
so less stuff needs to be rebuilt after each modification.

Also we turn on `-DDEBUG`, because that enables assertions and
debugging code in the compiler itself.  Turning on DEBUG makes
the compiler about 30% slower.
</td></tr></table>

<table><tr><th>`GhcLibHcOpts = -O -fgenerics`</th>
<td>
You almost certainly want optimisation *on* when building
libraries, otherwise the code you build with this compiler
goes really slowly.  `-fgenerics` add generics support to the
libraries - you can turn this off if you like (it'll make the
libraries a bit smaller), but you won't be able to use Generics in
the code you build against these libraries.
</td></tr></table>

<table><tr><th>`GhcLibWays =`</th>
<td>
Normally the profiled libs are built.  Setting `GhcLibWays` to
empty disables this, so you only build the normal libs.
</td></tr></table>

<table><tr><th>`SplitObjs = NO`</th>
<td>
Object splitting causes each module to be split into smaller
pieces in the final library, to reduce executable sizes when
linking against the library.  It can be quite time and
memory-consuming, so turn it off when you're hacking.
</td></tr></table>

## Actually building the bits


To just build everything, from the top level:

```wiki
  $ autoreconf
  $ ./configure
  $ make
  $ make install
```


(See here for [what can go wrong](building/problems).)

## Building individual parts of the tree


The first thing to understand is that the source tree is built in two
passes.  First `make boot` builds dependencies and any other tools
required as part of the build itself.  For example,
`utils/genprimopcode` is built as part of `make boot`, because it is
required to preprocess `compiler/prelude/primops.txt.pp`.


After `make boot`, `make` will build everything.


If you say `make` from the very top-level, the build system will
arrange to do the appropriate 'make boot' steps for you.  If you just
want to build in a subdirectory (eg. ghc), you have to do `make boot`
yourself.  You don't need to `make boot` after every single change,
but you might want to do it to update dependencies, for example.

## Refining the setup


If you will be hacking mostly on libraries, then you probably want to
build stage1 with optimisation, because you're only building it once
but using it many times.

```wiki
  GhcStage1HcOpts = -O
```


If you are working on GHCi or Template Haskell, then you will be
building and modifying the stage 2 compiler.  Hence, you want to build
stage 1 with, and stage 2 without, optimisation.

```wiki
  GhcStage1HcOpts = -O
  GhcStage2HcOpts = -O0 -DDEBUG
```


Take a look through `mk/config.mk` for more settings you might want to
override in build.mk.  Remember: don't modify `config.mk` directly (it
gets overwritten when you run `./configure`).

## Full optimisation


To turn up everything to the max, for running performance tests for
example, try these:

```wiki
  SRC_HC_OPTS  = -H64m -O2 
  GhcLibHcOpts = -O2
  SplitObjs    = YES
```


You can even add some more aggresive options, such as
`-fliberate-case-threshold50`, `-funfolding-use-threshold50`.


Here is a [roadmap to the source tree](commentary/source-tree).
