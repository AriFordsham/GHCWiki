[ Video: Getting and Building](http://video.google.com/videoplay?docid=7166458546326012899), layout of the source tree, how to set up build.mk (23'43")

# Quick start for developers


This section is for those who want to do more than just build & install GHC.  It
is for those who want to actually modify parts of GHC, and perhaps distribute those
modifications to others.  This section contains a few nuggets of information
that will help get you started right away.  For more detailed documentation
on the build system, read on to the later sections.

## Controlling your build: `mk/build.mk`

`mk/build.mk` is a GNU makefile that contains all your build settings.
By default, this file doesn't exist, and all the parameters are set to
their defaults in `mk/config.mk` (`mk/config.mk` is the place to look for
*all* the things you might want to tune). 


There's an example in 
[mk/build.mk.sample](/trac/ghc/browser/ghc/mk/build.mk.sample), which you can copy to `mk/build.mk` and edit as required.
Alternatively if you want to understand a bit more about what's going on (recommended), read on.

## How to make GHC build quickly


The GHC build tree is set up so that, by default, it builds a compiler
ready for installing and using.  That means full optimisation, and the
build can take a *long* time.  If you unpack your source tree and
right away say `./configure; make`, expect to have to wait a while.
For hacking, you want the build to be quick - quick to build in the
first place, and quick to rebuild after making changes.  Tuning your
build setup can make the difference between several hours to build
GHC, and less than an hour.  


Here are the `build.mk` settings that
we use to build fast:

```wiki
# My build settings for hacking on stage 1 
SRC_HC_OPTS     = -H32m -O -fasm -Rghc-timing
GhcStage1HcOpts = -O0 -DDEBUG -W
GhcLibHcOpts    = -O -fgenerics
GhcLibWays      =
SplitObjs       = NO
GhcBootLibs     = YES
```


What do these options do?

<table><tr><th>`SRC_HC_OPTS = -H32m -O -fasm -Rghc-timing`</th>
<td>
These options are added to the command line for all Haskell
compilations.  We turn on `-fasm`, because that halves compilation
time at the expense of a few percent performance.  `-Rghc-timing`
prints out a line of timing info about each compilation.  It's handy
to keep an eye on.  `-W` turns on some warnings; you probably want
to tweak exactly which warnings are turned on, since GHC isn't
very warning-clean with `-W`.
</td></tr></table>

<table><tr><th>`GhcStage1HcOpts = -O0 -DDEBUG`</th>
<td>
The options for building the stage1 compiler (these come after
SRC_HC_OPTS, so you can override settings from there).  We turn off
optimisation here, assuming you'll be modifying and testing stage1.
With optimisation off, rebuilding GHC after modifying it will be
*much* quicker, not only because the individual compilations will be
quicker, but also there will be fewer dependencies between modules,
so much less stuff is recompiled after each modification.

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

<table><tr><th>`GhcBootLibs = YES`</th>
<td>
If you're just interested in working on GHC, then you probably don't want
to build the "extralibs" libraries that we normally ship with GHC.  
So when [getting the sources](building/getting-the-sources), 
run `darcs-all` without the `--extra` option.  Alternatively, even if you have
the libraries in your tree, you can stop them being built by setting
`GhcBootLibs` in your `build.mk`.
</td></tr></table>


The other thing to remember is that for quick re-builds, you don't necessarily
want to go through the entire "make boot, make stage1, make libraries, make stage2"
sequence, which is the default if you type `make` in the root directory.  Instead,
we often say:

- `cd compiler; make stage=1`: re-makes the stage-1 compiler only
- `cd libraries; make`: re-make the libraries only
- `cd compiler; make stage=2`: re-make the stage-2 compiler only


If you do things this way, it's your responsibility to say `make boot` when necessary
to rebuild dependencies.

## Actually building the bits


To just build everything, from the top level:

```wiki
  $ autoreconf
  $ ./configure
  $ make
```


NB. that's auto**re**conf, not just `autoconf`.  The former
works recursively, which is necessary because the GHC tree contains
multiple configure scripts.


(See here for [what can go wrong](building/problems).)


If you just want to build stage 1, then instead you can say

```wiki
  $ make stage1
```


but note that the stage 1 compiler doesn't support GHCi or Template Haskell, those are compiled into stage 2 only (see 
[BootstrappingGHC](building/using#bootstrapping-ghc)).


To install the compiler you built, you can say

```wiki
  $ make install
```


However, you don't need to install GHC to use it.  Running `./compiler/stage1/ghc-inplace` from the build tree
will invoke the stage1 compiler, and `./compiler/stage2/ghc-inplace` will invoke the stage2 compiler.

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
