# The (new) GHC Build System


(this page will eventually replace [Building/BuildSystem](attic/building/build-system) when the new build system is merged in)


This section contains everything you need to know in order to
understand and modify the GHC build system.  The build system is
non-standard in various ways (to be explained shortly), and is
decidedly non-trivial: do not attempt to modify it without having a
grasp of the concepts that follow!


Each of the following subsections describes one of the ``idioms`` that
we use in the build system.  There are a handful of such idioms, and
when you've understood them all you'll be able to understand most of
the code you'll find in the build system.  We'll describe the idioms
first, and then get on to the specifics of how we build GHC.


Historical note: this is the third major revision of the GHC build
system.  The first incarnation was based on "jmake", a derivative of
X11's "imake", which is based on using the C preprocessor to add macro
capabilities and `#include` to plain make.  The second incarnation
used GNU make's extensions for including makefiles (but lost the
ability to use macros, since at the time GNU make didn't have support
for general macros).  In this third revision, we use even more of GNU
make's extensions, and we make a fundamental change to the design, as
described in the next section.

## Idiom: non-recursive make


Build systems for large projects often use the technique commonly
known as "recursive make", where there is a separate `Makefile` in
each directory that is capable of building that part of the system.
The `Makefile`s may share some common infrastructure and configuration
by using GNU make's `include` directive; this is exactly what the
previous GHC build system did.  However, this design has a number of
flaws, as described in Peter Miller's
[ Recursive Make Considered Harmful](http://miller.emu.id.au/pmiller/books/rmch/).  


The GHC build system adopts the non-recursive make idiom.  That is, we
never invoke make from inside a `Makefile`, and the whole build system
is effectively a single giant `Makefile`.


This gives us the following advantages:

- Specifying dependencies between different parts of the tree is
  easy.  In this way, we can accurately specify many dependencies
  that we could not in the old recursive-make system.  This makes it much more likely that when you say "make"
  after modifying parts of the tree or pulling new patches,
  the build system will bring everything up-to-date in the correct order, and leave you with a working
  system.

- More parallelism: dependencies are more fine-grained, and there
  is no need to build separate parts of the system in sequence, so
  the overall effect is that we have more parallelism in the build.


Doesn't this sacrifice modularity?  No - we can still split the build
system into separate files, using GNU make's `include`.


Specific notes related to this idiom:

- Individual directories have a `ghc.mk` file which typically
  contains the build instructions for that directory.

- The top-level of the build system is in `Makefile` and `ghc.mk`.

- Other parts of the build system are in `mk/*.mk` and `rules/*.mk`.

## Idiom: stub makefiles


It's all very well having a single giant `Makefile` that knows how to
build everything in the right order, but sometimes you want to build
just part of the system.  When working on GHC itself, we might want to
build just the compiler, for example.  In the recursive make system we
would do `cd ghc` and then `make`.  In the non-recursive system we can
still achieve this by specifying the target with something like \`make
ghc/stage1/build/ghc\`, but that's not so convenient.


Our second idiom therefore is to have a tiny stub `Makefile` in each
directory whose job it is to invoke the main `Makefile` specifying the
appropriate target(s) for that directory.  These stub `Makefiles`
follow a simple pattern:

```wiki
dir = libraries/base
TOP = ../..
include $(TOP)/mk/sub-makefile.mk
```


where `mk/sub-makefile.mk` knows how to recursively invoke make.  How
does it know what to build?  By convention, for each directory there
is a target `all_`*directory* (e.g. `all_libraries/base`) which
builds every target in that directory (see "Idiom: the "all" target",
below).

## Idiom: macros and variable names


Now that our build system is one giant `Makefile`, all our variables
share the same namespace.  Where previously we might have had a
variable that contained a list of the Haskell source files called
`HS_SRCS`, now we have one of these for each directory in the build
system, so we have to give them all different names.


The idiom that we use for distinguishing variable names is to prepend
the directory name to the variable.  So for example the list of
Haskell sources in the directory `utils/hsc2hs` would be in the
variable `utils/hsc2hs_HS_SRCS` (make doesn't mind slashes in variable
names).


In many parts of the build, we can build the same component in
multiple different ways.  For example, the compiler itself is built in
the `stage1` and `stage2` ways, so we additionally need to distinguish
variable names based on which way we're building.  Typically we place
each build in a separate subdirectory, so for example the `stage1`
build of the compiler places its files in `compiler/stage1`.  Variable
names related to this build would be named something like
`compiler_stage1_HS_SRCS`.  The pattern is therefore:
*directory*_*build*_*variable*.


The build system makes extensive use of macros.  A macro is defined in
GNU make using `define`, e.g.

```wiki
define build-package
# args: $1 = directory, $2 = build
... makefile code to build a package ...
endef
```


(for example, see `rules/build-package`), and is invoked like this:

```wiki
$(eval $(call build-library,libraries/base,dist))
```


(this code would be in `libraries/base/ghc.mk`).


Note that `eval` works like this: its argument is expended as normal,
and then the result is interpreted by make as makefile code.  This
means the body of the `define` gets expanded *twice*.  Typically
this means we need to use `$$` instead of `$` everywhere in the body of
`define`.


Now, the `build-package` macro may need to define local variables.
There is no support for local variables in macros, but we can define
variables which are guaranteed to not clash with other variables by
preceding their names with a string that is unique to this macro call.
A convenient unique string to use is *directory*_*build*_; this is unique as long as we only call each macro with a given directory/build pair once.  Most macros in
the GHC build system take the directory and build as the first two
arguments for exactly this reason.  For example, here's an excerpt
from the `build-prog` macro:

```wiki
define build-prog
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$1_$2_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
...
```


So if `build-prog` is called with `utils/hsc2hs` and `dist` for the
first two arguments, after expansion make would see this:

```wiki
utils/hsc2hs_dist_INPLACE = $(INPLACE_BIN)/$(utils/hsc2hs_dist_PROG)
```


The idiom of `$$($1_$2_VAR)` is very common throughout the build
system - get used to reading it!  Note that the only time we use a
single `$` in the body of `define` is to refer to the parameters `$1`,
`$2`, and so on.

## Idiom: phase ordering


NB. you need to understand this section if either (a) you are modifying parts of the build system that include automatically-generated `Makefile` code, or (b) you need to understand why we have a top-level `Makefile` that recursively invokes make.


The main hitch with non-recursive make arises when parts of the build
system are automatically-generated.  The automatically-generated parts
of our build system fall into two main categories:

- Dependencies: we use `ghc -M` to generate make-dependencies for 
  Haskell source files, and similarly `gcc -M` to do the same for
  C files.  The dependencies are normally generated into a file
  `.depend`, which is included as normal.

- `package-data.mk`.  Many of the components of the GHC build system
  are also Cabal packages, with package metadata defined in a 
  `foo.cabal` file.  For the GHC build system we need to extract that
  metadata and use it to build the package.  This is done by the
  program `ghc-cabal` (in `utils/ghc-cabal` in the GHC source tree).
  This program reads `foo.cabal` and produces `package-data.mk` 
  containing the package metadata in the form of makefile bindings
  that we can use directly.


Now, we also want to be able to use make to build these files, since
they have complex dependencies themselves (in order to build
`package-data.mk` we need to first build `ghc-cabal` etc., and a `.depend` file needs to be re-generated if any of the source files have changed).


GNU make has a clever strategy for handling this kind of scenario.  It
first reads all the included Makefiles, and then tries to build each
one if it is out-of-date, using the rules in the Makefiles themselves.
When it has brought all the Makefiles up-to-date, it restarts itself
to read the newly-generated Makefiles.


This works fine, unless there are dependencies *between* the
Makefiles.  For example in the GHC build, the `.depend` file for a
package cannot be generated until `package-data.mk` has been generated
and make has been restarted to read in its contents, because it is the
`package-data.mk` file that tells us which modules are in the package.
But make always makes all the `Makefiles` before restarting - it
doesn't know how to restart itself earlier when there is a dependency
between `Makefiles`.


Consider the following makefile:

```wiki
all :

include inc1.mk

inc1.mk : Makefile
	echo "X = C" >$@

include inc2.mk

inc2.mk : inc1.mk
	echo "Y = $(X)" >$@
```


Now try it:

```wiki
$ make -f fail.mk
fail.mk:3: inc1.mk: No such file or directory
fail.mk:8: inc2.mk: No such file or directory
echo "X = C" >inc1.mk
echo "Y = " >inc2.mk
make: Nothing to be done for `all'.
```


make built both `inc1.mk` and `inc2.mk` without restarting itself
between the two (even though we added a dependency on `inc1.mk` from
`inc2.mk`).


The solution we adopt in the GHC build system is as follows.  We have
two Makefiles, the first a wrapper around the second.

```wiki
# top-level Makefile
% :
        $(MAKE) -f inc.mk PHASE=0 just-makefiles
        $(MAKE) -f inc.mk $<
```

```wiki
# inc.mk

-include inc1.mk

inc1.mk : Makefile
	echo "X = C" >$@

ifneq "$(PHASE)" "0"
include inc2.mk

inc2.mk : inc1.mk
	echo "Y = $(X)" >$@
endif

just-makefiles:
        @: # do nothing

clean :
	rm -f inc1.mk inc2.mk
```


each time make is invoked, it first invokes `inc.mk` with `PHASE=0`.
This brings `inc1.mk` up-to-date (and *only*`inc1.mk`).  The second
time we invoke make, we can be sure that `inc1.mk` is up-to-date and
proceed to generate `inc2.mk`.  This is not at all pretty, and
re-inovking make every time is slow, but we don't know of a better
workaround for this problem.


In the case of the GHC build system we need 4 such phases, see the
comments in the top-level `ghc.mk` for details.

## Idiom: no double-colon rules


Make has a special type of rule of the form `target :: prerequisites`,
with the behaviour that all double-colon rules for a given target are
executed if the target needs to be rebuilt.  This style was popular
for things like "all" and "clean" targets in the past, but it's not
really necessary.   We adopt the following idiom instead:

```wiki
all : all_foo
.PHONY all_foo
all_foo : ...
```

## Idiom: the vanilla way


Libraries can be built in several different "ways", for example
"profiling" and "dynamic" are two ways.  Each way has a short tag
associated with it; "p" and "dyn" are the tags for profiling and
dynamic respectively.  In previous GHC build systems, the "normal" way
didn't have a name, it was just always built.  Now we explicitly call
it the "vanilla" way and use the tag "v" to refer to it.  


This means that the `GhcLibWays` variable, which lists the ways in
which the libraries are built, must include "v" if you want the
vanilla way to be built (this is included in the default setup, of
course).

## Idiom: the "all" and "clean" targets


There are pre-canned macros to define your "all" and "clean" targets,
take a look in `rules/all-target.mk` and `rules/clean-target.mk`.
