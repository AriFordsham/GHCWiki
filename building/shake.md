# Shaking up GHC


This page is a working document to capture the ongoing work on migrating the current GHC build system based on `make` into a new and (hopefully) better one based on `Shake`. See [Building/Architecture](building/architecture) and [Building/Using](building/using) for more details on the current build system. For general reference to `Shake`, the first sources to try are:

- [ http://hackage.haskell.org/package/shake/docs/Development-Shake.html](http://hackage.haskell.org/package/shake/docs/Development-Shake.html) (Haddock docs)
- [ https://github.com/ndmitchell/shake/blob/master/docs/Manual.md\#readme](https://github.com/ndmitchell/shake/blob/master/docs/Manual.md#readme) (User manual)


Somewhat related, [\#5793](https://gitlab.haskell.org//ghc/ghc/issues/5793) suggests using `Shake` for `nofib`, and there's some code attached as well: [attachment:Main.2.hs:ticket:5793](/trac/ghc/attachment/ticket/5793/Main.2.hs)[](/trac/ghc/raw-attachment/ticket/5793/Main.2.hs)

## The goal


The existing build system performs the following major steps:

- **boot**: run `autoconf` on {`configure.ac`, ...} to produce the `configure` script, as well as `libraries/*/GNUmakefile` and `libraries/*/ghc.mk` files. Note that some `libraries/*` also contain `configure.ac` and thus require `autoconf` to create local `configure` scripts.

- **configure**: take a bunch of `*.in` files {`config.mk.in`, `ghc.cabal.in`, `ghc-bin.cabal.in`, ...} and generate {`config.mk`, `ghc.cabal`, `ghc-bin.cabal`, ...}. 

- **make**: do the rest of the build in three phases by invoking `ghc.mk` with the phase parameter set to one of {`0`, `1`, `final`}. All other `*.mk` files {`config.mk`, `tree.mk`, ...} are included in `ghc.mk`. The approximate build order is described in [ghc.mk](/trac/ghc/browser/ghc/ghc.mk)[](/trac/ghc/export/HEAD/ghc/ghc.mk). 


The goal is to eventually replace all of the above with a single shake script that will be invoking `autoconf`, `configure`, etc., and taking care of all dependencies. Specific parts of the old build system that will be shake-ified are: `boot`, `ghc-cabal` and `*.mk`.

### Why are we doing this?


We are unhappy with the current build system for a number of reasons including:

- It works in a number of phases, relating to when you can generate dependency information from files that are themselves generated. `Shake` is specifically designed to eliminate this complication. 
- For good reasons the current build systems uses `make` in a very tricky way; you can see expressions like `$$$$(dir $$$$@)`, meaning multiple layers of interpretation.
- Because `make` has a single global variable space, it is extremely hard to find how a particular variable gets its value.

## Naming conventions


We want to keep all existing naming conventions. For example, `*.o` files will stay as they are (as opposed to being renamed to `*.hs.o` and `*.c.o` as advised in shake documentation). This implies that there will be

- either a separate (automatically generated) build rule for each `*.o` file in the shake script, which may end up being slow;

- or the filenames will be stored in `Set`-like structures (e.g., `setHsObj` and `setCObj`) and the corresponding build rules will be: 

  ```wiki
  (`member` setHsObj) ?> \out -> do { ... }.
  ```

## Build options


One of the major difficulties will be taking care of various build options. A possible direction to start with is pulling the configuration out of the `*.mk` files, with a *parser* for a subset of `Makefile`, resolve the variables, and then use that information to drive the `Shake` rules. Some initial inspiration can be taken from the [ Shake.Config module](http://hackage.haskell.org/package/shake-0.13.4/docs/Development-Shake-Config.html) that can parse simple configuration files and is integrated with shake rules. Also see [ Development.Make](https://github.com/ndmitchell/shake/tree/master/Development/Make) for a simple `Makefile` parser embedded in `Shake`. Some configuration values can be passed as command line parameters to `Shake`, which can be handled using `shakeArgs`.


Parsing the existing `*.mk` files and extracting variables is an interesting small standalone project (see intermediate goals).

### Where options come from


The table below explains where most build variables are defined (this is taken from `rules/distdir-way-opts.mk`). Arguments `$1-$4` stand for:

- `$1` is the directory we're building in
- `$2` is the distdir (e.g. "dist", "dist-install" etc.)
- `$3` is the way (e.g. "v", "p", etc.)
- `$4` is the stage ("1", "2", "3")

<table><tr><th>  Variable </th>
<th> Purpose </th>
<th> Defined by<sup>\*</sup></th></tr>
<tr><th>`$1_PACKAGE`</th>
<th> Package name for this dir, if it is a package </th>
<th>`$1/$2/ghc.mk`</th></tr>
<tr><th>`CONF_HC_OPTS`</th>
<th> GHC options from `./configure`</th>
<th>`mk/config.mk.in`</th></tr>
<tr><th>`CONF_HC_OPTS_STAGE$4`</th>
<th> GHC options from `./configure` specific to stage `$4`</th>
<th>`mk/config.mk.in`</th></tr>
<tr><th>`WAY_$3_HC_OPTS`</th>
<th> GHC options specific to way `$3`</th>
<th>`mk/ways.mk`</th></tr>
<tr><th>`SRC_HC_OPTS`</th>
<th> source-tree-wide GHC options </th>
<th>`mk/config.mk.in`, `mk/build.mk`, `mk/validate.mk`</th></tr>
<tr><th>`SRC_HC_WARNING_OPTS`</th>
<th> source-tree-wide GHC warning options </th>
<th>`mk/config.mk.in`, `mk/build.mk`, `mk/validate.mk`</th></tr>
<tr><th>`EXTRA_HC_OPTS`</th>
<th> for supplying extra options on the command line </th>
<th>`make EXTRA_HC_OPTS=...`</th></tr>
<tr><th>`$1_HC_OPTS`</th>
<th> GHC options specific to dir `$1`</th>
<th>`$1/$2/package-data.mk`</th></tr>
<tr><th>`$1_$2_HC_OPTS`</th>
<th> GHC options specific to dir `$1` and distdir `$2`</th>
<th>`$1/$2/package-data.mk`</th></tr>
<tr><th>`$1_$2_$3_HC_OPTS`</th>
<th> GHC options specific to dir `$1`, distdir `$2` and way `$3`</th>
<th>`$1/$2/package-data.mk`</th></tr>
<tr><th>`$1_$2_MORE_HC_OPTS`</th>
<th> GHC options specific to dir `$1` and distdir `$2`</th>
<th> ?? 
</th></tr>
<tr><th>`$1_$2_EXTRA_HC_OPTS`</th>
<th> GHC options specific to dir `$1` and distdir `$2`</th>
<th>`mk/build.mk`</th></tr>
<tr><th>`$1_$2_HC_PKGCONF`</th>
<th>`-package-db` flag if necessary </th>
<th>`rules/package-config.mk`</th></tr>
<tr><th>`$1_$2_HS_SRC_DIRS`</th>
<th> dirs relative to `$1` containing source files </th>
<th>`$1/$2/package-data.mk`</th></tr>
<tr><th>`$1_$2_CPP_OPTS`</th>
<th> CPP options </th>
<th>`$1/$2/package-data.mk`</th></tr>
<tr><th>`<file>_HC_OPTS`</th>
<th> GHC options for this source file (without the extension) </th>
<th>`$1/$2/ghc.mk`</th></tr></table>

<sup>\*</sup> Note: this now appears to be outdated -- some variable definitions have been moved to other files.
                          

## Intermediate goals


Following the divide-and-conquer principle, we split the big goal into a number of less ambitious ones below. More intermediate goals will be added here as the project progresses.

### Mining variables


A parser for (a subset of) makefiles is being implemented in order to mine all variable definitions and associated conditions from the existing makefiles (876 makefiles found in the entire GHC tree). The definitions are to be further converted to Haskell code in a semi-automated way.


System related `@variables@` which are expanded by `configure` are to be placed in `default.config.in` file that will be processed by `configure` to produce `default.config` file that will be read by the `Shake` build system. GHC developers can override some of the default settings using the `user.config` file, whose role will correspond to that of `build.mk` in the current build system.

### Shaking up a library


The first intermediate goal is to choose a library and build it with `Shake`. This will be tested by running the existing build system, removing all the built stuff for this particular library, and then restoring it with `Shake`, hopefully getting the same result. It was decided to choose a library without `cbits` and `#include`'s for the first attempt; `libraries/haskell2010` seems like a good candidate. The build code should be sufficiently generic to handle all other libraries without much rewriting.

## How to contribute


Please email to `andrey.mokhov@ncl.ac.uk` who coordinates the efforts if you'd like to contribute or have any comments/suggestions.