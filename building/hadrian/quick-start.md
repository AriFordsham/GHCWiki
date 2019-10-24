# Quick Start to building GHC with Hadrian


Below are quick instructions for building GHC with Hadrian.


**The following instructions assume that you have [got the sources](building/getting-the-sources) and [installed the necessary tools](building/preparation).**  In particular for Windows users, all the commands below must be executed in the MinGW shell, not Command Prompt or PowerShell. The commands given below should be executed from the root of ghc's source tree.


Hadrian is much younger than GHC's Make-based build system. If you need a feature supported by the Make build system but not by Hadrian, or more generally if you encounter any problem, please let us know [on the issue tracker](https://gitlab.haskell.org/ghc/ghc/issues).

## Your first build

On Linux/OS X:

```
$ ./boot && ./configure && ./hadrian/build.sh -j
```

On Windows:

```
$ ./boot && ./configure && hadrian\build.bat -j
```



These commands should be run from the root of ghc's source tree. The `-j` flag's meaning is the same as in the Make build system, it tells hadrian to build two or more targets in parallel, whenever possible.


If the build succeeds, you will find your stage 2 GHC at `_build/stage1/bin/ghc`. More generally, everything generated and built by the stage 0 (resp. stage 1) compiler lives under `_build/stage0/` (resp `_build/stage1/`)

## Alternative build scripts

If the default build script doesn't work on your system for some reason, you might want to give a try to another one, e.g. based on cabal (`hadrian/build.cabal.sh`), Stack (`hadrian/build.stack.sh`, `hadrian/build.stack.bat`, `hadrian/build.stack.nix.sh`) or the global package database (`hadrian/build.global-db.sh`, `hadrian/build.global-db.bat`).

Windows users might want to read through [building GHC on Windows using Stack](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/windows.md).

**From now on, this page assumes you have found a build script that works for you and will refer to it as just `build`.**

## Anatomy of a GHC build

GHC is a self-hosted compiler and consequently the build proceeds in several
stages:

1. The build begins with a user-provided installation of GHC called the
   stage0 (or bootstrap) compiler which is used (via the `build.*.sh` scripts)
   to build Hadrian.
2. Hadrian uses the stage0 compiler to build a stage1 compiler (found in `_build/stage0
   /bin/ghc`, because it's built by the stage0 compiler), linking against the stage0
   compiler's core libraries (e.g. `base`).
3. The stage1 compiler is used to build new core libraries (found in
   `_build/stage1/lib`).
4. The stage1 compiler is used to build a stage2 compiler (found in
   `_build/stage1/bin/ghc`), linking against these new core libraries.

Note that the stage directories in the `_build` directory can be thought of as
named after the stage that was used to *build* the artifacts in each directory.

These stages can be summarized graphically:

![an overview of the stages of a Hadrian compilation](https://gitlab.haskell.org/ghc/ghc/raw/9a2798e139e3d20183b59bb5a66012db495c66c7/hadrian/doc/staged-compilation.svg)

## Important commands

For GHC hackers already used to the Make build system, here is what you need to know to get started with Hadrian:

- You can still boot and configure yourself.
- Use `hadrian/build.{sh, bat}` instead of `make`. It supports `-j`.
- Add the `-c` flag if you want hadrian to boot and configure the source tree for you.
- Build products are not in `inplace` anymore, but `_build` by default. Your stage 2 GHC would then be at `_build/stage1/bin/ghc` (because it's built by stage1).
- The build root is configurable with `--build-root` or `-o`.
- You can pick the build flavour with `--flavour=X` where X is `perf`, `prof`, etc.
- You can run tests with `build test`, and specific ones by adding `--only="T12345 T11223"` for example.
- GHCs built by Hadrian are now relocatable. This means you can move the `<build root>/stage1/{lib, bin}` directories around and GHC will still happily work, as long as both directories stay next to each other.

Below is a list of Hadrian commands and the description of what they do. We also give the equivalent Make command whenever possible in the first column, as it will be familiar to all the GHC developers used to the Make build system.

| Make command | Hadrian command | description |
|--------------|-----------------|-------------|
| `make`       | `build`         | Build a complete stage2 compiler with libraries and with the default flavour (`perf`) |
| `make inplace/bin/ghc-stage1` | `build _build/stage0/bin/ghc` | Build a stage1 GHC executable |
| `make inplace/bin/ghc-stage2` | `build _build/stage1/bin/ghc` | Build a stage2 GHC executable  |
|`make inplace/lib/package.conf.d/text-1.2.3.0.conf` | `build _build/stage1/lib/package.conf.d/text-1.2.3.0.conf` | Build and register text-1.2.3.0.conf with the stage2 haskell compiler |
| `make inplace/lib/platformConstants` | `build _build/stage1/lib/platformConstants` | Generate the `platformConstants` file to be used for stage 2 GHC |
| `make libraries/base/dist-install/build/libHSbase-4.12.0.0.a` | `build _build/stage1/libraries/base/build/libHSbase-4.12.0.0.a` | Build static library for <tt>base</tt> with the stage 1 compiler |
| `make docs` | `build docs` | Generate haddocks, user guide and more |
| `make BUILD_SPHINX_HTML=NO BUILD_SPHINX_PDF=NO` | `build _build/docs/html/libraries/index.html` | Generate haddocks only |
| `cd nofib; make clean ; make boot ; make 2>&1 \| tee nofib-log` | `build nofib` | Run the nofib suite |
| `make BuildFlavour=quickest` | `build --flavour=quickest` | Build a `quickest`-flavoured stage 2 compiler |
| `make test` | `build test` | Run the testsuite |
| `make test TEST=abcd` | `build test --only=abcd` | Run only the test named `abcd` |
| `make binary-dist` | `build binary-dist` | Build a complete binary distribution |
| `make source-dist` | `build source-dist` | Build a complete source distribution |
| N/A | `build --build-root=ghc-T9999` | Build a complete stage2 compiler under a user specified build root (`./ghc-T9999/` here). Hadrian places all build artifacts under that directory |

Note that `build test` is quite flexible and can take various arguments, to accomplish all the tasks that the Make build system's `test` rule can handle. Head to [this document](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/testsuite.md) for more on Hadrian's `test` rule.

## Loading GHC in GHCi (fast feedback loop)

Hadrian comes with a script that lets you load the `ghc` library as well as the code for the executable, at `hadrian/ghci.sh`. This is a lot less work than a complete build, and you can pass arguments like `-j` to that script to distribute the work for the initial module loading. Note however that this only typechecks the code and you therefore cannot run any function from GHC in that ghci session.

``` sh
# for example:
$ hadrian/ghci.sh -j4
```

You can then edit the source code and type `:r` in ghci whenever you want to see if your changes typecheck.



## User settings

The Hadrian equivalent of `mk/build.mk` in the make build system is the `hadrian/src/UserSettings.hs` module, where you can customise Hadrian to define a custom build flavour tailored to your needs, change the command line options passed to the various build tools under some specific circumstances, define new packages to be built, enable/disable profiling ways and much more. Hadrian alternatively supplies a key-value style interface to those settings, allowing users to customise builds by specifying a few key-value expressions from the Hadrian command itself or a dedicated file, à là `mk/build.mk`.

Both approaches are documented with various examples [here](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md).

## Command line options

In addition to standard Shake flags (try `--help`), the build system
currently supports several many others, among which the important ones described below. Please refer to `build --help` for a complete listing of the command line arguments supported by hadrian.

- `--build-root=PATH` or `-oPATH`: specify the directory in which you want to store all the build artifacts. If none is specified by the user, hadrian will store everything under `_build/` at the top of ghc's source tree. Unlike GHC's make build system, hadrian doesn't have any "inplace" logic left anymore. This option is therefore useful for GHC developers who want to build GHC in different ways or at different commits, from the same directory, and have the build products sit in different, isolated folders.

- `--configure` or `-c`: use this flag to run the `boot` and `configure` scripts automatically, so that you don't have to remember to run them manually as you normally do when using Make (typically only in the first build):

  ```
  ./boot
  ./configure # On Windows run ./configure --enable-tarballs-autodownload
  ```

>
>
> Beware that with this flag Hadrian may do network I/O on Windows to download necessary tarballs, which may sometimes be undesirable.
>
>

- `--flavour=FLAVOUR`: choose a build flavour. The following settings are currently supported: `default`, `quick`, `quickest`, `perf`, `prof`, `devel1` and `devel2`. As an example, the `quickest` flavour adds `-O0` flag to all GHC invocations and builds libraries only in the `vanilla` way, which speeds up builds by 3-4x. Build flavours are documented [here](https://github.com/snowleopard/hadrian/blob/master/doc/flavours.md).

- `--freeze1`: freeze Stage1 GHC, i.e. do not rebuild it even if some of its source files are out-of-date. This allows to significantly reduce the rebuild time when you are working on a feature that affects both Stage1 and Stage2 compilers, but may lead to incorrect build results. To unfreeze Stage1 GHC simply drop the `--freeze1` flag and Hadrian will rebuild all out-of-date files.

- `--integer-simple`: build GHC using the `integer-simple` integer library (instead of `integer-gmp`).

- `--progress-colour=MODE`: choose whether to use colours when printing build progress info. There are three settings: `never` (do not use colours), `auto` (attempt to detect whether the console supports colours; this is the default setting), and `always` (use colours).

- `--progress-info=STYLE`: choose how build progress info is printed. There are four settings: `none`, `brief` (one line per build command; this is the default setting), `normal` (typically a box per build command), and `unicorn` (when `normal` just won't do).

- `--split-objects`: generate split objects, which are switched off by default. Due to a GHC [bug](https://gitlab.haskell.org/ghc/ghc/issues/11315), you need a full clean rebuild when using this flag.

- `--verbose`: run Hadrian in verbose mode. In particular this prints diagnostic messages by Shake oracles, useful when debugging hadrian.

- `--docs[=TARGET]`: strip down docs targets to build (possible values: `none`, `no-haddocks`, `no-sphinx[-{html, pdfs, man}]`).

- `--test-compiler[=TEST_COMPILER]`: use the given compiler (default is stage2) for running the testsuite, when executing the `test` target. `TEST_COMPILER` can be `stage1, stage2` or even the path to some arbitrary ghc executable.

- `--only[=TESTS]`: only run the given test cases, e.g `--only="T123 T456"`.

- `--skip-perf`: skip performance tests.

- `--only-perf`: only run performance tests.

- `--test-speed[=SPEED]`: `fast`, `slow` or `normal` (Normal by default). They respectively correspond to `make fasttest`, `make slowtest` and `make test`.

- `--summary[=TEST_SUMMARY]`: where to output the textual testsuite summary

- `--test-way[=TEST_WAY]`: only run these test ways, e.g `--test-way=threaded2`. 

- `-a` or `--test-accept`: accept new output of tests (by updating the appropriate `.stdout`/`.stderr` files)

- `--test-verbose[=TEST_VERBOSE]`: a verbosity value between 0 and 5. 0 is silent, 4 and higher activates extra output from the testsuite driver.

- `--summary-junit[=TEST_SUMMARY_JUNIT]`: where to output the XML test summary file (JUnit format)

- `--test-root-dirs[=DIR1:[DIR2:...:DIRn]]`: directories to scan for `all.T` files, in which all our tests are declared (default: all directories).

- `-k` or `--keep-test-files`: keep all the files generated when running the testsuite.

Hadrian can also take user settings as CLI arguments, see [the documentation](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md#key-value-and-key-value-style-settings) for more details about them.

## Contributing

Hadrian is part of [the main GHC git repository](https://gitlab.haskell.org/ghc/ghc) and lives under the `hadrian/` directory. You can therefore file Hadrian bug reports or feature requests [on GHC's issue tracker](https://gitlab.haskell.org/ghc/ghc/issues) (with the ~hadrian tag) and [contribute patches](https://gitlab.haskell.org/ghc/ghc/wikis/Contributing-a-Patch) as for any other part of GHC. This section provides a brief introduction to Hadrian and gives pointers to help you find the information you need.

Hadrian is a build system for GHC, written in Haskell, around Shake ([website](https://shakebuild.com/), [hackage](https://hackage.haskell.org/package/Shake), [paper](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf)), a library for writing build systems. It might be useful for potential Hadrian contributors to take at least a quick look at the documentation or the paper to get an idea of what Shake build systems look like, on simple examples, since Hadrian uses this library all over the place.

Many modules in Hadrian are about declaring rules to build or generate a certain type of file (object files, static or shared libraries, executables, ...) using specific tools (C compiler, GHC, Haddock, ...) that we refer to as "builders". These modules live under `hadrian/src/Rules/`. These rules are also where the various dependencies between rules/files are declared, using the `need` function from Shake. For instance, `hadrian/src/Rules/Library.hs` contains the rules that specify how static/shared/ghci libraries should be built, and as part of this specification require the object files of the said libraries to be built (the library files therefore depend on the object files). Those object files in turn depend on the corresponding source files. Looking for all calls to Shake's `need` function should reveal all the places where we record dependencies from things that we are trying to produce (generated files, object files, libraries, executables, package database entries, testsuite run, ...) on things that are required to produce them.

We separated the code that calls the builders (`hadrian/src/Rules/...`) from the code that computes the command line arguments the said builders are going to receive from Hadrian. You can find the latters under `hadrian/src/Settings/`, in particular `hadrian/src/Settings/Builders/` and `hadrian/src/Settings/Packages.hs`. Finally, the flavours are defined under `hadrian/src/Settings/Flavours/`, except for the default flavour (the one Hadrian uses if you don't specify any `--flavour` argument on the command line) which is defined in `hadrian/src/Settings/Default.hs`.

A few additional comments to wrap up this introduction to Hadrian:

- Hadrian has a small EDSL for conditionally emitting command line arguments, predicated on various pieces of a given build context (package, stage, way, ...). It lies at the heart of the `hadrian/src/Settings/*` modules and is covered in depth by [a dedicated document](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/expressions.md).

- All the packages that Hadrian knows about are defined in `hadrian/src/Packages.hs`.

- Hadrian picks up a bunch of information from the `configure` script by having `./configure` turn `hadrian/cfg/system.cfg.in` into `hadrian/cfg/system.cfg`. This is how Hadrian learns about the path to various tools, information about host/target platforms and much more.

- The modules under `hadrian/src/Hadrian/Haskell/` handle the extraction of package descriptions from `.cabal` files, and their use for configuring, copying and registering packages (but not building -- this is handled by Hadrian itself).