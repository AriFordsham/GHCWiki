# Quick Start to building GHC with Hadrian


Below are quick instructions for building GHC with Hadrian.


The following instructions assume that you have [got the sources](building/getting-the-sources) and [installed the necessary tools](building/preparation).  In particular for Windows users, all the commands below must be executed in the MinGW shell, not Command Prompt or PowerShell. The commands given below should be executed from the root of ghc's source tree.


Hadrian is much younger than GHC's Make-based build system. If you need a feature supported by the Make build system but not by Hadrian, or more generally if you encounter any problem, please let us know [on the issue tracker](https://ghc.haskell.org/trac/ghc/newticket).

## tl;dr


For GHC hackers already used to the Make build system, here is what you need to know:

- You can still boot and configure yourself.
- Use `hadrian/build.{sh, bat}` instead of `make`. It supports `-j`.
- Add the `-c` flag if you want hadrian to boot and configure the source tree for you.
- Build products are not in `inplace` anymore, but `_build` by default. Your stage 2 GHC would then be at `_build/stage1/bin/ghc` (because it's built by stage1).
- The build root is configurable with `--build-root` or `-o`.
- You can pick the build flavour with `--flavour=X` where X is `perf`, `prof`, etc.
- You can run tests with `build test`, and specific ones by adding `--only="T12345 T11223"` for example.
- GHCs built by Hadrian are now relocatable. This means you can move the `<build root>/stage1/{lib, bin}` directories around and GHC will still happily work, as long as both directories stay next to each other.


You can now directly jump [here](building/hadrian/quick-start#) to see the table listing usual make commands and their hadrian equivalents.

## Your first build


On Windows:

```
$ hadrian\build.bat -c -j
```


On other systems:

```
$ ./hadrian/build.sh -c -j
```


These commands should be run from the root of ghc's source tree. The `-c` flag asks Hadrian to run ghc's `boot` and `configure` scripts and can therefore be left out if you've already been building that source tree with the Make build system. The `-j` flag's meaning is the same as in the Make build system, it tells hadrian to build two or more targets in parallel, whenever possible.


If the build succeeds, you will find your stage 2 GHC at `_build/stage1/bin/ghc`. More generally, everything generated and built by the stage 0 (resp. stage 1) compiler lives under `_build/stage0/` (resp `_build/stage1/`)

## Alternative build scripts


If the default build script doesn't work on your system for some reason, you might want to give a try to another one, e.g. based on Cabal sandboxes (`hadrian/build.cabal.sh`), Stack (`hadrian/build.stack.sh`, `hadrian/build.stack.bat`, `hadrian/build.stack.nix.sh`) or the global package database (`hadrian/build.global-db.sh`, `hadrian/build.global-db.bat`).


Windows users might want to read through [building GHC on Windows using Stack](https://github.com/snowleopard/hadrian/blob/master/doc/windows.md).

**From now on, this page assumes you have found a build script that works for you and will refer to it as just `build`.**

## Migrating from the Make build system


Below is an equivalence table between make and hadrian commands, with a short description of what the commands do.

<table><tr><th> make command </th>
<th> hadrian command </th>
<th> description 
</th></tr>
<tr><th> <tt>make</tt>         </th>
<th> <tt>build</tt>           </th>
<th> Build a complete stage2 compiler with libraries and with the default flavour (<tt>perf</tt>) 
</th></tr>
<tr><th> <tt>make inplace/bin/ghc-stage1</tt> </th>
<th> <tt>build _build/stage0/bin/ghc</tt> </th>
<th> Build a stage1 GHC executable 
</th></tr>
<tr><th> <tt>make inplace/bin/ghc-stage2</tt> </th>
<th> <tt>build _build/stage1/bin/ghc</tt> </th>
<th> Build a stage2 GHC executable 
</th></tr>
<tr><th> <tt>make inplace/lib/package.conf.d/text-1.2.3.0.conf</tt> </th>
<th> <tt>build _build/stage1/lib/package.conf.d/text-1.2.3.0.conf</tt> </th>
<th> Build and register text-1.2.3.0.conf with the stage2 haskell compiler 
</th></tr>
<tr><th> <tt>make inplace/lib/platformConstants</tt> </th>
<th> <tt>build _build/stage{0,1}/lib/platformConstants</tt> </th>
<th> Generate the <tt>platformConstants</tt> file to be used for stage 1 or stage 2 GHC 
</th></tr>
<tr><th> <tt>make libraries/base/dist-install/build/libHSbase-4.12.0.0.a</tt> </th>
<th> <tt>build _build/stage1/libraries/base/build/libHSbase-4.12.0.0.a</tt> </th>
<th> Build static library for <tt>base</tt> with the stage 1 compiler 
</th></tr>
<tr><th> <tt>make docs</tt> </th>
<th> <tt>build docs</tt> </th>
<th> Generate haddocks, user guide and more 
</th></tr>
<tr><th> N/A </th>
<th> <tt>build _build/docs/html/libraries/index.html</tt> </th>
<th> Generate haddocks only 
</th></tr>
<tr><th> <tt>make clean ; make boot ; make 2>&1 | tee nofib-log</tt> </th>
<th> <tt>build nofib</tt> </th>
<th> Run the nofib suite (building a compiler if necessary) 
</th></tr>
<tr><th> <i>Setting <tt>BuildFlavour=quickest</tt> in build.mk</i> </th>
<th> <tt>build --flavour=quickest</tt> </th>
<th> Build a <tt>quickest</tt>-flavoured stage 2 compiler 
</th></tr>
<tr><th> <tt>make test</tt> </th>
<th> <tt>build test</tt> </th>
<th> Run the testsuite 
</th></tr>
<tr><th> <tt>make test TEST=abcd</tt> </th>
<th> <tt>build test --only=abcd</tt> </th>
<th> Run only the test named <tt>abcd</tt> 
</th></tr>
<tr><th> N/A </th>
<th> <tt>build --build-root=ghc-T9999</tt> </th>
<th> Build a complete stage2 compiler under a user specified build root (<tt>./ghc-T9999/</tt> here). Hadrian places all build artifacts under that directory 
</th></tr></table>


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

- `--test-compiler[=TEST_COMPILER]`: use the given compiler (default is stage2) for running the testsuite, when executing the `test` target. `TEST_COMPILER` can be `stage1, stage2` or even the path to some arbitrary ghc executable.

- `--only[=TESTS]`: only run the given test cases, e.g `--only="T123 T456"`.

- `--skip-perf`: skip performance tests.

- `--test-speed[=SPEED]`: `fast`, `slow` or `normal` (Normal by default). They respectively correspond to `make fasttest`, `make slowtest` and `make test`.

- `--summary[=TEST_SUMMARY]`: where to output the test summary file.

- `--test-way[=TEST_WAY]`: only run these test ways, e.g `--test-way=threaded2`. 

## User settings


The equivalent of `mk/build.mk` in the make build system is the `hadrian/src/UserSettings.hs` module, where you can customise Hadrian to define a custom build flavour tailored to your needs, change the command line options passed to the various build tools under some specific circumstances, define new packages to be built, enable/disable profiling ways and much more.


Hadrian has an entire document dedicated to this topic, with various examples. You can find it [here](https://github.com/snowleopard/hadrian/blob/master/doc/user-settings.md).

## Build targets to be documented

```
# phony target for producing a binary distribution
# note: they are not as complete as the ones the make build
#       system produces yet.
$ build binary-dist

# phony target for producing a source distribution
$ build source-dist

# phony target for running validate
# note: those are quite incomplete and buggy at the moment,
#       see https://github.com/snowleopard/hadrian/issues/187 
$ build validate
```
