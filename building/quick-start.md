# Quick Start: building and installing GHC


If you are an aspiring GHC developer, you may want to read the [Contributing](contributing) page first.


To build GHC, first make sure to:

- [Prepare your machine](building/preparation)


For Windows users, all the commands below must be executed in the MinGW shell, not Command Prompt nor PowerShell.


Then, get the sources by cloning GHC's main Git repository or via an alternative method (see [Getting the Sources](building/getting-the-sources) for more details). Note: you can connect to GitLab using SSH, if you register to GitLab and upload your SSH key. In that case the following line would have "git:" instead of "https:"

```
git clone --recursive https://gitlab.haskell.org/ghc/ghc
cd ghc/
```


Unless you want to build the latest development version of GHC, [checkout a stable release](building/quick-start#).


Optionally, you can customize your build by creating the file `mk/build.mk` using `mk/build.mk.sample` as a template and e.g. removing the comment marker \# on the line "BuildFlavour = devel2".


If you obtained the repository via `git clone --recursive`, you will need to first run:

```
$ ./boot
```

<sub>This step isn't necessary if you obtained the source from a tar archive.</sub>

The ghc binary is required in order to build ghc. If you don't have it, install it through [ghcup](https://www.haskell.org/ghcup/) or the [Haskell platform](https://www.haskell.org/platform/).

Next, run the `./configure` script followed by `make` to start the build:

```
$ ./configure # Windows users must append "--enable-tarballs-autodownload"
$ make -j8 # Use 8 parallel jobs; adapt to actual number of cpu cores
```

<sub>On Windows you need to download some binary distributables before being able to build.  This only has to be done once and can be done by adding the `--enable-tarballs-autodownload` flag to the call to `./configure`.</sub>


By default (without any `mk/build.mk` customization), this will do a 2-stage bootstrap build of the compiler, with profiling libraries.

## Run without installing


You can find the binaries built by make in the `inplace/bin` directory under the root of the ghc source tree. The binaries can be run from here without installing.

## Installing


After building, to install GHC (by default in `/usr/local`):

```
$ make install
```


You may need to use `sudo`.

## Fast rebuilding


There are 4 things to remember:

1. Select `BuildFlavour = devel2` in your `mk/build.mk` file (copy `mk/build.mk.sample` to `mk/build.mk` first), to
  [make GHC build more quickly](building/using#how-to-make-ghc-build-quickly).

1. Don't run `make` directly in the ghc root directory (unless you just pulled in changes from others). Instead, first
  change to the directory (`compiler`, `utils`, `ghc` or `libraries`) where you're making your changes.
  See [Building a single sub-component](building/using#).

1. Set `stage=2` in your `mk/build.mk` file, to
  [freeze the stage 1 compiler](building/using#freezing-stage-1).
  This makes sure that only the
  [stage-2](building/architecture/idiom/stages) compiler will be
  rebuilt after this.

1. While in the sub-component directory, use `make fast` [skip dependency building](building/using#skip-dependency-building) (except after pulling in changes from others).


A good first sanity check is to twiddle some error message in the code, just to see that changed error message pop up when you compile a file. Write some Haskell code with an error in it, and look at the error message. Search through the code for that error message. Change the message, rebuild ghc (run `make fast` in the `ghc` directory), and recompile your file again with `./inplace/bin/ghc-stage2`. If you see the changed message, you're good to go.

## Check out a Stable Release


You can learn what branches are available via ```git branch -a```. You can e.g. switch to the latest stable release of GHC-8.6.x:

```
git checkout ghc-8.6
```


You'll then have to update the git submodules:

```
git submodule update --init
```

## More information


Has your question not been answered? See [Building](building) for more resources. Also, don't hestitate to [ask for help](mailing-lists-and-irc).
