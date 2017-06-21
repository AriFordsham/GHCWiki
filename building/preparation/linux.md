# Setting up a Linux system for building GHC


If you're on a recent Linux system, then you should be able to get a working build environment by installing the following packages using your system's package manager.

## Docker


If you are familiar with docker, this is a 1 step install for a development image (ghc build requirements plus a few development related tools).
First cd into your ghc directory that you checkout according to [Building/GettingTheSources](building/getting-the-sources)

```wiki
     docker run --rm -i -t -v `pwd`:/home/ghc gregweber/ghc-haskell-dev /bin/bash
```


Thats it!
This mounts your ghc source code into the docker container.
This way you can still hack on GHC with Emacs, etc, but you are just building from the docker container.
Note that `arc` (the ghc patch submission tool) is installed in the image (although you can also use it from your docker host) along with vim-tiny for editing commit messages.


Send pull requests to [ https://github.com/gregwebs/ghc-docker-dev](https://github.com/gregwebs/ghc-docker-dev) if something is out-of-date.

## Fedora


Install the [ required tools](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools) using the following command for Fedora 22 and later (for earlier versions of Fedora, use `yum` instead of `dnf`):

```wiki
   sudo dnf install glibc-devel ncurses-devel gmp-devel autoconf automake libtool gcc make perl python ghc happy alex git
```


For building the documentation: (User's Guide and Cabal guide):
(optional)

```wiki
   # GHC > 7.10 (untested)
   sudo dnf install python-sphinx
   # GHC <= 7.10
   sudo dnf install docbook-utils docbook-utils-pdf docbook-style-xsl
```


other  packages that are useful for development:
(optional)

```wiki
   sudo dnf install strace patch
```


Now the system should be ready to build GHC.


For a quickstart, follow the commands listed under:

[ https://github.com/ghc/ghc\#building--installing](https://github.com/ghc/ghc#building--installing)

## Debian, Ubuntu, and other Debian-based systems


You can make sure you have all dependencies by

```wiki
   sudo apt-get build-dep ghc
```


But this might install some packages you do not use in your system (e.g. `java`, `docbook`, `xsltproc`).  Alternatively install the following:

```wiki
   sudo apt-get install haskell-platform git autoconf automake libtool make libgmp-dev ncurses-dev g++ python3 bzip2
```


(`ncurses-dev` is needed by the `terminfo` package, and `g++` is needed by a couple of tests, `ghcilink003` and `ghcilink006`).


Optional: install llvm from [ http://apt.llvm.org](http://apt.llvm.org) (only necessary to make the `-fllvm` flag work). [Commentary/Compiler/Backends/LLVM/Installing](commentary/compiler/backends/llvm/installing#llvm-support) will tell you which version to install.


Due to the nature of Debian, you may have difficulty building GHC \>7.6 due to version incompatibilities with the Happy and Alex packages.  To alleviate this issue simply install both packages using the haskell-platform provided cabal.

```wiki
   cabal install alex happy
```


For building the documentation (User's Guide):

```wiki
   # GHC > 7.10:
   sudo apt-get install python-sphinx texlive-xetex texlive-fonts-recommended fonts-lmodern texlive-latex-recommended texlive-latex-extra
   # GHC <= 7.10:
   sudo apt-get install dblatex docbook-xsl docbook-utils libxml2-utils texlive-font-utils
```


other packages that are useful for development:

```wiki
   sudo apt-get install linux-tools-generic xutils-dev
```


The package `linux-tools` includes `perf`, see [Debugging/LowLevelProfiling/Perf](debugging/low-level-profiling/perf). The package `xutils-dev` provides the `lndir` program, need for running `make sdist` and useful for maintaining a separate build tree, see [Building/Using](building/using).


For [validating patches](testing-patches) :

```wiki
   # GHC >= 8.2:
   sudo apt-get install python3
```

## Arch


Install the [ required tools](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools):

```wiki
   sudo pacman -Sy ghc perl gcc make happy alex cabal-install autoconf automake python python-sphinx libedit
```

## Nix/NixOS


First, you need to clone Nixpkgs, so you could use recent Nix recipes:

```wiki
   git clone https://github.com/NixOS/nixpkgs  
```


Then, you can build the environment needed for compiling HEAD (assuming that the `nixpkgs` directory is in `/home/user`):

```wiki
   cd ~
   nix-shell '<nixpkgs>' -A haskell.compiler.ghcHEAD
```


Finally, clone, configure, and build GHC (see [Newcomers](newcomers) for details), but replace the usual `configure && make` with the Nix build phases:

```wiki
   git clone --recursive https://github.com/ghc/ghc
   cd ghc/
   # patchPhase (sometimes this is necessary if there are patches which need to be applied as of Dec 2016 with the unstable channel it is unnecessary)

   #edit mk/build.mk.sample as normal
   # This command needs to be used so that the right paths to libraries are passed to ./configure
   configurePhase 
   buildPhase
   # edit build.mk to remove the comment marker # on the line stage=2
```


If you have come this way it is assumed you know what you are doing! The `ghcHEAD` derivation does not include the dependencies needed for the test suite. I (mpickering, Dec 2016) don't know exactly what these dependencies are but you should add them to your environment by defining a suitable `shell.nix`.  


Enable parallel builds in the nix build environment:

```wiki
   export NIX_BUILD_CORES=4
```