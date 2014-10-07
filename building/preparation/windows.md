# Building GHC on Windows


This page documents the instructions for setting up a Windows build using [ MSYS2](http://sourceforge.net/projects/msys2/), which is a fairly complete build of MinGW + the MSYS2 tools.


This guide should get you running in \~5 minutes, modulo download speeds.

## msys2 setup


Download and run the [ msys2 installer (64-bit)](http://sourceforge.net/projects/msys2/files/latest/download), or the [ 32-bit version](http://sourceforge.net/projects/msys2/files/Base/i686/msys2-i686-20140910.exe/download). Open a mingw64 shell.


IMPORTANT: the msys2 installer creates multiple shortcuts, "MSYS2 Shell", "MinGW-w64 Win32 Shell" and "MinGW-w64 Win64 Shell". You need the latter one (or the middle one for 32-bit). The MSYS2 shell is set up for building applications with Cygwin which provides an additional POSIX compatibility layer, while MinGW is set up for building native Windows applications which is what we need for GHC. 


An easy way to check that you are running the right shell is to check the output of `echo $PATH`. The first item of the list should be `/mingw64/bin`. Also, `echo $MSYSTEM` should show either `MINGW32` or `MINGW64`.

## Installing packages & tools


The msys2 package uses `pacman` (the venerable ArchLinux package manager) to manage packages. Once you're set up, upgrade everything, and install system dependencies required for building GHC:

```wiki
pacman -Syu  # upgrade all packages to the newest available version
pacman -S git curl tar binutils autoconf make libtool automake mingw-w64-x86_64-gcc
```


(Problems with PGP keys? Try `pacman-key --init` and `pacman-key --populate msys2`)


If you want to run tests, you will need to install a Windows version of [ Python 2](https://www.python.org/download/releases/2.7.8/). Python is only used by the test runner though and is not necessary for building GHC.

## Host GHC setup


A host GHC binary is required for bootstrapping. Let's download and install a prebuilt GHC into /usr/local:

```wiki
curl http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-$(uname -m)-unknown-mingw32.tar.xz | tar -xJ -C /tmp &&
mkdir -p /usr/local &&
mv /tmp/ghc-7.8.3/* /usr/local &&
rmdir /tmp/ghc-7.8.3
```

## Cabal setup


Building ghc requires [ Alex](http://www.haskell.org/alex/) and [ Happy](http://www.haskell.org/happy/). It is easiest to install them using cabal. We will also put them in /usr/local to make sure that they end up on $PATH.

```wiki
curl http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-mingw32.tar.gz | tar -xz -C /usr/local/bin &&
cabal update &&
cabal install -j --prefix=/usr/local alex happy
```

## A Quick Build


You should now be able to build GHC:

```wiki
cd ~ &&
git clone --recursive git://git.haskell.org/ghc.git &&
cd ghc &&
git clone git://git.haskell.org/ghc-tarballs.git ghc-tarballs &&
./boot &&  # Consider setting up mk/build.mk here.
./configure &&
make -j5
```


Alternatively, just run:

```wiki
./validate
```

## Other documentation


Other documentation for Windows includes:

- [MinGW/MSYS/Cygwin](building/platforms/windows) information for people new to using UNIX tools on Windows.
- [Using MSYS1](building/preparation/windows/msy-s1) to build GHC (not recommended any more)
- [Using Cygwin](building/windows/cygwin) to build GHC.
- [Using SSH](building/windows/ssh) on Windows.
- [ Guidance on how to use Haskell on Windows](http://www.haskell.org/haskellwiki/Windows)