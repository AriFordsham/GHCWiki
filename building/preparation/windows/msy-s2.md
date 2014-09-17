# Building GHC on msys2


This page documents the instructions for setting up a Windows build using [ msys2](http://sourceforge.net/projects/msys2/), which is a fairly complete build of MinGW + the msys tools. It is self contained and fixes several pesky bugs with the traditional implementation. It's also smaller and has a convenient package manager, `pacman`.


This guide should get you running in \~5 minutes, modulo download speeds.

## msys2 setup


Download and run the [ msys2 installer (64-bit)](http://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-x86_64-20140910.exe/download), or the [ 32-bit version](http://sourceforge.net/projects/msys2/files/Base/i686/msys2-i686-20140910.exe/download). Open an msys2 shell.

## Installing packages & tools


The msys2 package uses `pacman` (the venerable ArchLinux package manager) to manage packages. Once you're set up, upgrade everything, and install system dependencies required for building GHC:

```wiki
pacman -Syu
pacman -S git curl tar binutils autoconf make libtool automake python2
```

## Host GHC setup


A host GHC binary is required for bootstrapping. Let's download and install a prebuilt GHC into /opt:

```wiki
curl http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-$(uname -m)-unknown-mingw32.tar.xz | tar -xJ -C /tmp &&
mv /tmp/ghc-7.8.3/* /opt &&
rmdir /tmp/ghc-7.8.3
```

## Cabal setup


Building ghc requires [ Alex](http://www.haskell.org/alex/) and [ Happy](http://www.haskell.org/happy/). It is easiest to install them using cabal:

```wiki
curl http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-mingw32.tar.gz | tar -xz -C /opt/bin &&
cabal install -j --prefix=/opt alex happy
```

## A Quick build


You should now be able to build GHC:

```wiki
cd ~ &&
git clone --recursive git://git.haskell.org/ghc.git &&
cd ghc &&
git clone git://git.haskell.org/ghc-tarballs.git ghc-tarballs &&
./boot && ./configure &&
make -j5
```


Alternatively, just run:

```wiki
./validate
```


Note: running tests does not work on msys2 due to issues in the test runner and in the bundled version of gcc (fixes pending).
