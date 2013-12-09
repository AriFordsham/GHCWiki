
This page documents the instructions for setting up a Windows build using [ msys2](http://sourceforge.net/projects/msys2/files/Alpha-versions/), which is a fairly complete build of MinGW + the msys tools. It is pretty self contained and fixes several pesky bugs with the traditional implementation. It's also smaller and has a convenient package manager, `pacman`.

*NB*: You can extract `.tar.xz` files with 7Zip: [ http://www.7-zip.org/](http://www.7-zip.org/)

## 32bit Windows

- Download this package: [ http://sourceforge.net/projects/msys2/files/Base/i686/msys2-base-i686-20131208.tar.xz/download](http://sourceforge.net/projects/msys2/files/Base/i686/msys2-base-i686-20131208.tar.xz/download)
- Download 32bit GHC 7.6.3: [http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-i386-unknown-mingw32.tar.bz2](http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-i386-unknown-mingw32.tar.bz2)


Extract the msys2 base package somewhere like **C:\\msys32**


Extract the GHC binary somewhere like **C:\\msys32\\ghc-7.6.3**


Launch the shell with **C:\\msys32\\mingw32_shell.bat**

## 64bit Windows

- Download this package: [ http://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-base-x86_64-20131208.tar.xz/download](http://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-base-x86_64-20131208.tar.xz/download)
- Download 64bit GHC 7.6.3: [http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-mingw32.tar.bz2](http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-mingw32.tar.bz2)


Extract the msys2 base package somewhere like **C:\\msys64**


Extract the GHC binary somewhere like **C:\\msys64\\ghc-7.6.3**


Launch the shell with **C:\\msys64\\mingw64_shell.bat**

## Setting up your PATH


Create a `~/bin` directory for the `cabal.exe` (and whatever else you want.) Also, put GHC in your `$PATH` by appending the following to `~/.bashrc`:

```wiki
$ mkdir ~/bin
$ echo 'export PATH=/ghc-7.6.3/bin:$PATH' >> ~/.bashrc
$ echo 'export PATH=$HOME/bin:$PATH'      >> ~/.bashrc
```


We'll also go ahead and add the default `cabal.exe` binary installation path:

```wiki
$ echo 'export PATH=/c/Users/YourUserNameHere/AppData/Roaming/cabal/bin:$PATH' >> ~/.bashrc
```


Make sure you restart your shell.

## Installing packages & tools


The msys2 package uses `pacman` (the venerable ArchLinux package manager) to manage packages. Once you're set up, upgrade everything, and install some dependencies:

```wiki
$ pacman -Syu
$ pacman -S python2 git wget tar gzip binutils gcc autoconf make libtool automake
```


Now install a `cabal.exe` prebuilt binary:

```wiki
$ wget http://www.haskell.org/cabal/release/cabal-install-1.18.0.2/cabal.exe
$ mv cabal.exe ~/bin
$ cabal update
```


Make sure your `$PATH` now contains the cabal installation path: `/c/Users/YourUserNameHere/AppData/Roaming/cabal/bin`, which will take precedence over earlier things:


Install `happy` and `alex`:

```wiki
$ cabal install happy alex
```

## A Quick build


You should now be able to build GHC:

```wiki
$ cd ~
$ git clone https://github.com/ghc/ghc.git
$ cd ghc && ./sync-all --testsuite --nofib --extra get
$ ./boot && ./configure
$ make -j5
```


But be sure to read on for more!
