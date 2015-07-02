# Building GHC on Windows


This page documents the instructions for setting up a Windows build using [ MSYS2](http://sourceforge.net/projects/msys2/), which is a fairly complete build of [ MinGW-w64](http://mingw-w64.org/) + the MSYS2 tools.


This guide should get you running in \~5 minutes, modulo download speeds.

## MSYS2 setup

### 64-bit


Download and run the [ msys2 installer (64-bit)](http://sourceforge.net/projects/msys2/files/latest/download). Be sure to open a mingw64 shell (see below).

### 32-bit


Download and run the [ 32-bit MSYS2 installer](http://sourceforge.net/projects/msys2/files/Base/i686/). Be sure to open a mingw32 shell (see below).


The result of attempting to create a 32-bit build on a 64-bit machine has not been documented yet. Building on a 32-bit version of Windows works, of course.

### Configuring MinGW properly

**IMPORTANT:** The MSYS2 installer creates multiple shortcuts, "MSYS2 Shell", "MinGW-w64 Win32 Shell" and "MinGW-w64 Win64 Shell". You do **not** want the "MSYS2 Shell." The MSYS2 shell is set up for building applications with Cygwin which provides an additional POSIX compatibility layer, while MinGW is set up for building native Windows applications which is what we need for GHC. 


An easy way to check that you are running the right shell is to check the output of `echo $MSYSTEM`. It should show either `MINGW32` or `MINGW64`. You can also tell by examining the `$PATH`.


Consider upgrading all installed packages to the latest versions. See [ MSYS2 installation instructions](http://sourceforge.net/p/msys2/wiki/MSYS2%20installation/) (section III) for details.

## Installing packages & tools


The msys2 package uses `pacman` (the venerable ArchLinux package manager) to manage packages. Let's install system dependencies required for building GHC:

```wiki
pacman -Sy git tar binutils autoconf make libtool automake python2 p7zip patch gcc docbook-xsl
```


Note: `dblatex` isn't available as a [ MSYS2 package](https://github.com/Alexpux/MSYS2-packages), so the user's guide won't be build in ps and pdf format (only html).
  

## Host GHC setup


A host GHC binary is required for bootstrapping. Let's download and install a prebuilt GHC into `/usr/local`:

```wiki
curl -L http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-$(uname -m | sed -e s/686/386/)-unknown-mingw32.tar.xz | tar -xJ -C /tmp &&
mkdir -p /usr/local &&
mv /tmp/ghc-7.8.3/* /usr/local &&
rmdir /tmp/ghc-7.8.3
```

## Cabal setup


Building ghc requires [ Alex](http://www.haskell.org/alex/) and [ Happy](http://www.haskell.org/happy/). It is easiest to install them using cabal. We will also put them in `/usr/local` to make sure that they end up on $PATH.

```wiki
curl -L http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-mingw32.tar.gz | tar -xz -C /usr/local/bin &&
cabal update &&
cabal install -j --prefix=/usr/local alex happy
```

## A Quick Build


You should now be able to build GHC:

```wiki
cd ~ &&
git clone --recursive git://git.haskell.org/ghc.git &&
cd ghc &&
git clone git://git.haskell.org/ghc-tarballs.git
```


Consider setting up `mk/build.mk` here (`cp mk/build.mk.sample mk/build.mk && vim mk/build.mk`).


Finally, to perform the actual build:

```wiki
./boot &&
./configure &&
make
```

<sub>Running parallel make (e.g., make -j5) is faster, but appears to cause segfaults during the build sometimes. The reasons are not clear yet.</sub>


Msys2 is known to be glitchy in some situations. If you see errors related to fork(), try closing and reopening the shell; see also [ msys2 issue \#74](http://sourceforge.net/p/msys2/tickets/74/). Also there have been issues with the build process segfaulting. The reason is not known (we're looking into it). If that happens, simply rerunning `make` will continue the build process.


Alternatively, to run a pristine build and tests (takes a while):

```wiki
./validate
```

**NOTE**: You may see an error like `make 7628 child_info_fork::abort: ... make: fork: Resource temporarily unavailable` when running `make`. To fix this, go to the root of your MSYS dir and run `autorebase.bat`; see [ http://sourceforge.net/projects/mingw/files/MSYS/Extension/rebase/rebase-4.0.1_1-1/](http://sourceforge.net/projects/mingw/files/MSYS/Extension/rebase/rebase-4.0.1_1-1/) and again [ http://sourceforge.net/p/msys2/tickets/74/](http://sourceforge.net/p/msys2/tickets/74/). Alternatively, run `shutdown //r`.

## Other documentation


Other documentation for Windows includes:

- [MinGW/MSYS/Cygwin](building/platforms/windows) information for people new to using UNIX tools on Windows.
- [Setting up a SSH Daemon](building/windows/sshd) on CygWin/MinGW and let's you treat Windows as yet another remote SSH session.
- [Using MSYS1](building/preparation/windows/msy-s1) to build GHC (not recommended any more)
- [Using Cygwin](building/windows/cygwin) to build GHC. (no longer supported)
- [Using SSH](building/windows/ssh) on Windows.
- [ Guidance on how to use Haskell on Windows](http://www.haskell.org/haskellwiki/Windows)