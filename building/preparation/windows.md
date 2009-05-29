# Setting up a Windows system for building GHC


Installing the following will get you a working build environment with MSYS (alternatively, [install Cygwin](building/windows/cygwin)).  For your convenience we've cached a working set of build tools that you can download.  Note: do not install anything in a directory that contains spaces, because the GHC build system is not capable of handling paths with spaces in.

- First install a recent stable version of [GHC](http://www.haskell.org/ghc/download.html).

- Install MinGW: [http://www.haskell.org/ghc/tools/Win32/MinGW-5.1.4.exe](http://www.haskell.org/ghc/tools/Win32/MinGW-5.1.4.exe).  When the installer asks you which version to install, choose "Current".

- The `windres` program that comes with MinGW isn't compatible with GHC.  Download a later version: [http://www.haskell.org/ghc/tools/Win32/windres.exe](http://www.haskell.org/ghc/tools/Win32/windres.exe) and put it in `c:/mingw/bin` (or wherever you installed MinGW).

- Install MSYS: 

  - [http://www.haskell.org/ghc/tools/Win32/MSYS-1.0.10.exe](http://www.haskell.org/ghc/tools/Win32/MSYS-1.0.10.exe)
  - [http://www.haskell.org/ghc/tools/Win32/msysDTK-1.0.1.exe](http://www.haskell.org/ghc/tools/Win32/msysDTK-1.0.1.exe)
  - [http://www.haskell.org/ghc/tools/Win32/msysCORE-1.0.11-20080826.tar.gz](http://www.haskell.org/ghc/tools/Win32/msysCORE-1.0.11-20080826.tar.gz) (this is a tar file, which you have to unpack in `c:/msys/1.0`, or wherever you installed MSYS.  Note that you can't do that using an MSYS shell, because you can't overwrite the files in use, so make a copy of `c:/msys/1.0`, unpack it there, and then rename the copy back to `c:/msys/1.0`).

- Install [ Python](http://www.python.org/download/releases/) (version 2.6.2 is a good choice.  2.6.1 causes a problem with the test suite, and we don't support 3.x at this time).


The next three are just zip files, you can unpack them wherever you like, but you need to ensure that the programs can be found on your `PATH`.  I usually put all these in `c:/tools` (NB. don't put them anywhere in `c:/msys`, that's special).

- Install Happy: [http://www.haskell.org/ghc/tools/Win32/happy-1.17.zip](http://www.haskell.org/ghc/tools/Win32/happy-1.17.zip)
- Install Alex: [http://www.haskell.org/ghc/tools/Win32/alex-2.2.zip](http://www.haskell.org/ghc/tools/Win32/alex-2.2.zip)
- Install Haddock: [http://www.haskell.org/ghc/tools/Win32/haddock-0.8-Win32.zip](http://www.haskell.org/ghc/tools/Win32/haddock-0.8-Win32.zip)


Now set your `PATH`.  We recommend doing this by creating a file `.profile` in your home directory (by default `c:/msys/1.0/home/<username>`).  The contents of your `.profile` should be something like this:

```wiki
PATH=/c/mingw/bin:/c/ghc/ghc-6.10.1/bin:/usr/bin:/bin:/c/tools:/c/Python26:/c/windows/system32
```


Modify the above according to where you installed things, and change the GHC version appropriately.

### Building documentation on Windows


Documentation is optional, but in order to build it in Windows you must currently use Cygwin (there isn't a working DocBook toolchain on MSYS as far as we know).


In the Cygwin installer, just install the complete `Doc` category. You
may have to help `configure` a little bit: Set the
environment variables `XmllintCmd` and
`XsltprocCmd` to the paths of the Cygwin executables
`xmllint` and `xsltproc`,
respectively, and set `fp_cv_dir_docbook_xsl` to the path
of the directory where the XSL stylesheets are installed,
e.g. `c:/cygwin/usr/share/docbook-xsl`.    


If you want to build HTML Help, you have to install the
[ HTML Help SDK](http://msdn.microsoft.com/library/default.asp?url=/library/en-us/htmlhelp/html/hworiHTMLHelpStartPage.asp),
too, and make sure that `hhc` is in your `PATH`.
