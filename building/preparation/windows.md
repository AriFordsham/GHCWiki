# Setting up a Windows system for building GHC


Installing the following will get you a working build environment with MSYS. The instructions are current for GHC 7.6.


Other documentation for Windows includes:

- [Using Cygwin](building/windows/cygwin) to build GHC. Using MSYS is the preferred approach though.
- [MinGW/MSYS/Cgwin](building/platforms/windows) information for people new to using UNIX tools on Windows.
- [Using SSH](building/windows/ssh) on Windows.

## Setting up Windows

1. **Install the following tools:**

- [ Haskell Platform](http://hackage.haskell.org/platform/)
- [ Git](http://git-scm.com/)
- [ Python](http://python.org/) (Version 2.7 is a good choice, we don't support version 3.x at this time)
- [ LLVM](http://www.llvm.org/releases/download.html) (Optional, for using GHC's LLVM backend, grab the file called 'LLVM Binaries for Mingw32/x86')


We recommend using the **default install locations** for all these tools.  If you choose your own paths, then we recommend not using a path containing spaces if the default did not have spaces.

1. **Install the MinGW and MSYS tools:**


MinGW provides a windows version of GCC while MSYS provides a minimal UNIX environment (e.g bash, make... ect). The website for MinGW is totally confusing, so go here:

- [ Mingw/MSYS Getting Started](http://www.mingw.org/wiki/Getting_Started)


and follow the download instructions for the **mingw-get-inst** installer. This is an easy to use installer for installing both MinGW and MSYS.   Make sure when you run the installer that you **select to install g++, MSYS and the MSYS Dev Kit**.

1. **Set your `PATH`**. You need to include at least

  - `c:/MinGW/bin` (contains `autoconf` etc)
  - `c:/MinGW/msys/1.0/bin` (contains `bash`, `make` etc)
  - `c:/git/bin` (or wherever you installed git)
  - `c:/Python27` (or wherever you installed Python)
  - `c:/dev/llvm/bin` (or wherever you installed LLVM, if you got it)


We recommend doing this by creating a file `.profile` in your home directory (by default `c:/MinGW/msys/1.0/home/<username>`). The contents of your `.profile` should be something like this:

```wiki
# Add Python to path
export PATH=${PATH}:/c/Python27

...etc..etc...
```


The Haskell platform installer should have already done the work needed to make GHC available on the path.


If you use a shell within Emacs, make sure your `SHELL` environment variable points to the `bash` in `c:/MinGW/msys/1.0/bin`. 

1. **Launch the shell** by starting the 'MinGW Shell' which should be in your start menu.


Use `autoconf --version` to check that you have at least version 2.68 of `autoconf`. Version 2.56 (which was around for a long time) does not work for GHC's build system.


You should now have a working environment for getting the source for GHC and building it!

## Disable realtime virus-scanning for your build


Realtime virus scanners are prone to causing weird build failures, typically "permission denied" errors that go away when the build is restarted.  The best way to avoid these problems is to exclude the directory containing your GHC build from realtime virus scanning, if your scanner supports excluding particular directories.  You probably also want to exclude directories in which temporary files are stored, which by default is `C:/Users/<user>/Local Settings/Temp` on Windows Vista and later, `C:/Documents and Settings/<user>/Local Settings/Temp` on Windows XP and older, or `C:/Temp`.

## Building documentation on Windows


Building GHC's documentation is optional, but in order to build it in Windows you must currently use Cygwin (there isn't a working DocBook toolchain on MSYS as far as we know).


In the Cygwin installer, just install the complete `Doc` category. You may have to help `configure` a little bit: Set the environment variables `XmllintCmd` and `XsltprocCmd` to the paths of the Cygwin executables `xmllint` and `xsltproc`, respectively, and set `fp_cv_dir_docbook_xsl` to the path of the directory where the XSL stylesheets are installed, e.g. `c:/cygwin/usr/share/docbook-xsl`.    


If you want to build HTML Help, you have to install the [ HTML Help SDK](http://msdn.microsoft.com/library/default.asp?url=/library/en-us/htmlhelp/html/hworiHTMLHelpStartPage.asp), tool, and make sure that `hhc` is in your `PATH`.
