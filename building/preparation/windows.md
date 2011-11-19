# Setting up a Windows system for building GHC


Installing the following will get you a working build environment with MSYS. The instructions are current for GHC 7.2.


Other documentation for Windows includes:

- [Using Cygwin](building/windows/cygwin) to build GHC. Using MSYS is the preferred approach though.
- [MinGW/MSYS/Cgwin](building/platforms/windows) information for people new to using UNIX tools on Windows.
- [Using SSH](building/windows/ssh) on Windows.

## Setting up Windows

1. We don't recommend installing anything into a directory path that contains spaces.

1. You will need to install the following tools:

- [ Haskell Platform](http://hackage.haskell.org/platform/)
- [ Git](http://git-scm.com/)
- [ Python](http://python.org/) (Version 2.7 is a good choice, we don't support version 3.x at this time)
- [ LLVM](http://www.llvm.org/releases/download.html) (Optional, for using GHC's LLVM backend, grab the file called 'LLVM Binaries for Mingw32/x86')

1. You will need to install the MinGW and MSYS tools:

- [ MinGW with MSYS](http://www.mingw.org/)


MinGW provides a windows version of GCC while MSYS provides a minimal UNIX environment (e.g bash, make... ect). The website for MinGW is a little confusing, go to the [ getting started](http://www.mingw.org/wiki/Getting_Started) page and follow the download instructions for the 'mingw-get-inst' installer. This is an easy to use single executable for installing both MinGW and MSYS, make sure when you run it that you select to install g++, MSYS and the MSYS Dev Kit.

1. Launch the shell by starting the 'MinGW Shell' which should be in your start menu.

1. Set your `PATH`.  We recommend doing this by creating a file `.profile` in your home directory (by default `c:/MinGW/msys/1.0/home/<username>`). The contents of your `.profile` should be something like this:

```wiki
# Add Git and Python to path
export PATH=${PATH}:/c/Git/bin:/c/Python27

# If you also grabbed LLVM
export PATH=${PATH}:/c/dev/llvm/bin
```


Modify the above according to where you installed Git and Python. The Haskell platform installer should have already done the work needed to make GHC available on the path.

1. You should now have a working environment for getting the source for GHC and building it!

## Disable realtime virus-scanning for your build


Realtime virus scanners are prone to causing weird build failures, typically "permission denied" errors that go away when the build is restarted.  The best way to avoid these problems is to exclude the directory containing your GHC build from realtime virus scanning, if your scanner supports excluding particular directories.  You probably also want to exclude directories in which temporary files are stored, which by default is `C:/Users/<user>/Local Settings/Temp` on Windows Vista and later, `C:/Documents and Settings/<user>/Local Settings/Temp` on Windows XP and older, or `C:/Temp`.

## Building documentation on Windows


Building GHC's documentation is optional, but in order to build it in Windows you must currently use Cygwin (there isn't a working DocBook toolchain on MSYS as far as we know).


In the Cygwin installer, just install the complete `Doc` category. You may have to help `configure` a little bit: Set the environment variables `XmllintCmd` and `XsltprocCmd` to the paths of the Cygwin executables `xmllint` and `xsltproc`, respectively, and set `fp_cv_dir_docbook_xsl` to the path of the directory where the XSL stylesheets are installed, e.g. `c:/cygwin/usr/share/docbook-xsl`.    


If you want to build HTML Help, you have to install the [ HTML Help SDK](http://msdn.microsoft.com/library/default.asp?url=/library/en-us/htmlhelp/html/hworiHTMLHelpStartPage.asp), tool, and make sure that `hhc` is in your `PATH`.
