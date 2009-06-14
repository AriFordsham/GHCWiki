# Windows platforms: Cygwin, MSYS, and MinGW


The build system is built around Unix-y makefiles.  Because it's not native,
the Windows situation for building GHC is particularly confusing.  This section
tries to clarify, and to establish terminology.


See also [ Preparation](http://hackage.haskell.org/trac/ghc/wiki/Building/Preparation/Windows).

## MinGW

[ MinGW (Minimalist GNU for Windows)](http://www.mingw.org) 
is a collection of header
files and import libraries that allow one to use `gcc` and produce
native Win32 programs that do not rely on any third-party DLLs. The
current set of tools include GNU Compiler Collection (`gcc`), GNU Binary
Utilities (Binutils), GNU debugger (Gdb), GNU make, and assorted
other utilities. 


GHC requires both the MinGW "core system" (a C compiler + other essential stuff)
and the g++ system (required by libffi) to be installed. 


The down-side of MinGW is that the MinGW libraries do not support anything like the full
Posix interface.  

## Cygwin and MSYS


You can't use the MinGW to *build* GHC, because MinGW doesn't have a shell,
or the standard Unix commands such as `mv`, `rm`,
`ls`, nor build-system stuff such as `make` and `darcs`.
For that, there are two choices: [ Cygwin](http://www.cygwin.com) 
and [ MSYS](http://www.mingw.org/msys.shtml):

- Cygwin comes with compilation tools (`gcc`, `ld` and so on), which
  compile code that has access to all of Posix.  The price is that the executables must be 
  dynamically linked with the Cygwin DLL, so that *you cannot run a Cywin-compiled program on a machine
  that doesn't have Cygwin*.  Worse, Cygwin is a moving target.  The name of the main DLL, `cygwin1.dll`
  does not change, but the implementation certainly does.  Even the interfaces to functions
  it exports seem to change occasionally. 
- MSYS is a fork of the Cygwin tree, so they
  are fundamentally similar.  However, MSYS is by design much smaller and simpler. 
  Access to the file system goes
  through fewer layers, so MSYS is quite a bit faster too.

  Furthermore, MSYS provides no compilation tools; it relies instead on the MinGW tools. These
  compile binaries that run with no DLL support, on any Win32 system.
  However, MSYS does come with all the make-system tools, such as `make`, `autoconf`, 
  `darcs`, `ssh` etc.  To get these, you have to download the 
  MsysDTK (Developer Tool Kit) package, as well as the base MSYS package.

  MSYS does have a DLL, but it's only used by MSYS commands (`sh`, `rm`, 
  `ssh` and so on),
  not by programs compiled under MSYS.

## Targeting MinGW


We want GHC to compile programs that work on any Win32 system.  Hence:

- GHC does invoke a C compiler, assembler, linker and so on, but we ensure that it only
  invokes the MinGW tools, not the Cygwin ones.  That means that the programs GHC compiles
  will work on any system, but it also means that the programs GHC compiles do not have access
  to all of Posix.  In particular, they cannot import the (Haskell) Posix 
  library; they have to do
  their input output using standard Haskell I/O libraries, or native Win32 bindings.
  We will call a GHC that targets MinGW in this way *GHC-mingw*.
- To make the GHC distribution self-contained, the GHC distribution includes the MinGW `gcc`,
  `as`, `ld`, and a bunch of input/output libraries.  


So *GHC targets MinGW*, not Cygwin.
It is in principle possible to build a version of GHC, *GHC-cygwin*, 
that targets Cygwin instead.  The up-side of GHC-cygwin is
that Haskell programs compiled by GHC-cygwin can import the (Haskell) Posix library.
*We do not support GHC-cygwin, however; it is beyond our resources.*


While GHC *targets* MinGW, that says nothing about 
how GHC is *built*.  We use both MSYS and Cygwin as build environments for
GHC; both work fine, though MSYS is rather lighter weight.


In your build tree, the compiler you build uses the `gcc` that you specify using the
`--with-gcc` flag when you run `configure` (see below).
The makefiles are careful to use the right gcc, either via the in-place ghc or directly,
to compile any C files, so that we use correct `gcc` rather than
whatever one happens to be in your path.  However, the makefiles do use whatever `ld` 
and `ar` happen to be in your path. This is a bit naughty, but (a) they are only
used to glom together .o files into a bigger .o file, or a .a file, 
so they don't ever get libraries (which would be bogus; they might be the wrong libraries), and (b)
Cygwin and MinGW use the same .o file format.  So its ok.

## File names


Cygwin, MSYS, and the underlying Windows file system all understand file paths of form `c:/tmp/foo`.
However:

- MSYS programs understand `/bin`, `/usr/bin`, and map Windows's lettered drives as
  `/c/tmp/foo` etc.  The exact mount table is given in the doc subdirectory of the MSYS distribution.

  When it invokes a command, the MSYS shell sees whether the invoked binary lives in the MSYS `/bin`
  directory.  If so, it just invokes it.  If not, it assumes the program is no an MSYS program, and walks over the command-line
  arguments changing MSYS paths into native-compatible paths.
  It does this inside sub-arguments and inside quotes. For example,
  if you invoke

  ```wiki
  foogle -B/c/tmp/baz
  ```

  the MSYS shell will actually call `foogle` with argument `-Bc:/tmp/baz`.
- Cygwin programs have a more complicated mount table, and map the lettered drives as `/cygdrive/c/tmp/foo`.

  The Cygwin shell does no argument processing when invoking non-Cygwin programs.
