# Surviving Windows as a Unix user: A glossary


Here's a summary of words you will see while working on GHC on Windows. Some other helpful background can be found on

- the [How does msys2 differ from Cygwin](https://github.com/msys2/msys2/wiki/How-does-MSYS2-differ-from-Cygwin) article on the msys2 wiki.
- This [Stack Overflow question](http://stackoverflow.com/questions/25019057/how-are-msys-msys2-and-msysgit-related-to-each-other) discussing the relationship between msys and msys2


Good luck.

## msys2 (the distribution)

**msys2** is a software distribution packaging a number of components including,

- the **msys2 runtime** (see below)
- the `pacman` package manager
- a basic UNIX-like command-line environment
- a toolchain for compiling code written for a POSIX environment (linking against the **msys2 runtime**)
- 64- and 32-bit `mingw` toolchains

## `msys2` (the runtime)


The **msys2 distribution** is built upon the **`msys2` runtime**, which is a fork of the **Cygwin runtime**. The runtime provides basic POSIX APIs, making it possible to run software written to run on a POSIX-like system on Windows.


The msys2 and Cygwin runtimes used to differ strongly, but have been converging (see [https://github.com/Alexpux/MSYS2-packages/blob/master/msys2-runtime/PKGBUILD](https://github.com/Alexpux/MSYS2-packages/blob/master/msys2-runtime/PKGBUILD) for the current patches). Most of the remaining differences are to make the provided environment a bit closer to Windows than Cygwin's behavior.

## msys2 (the toolchain)


The msys2 distribution includes a compiler toolchain which can be used to compile code written for a POSIX environment. It can be found in `/usr/bin` (e.g. `/usr/bin/gcc`).


Executables compiled with this toolchain will link against the **`msys2` runtime**. Consequently this is not the toolchain to use to build GHC. Instead use the **`mingw-w64` toolchain** (see below).

## Cygwin (the runtime)


Cygwin is a runtime library providing a POSIX-like environment, allowing software written for POSIX systems to run on Windows unchanged. The `msys2` distribution that we use in GHC doesn't use Cygwin, but instead the `msys2` fork (see above). Cygwin differs from `msys` in that it seeks to provide complete Linux emulation in user-mode; in contrast, `msys2` merely seeks to provide enough POSIX support to be able to use toolchains such as `mingw-w64` to make native binaries.

## Cygwin (the distribution)


Cygwin is also a distribution built upon the `Cygwin` runtime providing a Linux-like environment on Windows. We don't use it; instead we use the msys2  distribution.

## `Mingw-w64`

`Mingw-w64` is a compiler toolchain based upon `gcc` targeting Windows systems which is packaged with the `msys2` distribution. It started as a fork of the `mingw` toolchain. A fair amount of useful information about `mingw-w64` can be found on the project's [wiki](https://sourceforge.net/p/mingw-w64/wiki2/Home/).


Executables produced with the `mingw-w64` toolchain will link only against `msvcrt` and a few native Windows libraries (e.g. `ntdll` and `kernel32`).

## `mingw`

`mingw` is a compiler toolchain based upon `gcc` targeting Windows systems. The project is now unmaintained. `mingw` is not used by `msys2` distribution used by GHC; instead it packages the `mingw-w64` toolchain.

## `msys`

`msys` (short for "minimal system) is a component of the `mingw` project which provides Windows ports of a lightweight Unix-like shell environment. It does not provide a compiler toolchain; this is provided by `mingw`.


Note that GHC uses neither `mingw` nor `msys`; rather we use `mingw-w64` and `msys2`.

## `msvcrt`


The Microsoft Visual C runtime. This is the `libc` of the Windows world. It also offers a number of POSIX compatibility routines (e.g. `exec`) which aren't POSIX compliant.

## `mintty`

`mintty` is a terminal emulator shipped with the `msys2` distribution (and Cygwin). It's not so great; you probably want to use something else (e.g. [ConEmu](https://conemu.github.io/)).

## WSL

Windows Subsystem for Linux (WSL, also known as the Linux on Windows or Drawbridge) is a project undertaken by Microsoft seeking to add a Linux subsystem to the Windows kernel, allowing full user-mode emulation of a Linux environment. Currently we don't use LoW for GHC development. It exhibits a few infelicities relative to "typical" POSIX systems (e.g. [file locking](https://github.com/haskell/cabal/issues/6551#issuecomment-589212080), and [mmap performance](https://github.com/microsoft/WSL/issues/1671)).

## WSL2

Version 2 of WSL abandons the user-space emulation approach in favor of full hypervirtualisation and various driver- and userspace-level interoperability bridges.

## Useful tools

* [Dependencies](https://github.com/lucasg/Dependencies) is a very useful tool for diagnosing dynamic linking issues
* [CFF Explorer](https://ntcore.com/?page_id=388) is useful for exploring the structure of portable executable objects
* In the [Windows SDK](https://developer.microsoft.com/en-us/windows/downloads/sdk-archive):
   * the `gflags.exe` tool can be used to enable "loader snaps" for a particular executable. When enabled, the dynamic linker will emit debug output when the executable is run under `windbg`
   * `windbg` is the Windows debugger, apparently last updated circa 1992
* The [version](https://www.microsoft.com/en-us/p/windbg-preview/9pgjgd53tn86?activetab=pivot:overviewtab) of `windbg` in the Windows Store appears to be much better than that in the SDK
* [`x64dbg`](https://x64dbg.com/#start) is an alternative FOSS debugger
* The [sysinternals tools](https://docs.microsoft.com/en-us/sysinternals/)
   * `DebugView` allows one to view debug output from the system (e.g. the loader snaps mentioned above) without Windbg
   * Process Explorer provides a nice view onto the process tree and process state
   * [Process Monitor](https://docs.microsoft.com/en-us/sysinternals/downloads/procmon) provides an `strace`-like view of system calls
* Windows Performance Record/Analyzer is another mechanism capable of tracing system calls
* [ConEmu](https://conemu.github.io/) is a decent terminal emulator for Windows
* [this gdb fork](https://github.com/ssbssa/gdb/releases) can open minidump files, as produced by GHC's `--generate-crash-dumps` flag