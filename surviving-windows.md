# Surviving Windows as a Unix user: A glossary


Here's a summary of words you will see while working on GHC on Windows. Good luck.

## msys2 (the distribution)

**msys2** is a software distribution packaging a number of components including,

- the **msys2 runtime** (see below)
- the `pacman` package manager
- a basic UNIX-like command-line environment
- a toolchain for compiling code written for a POSIX environment (linking against the **msys2 runtime**)
- 64- and 32-bit `mingw` toolchains

## `msys2` (the runtime)


The **msys2 distribution** is built upon the **`msys2` runtime**, which is a fork of the **Cygwin runtime**. The runtime provides basic POSIX APIs, making it possible to run software written to run on a POSIX-like system on Windows.


The msys2 and Cygwin runtimes used to differ strongly, but have been converging (see [ https://github.com/Alexpux/MSYS2-packages/blob/master/msys2-runtime/PKGBUILD](https://github.com/Alexpux/MSYS2-packages/blob/master/msys2-runtime/PKGBUILD) for the current patches). Most of the remaining differences are to make the provided environment a bit closer to Windows than Cygwin's behavior.

## msys2 (the toolchain)


The msys2 distribution includes a compiler toolchain which can be used to compile code written for a POSIX environment. It can be found in `/usr/bin` (e.g. `/usr/bin/gcc`).


Executables compiled with this toolchain will link against the **`msys2` runtime**. Consequently this is not the toolchain to use to build GHC. Instead use the **`mingw-w64` toolchain** (see below).

## Cygwin (the runtime)


Cygwin is a runtime library providing a POSIX-like environment, allowing software written for POSIX systems to run on Windows unchanged. The `msys2` distribution that we use in GHC doesn't use Cygwin, but instead the `msys2` fork (see above). Cygwin differs from `msys` in that it seeks to provide complete Linux emulation in user-mode; in contrast, `msys2` merely seeks to provide enough POSIX support to be able to use toolchains such as `mingw-w64` to make native binaries.

## Cygwin (the distribution)


Cygwin is also a distribution built upon the `Cygwin` runtime providing a Linux-like environment on Windows. We don't use it; instead we use the msys2  distribution.

## `Mingw-w64`

`Mingw-w64` is a compiler toolchain based upon `gcc` targeting Windows systems which is packaged with the `msys2` distribution. It started as a fork of the `mingw` toolchain.


Executables produced with the `mingw-w64` toolchain will link only against `msvcrt` and a few native Windows libraries (e.g. `ntdll` and `kernel32`).

## `mingw`

`mingw` is a compiler toolchain based upon `gcc` targeting Windows systems. The project is now unmaintained. `mingw` is not used by `msys2` distribution used by GHC; instead it packages the `mingw-w64` toolchain.

## `msvcrt`


The Microsoft Visual C runtime. This is the `libc` of the Windows world.

## `mintty`

`mintty` is a terminal emulator shipped with the `msys2` distribution (and Cygwin). It's not so great; you probably want to use something else (e.g. [ ConEmu](https://conemu.github.io/).

## Linux on Windows


Linux on Windows (also known as the Windows Subsystem for Linux or Drawbirdge) is a project undertaken by Microsoft seeking to add a Windows subsystem to the Windows kernel, allowing full user-mode emulation of a Linux environment. Currently we don't use LoW for GHC development.