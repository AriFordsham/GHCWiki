# Windows Support for GHC

GHC on Windows is supported by the [Windows Task Force](windows-task-force).

## Versions supported

| GHC Version   | \<= WinNT4   | Win2k   | WinXP   | Vista   | Win7   | Win8   | Win10 Creators   | Win10    |
| ------------- | ------------ | ------- | ------- | ------- | ------ | ------ | ---------------- | -------  |
| 7.8           |              | ✔       | ✔       | ✔       | ✔      |        |                  |          |
| 7.10          |              |         | ✔       | ✔       | ✔      | ✔      |                  |          |
| 8.0           |              |         |         | ✔       | ✔      | ✔      | ✔ (Note 1)       | ✔        |
| 8.2           |              |         |         | ✔       | ✔      | ✔      | ✔                | ✔        |
| 8.4           |              |         |         | ✔       | ✔      | ✔      | ✔                | ✔        |
| 8.6           |              |         |         |         | ✔      | ✔      | ✔                | ✔        |
| 8.8           |              |         |         |         | ✔      | ✔      | ✔                | ✔        |
| 8.10          |              |         |         |         | ✔      | ✔      | ✔                | ✔        |

**Note 1**: Only distributions specifically advertising support the Creator's
Update will work with Windows 10 Creator's Update and later. 

Note that Vista must be patched with KB2533623.

## Building 32-bit Windows programs

Current releases of GHC provide a 32-bit Windows version for building 32-bit
programs on Windows. It ships with a small
[MinGW-w64](http://mingw-w64.sourceforge.net/) system bundled, which provides
tools (such as a C compiler, linker and assembler) that are used by the
compiler.

## Building 64-bit Windows programs

Releases of GHC since 7.6.1 also provide a 64-bit Windows version for building
64-bit programs on Windows. It ships with a
[MinGW-w64](http://mingw-w64.sourceforge.net/) system bundled, which provides
tools (such as a C compiler, linker and assembler) that are used by the
compiler.

## Building on Windows

Build instructions for Windows are incorporated in the [Building
Guide](building).  In particular, here is how to [set up your Windows system
for building GHC](building/preparation/windows).

