# Dynamic by default on Windows


We don't currently support dynamic-by-default on Windows. The issue is that, when building a dynamically linked library on Windows, there is no equivalent of elf's RPATH to tell the executable where its libraries are. This means that, unless the libraries have been installed to a global directory such as `c:/windows/system`, `ghc --make foo` would have to copy all the DLLs used to the current directory.

## Assemblies


Windows does actually have a similar concept to RPATHs: assemblies. These do allow us to give the location of DLLs that we use, but only as relative paths, and using at most 2 `../`s. It's therefore not possible to use assemblies to say that we use `c:/ghc/base/base.dll`. This therefore doesn't generally allow us to solve the problem.

## C stub


A simple option would be for `ghc --make foo` to link all of foo's code into `foo.exe.dll`, and then make `foo.exe` by generating and compiling a C stub that does

```wiki
AddDllDirectory("...");
AddDllDirectory("...");
LoadLibrary("foo.exe.dll);
```


However, this is a little klunky, as there is now an extra file needed for every executable. It would also likely cause problems for build systems.

## Loading DLL resource from memory


An improvement 
