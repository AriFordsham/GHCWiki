# Dynamic by default on Windows


We don't currently support dynamic-by-default on Windows. We are also not currently working on it - if anyone is interested in tackling it then your help would be much appreciated!


The issue is that, when building a dynamically linked library on Windows, there is no equivalent of elf's RPATH to tell the executable where its libraries are. This means that, unless the libraries have been installed to a global directory such as `c:/windows/system`, `ghc --make foo` would have to copy all the DLLs used to the current directory. Clearly not satisfactory!


There are some possibilities, some of which have been ruled out, but some of which remain potential solutions:

## Assemblies


Windows does actually have a similar concept to RPATHs: side-by-side assemblies. With Windows 7, these do allow us to give the location of DLLs that we use, but only as relative paths, and using at most 2 `../`s (see "privatePath" on [http://msdn.microsoft.com/en-us/library/aa374182.aspx](http://msdn.microsoft.com/en-us/library/aa374182.aspx)). It's therefore not possible to use assemblies to say that we use `c:/ghc/base/base.dll`. This therefore doesn't generally allow us to solve the problem.


With administrator privileges DLLs can be installed as shared assemblies into the standard Windows "side-by-side" folder, but when developing without as a normal user they would need to be copied into the same directory as the compiled executable, which might be a rather surprising thing for ghc -o or ghc --make to do.

## C stub


A simple option would be for `ghc --make foo` to link all of foo's code into `foo.exe.dll`, and then make `foo.exe` by generating and compiling a C stub that does

```wiki
AddDllDirectory("...");
AddDllDirectory("...");
LoadLibrary("foo.exe.dll);
```


However, this is a little klunky, as there is now an extra file needed for every executable. It would also likely cause problems for build systems.

## Loading DLL resource from memory


An improvement on the C stub options is to use windres to embed the DLL into the executable, and then to load the DLL from there. It would be possible to write the DLL out to a temporary file and load that, but that would be rather unpleasant. Better would be to load the DLL directly from memory.


Windows doesn't support that, but there is code to do so [here](http://www.joachim-bauch.de/tutorials/loading-a-dll-from-memory/). Downsides are that it is MPLed (to check: is that a problem?), and it involves replicating some of the system linker (but not as bad as the current GHCi linker). It is also untested, so we are not 100% sure that it will work.

## Delay loading


Another possibility is delay loading. This only links a DLL when a function from it is actually called, which means that the main function is called before the DLLs are loaded. The main function therefore has an opportunity to call `AddDllDirectory` first.


However, we have problems when we try to do things like (I think) getting the info table of a function that hasn't been called yet. If it hasn't been called then its address hasn't been updated, so we get garbage. There is a function, `__HrLoadAllImportsForDll`, which is supposed to update all addresses, but [it seems not to work](http://sourceforge.net/mailarchive/forum.php?thread_name=20121123141320.GA10578%40matrix.chaos.earth.li&forum_name=mingw-w64-public). Fixing that would therefore be necessary, but I am unsure whether it would be sufficient.
