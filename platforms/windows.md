# Windows Support for GHC


GHC on Windows is supported by the [Windows Task Force](windows-task-force).

## Versions supported

<table><tr><th> GHC Version </th>
<th> \<= WinNT4 </th>
<th> Win2k </th>
<th> WinXP </th>
<th> Vista </th>
<th> Win7 </th>
<th> Win8 </th>
<th> Win10 
</th></tr>
<tr><th>  7.8          </th>
<th></th>
<th>  Yes    </th>
<th>  Yes    </th>
<th>  Yes    </th>
<th>  Yes   </th>
<th></th>
<th></th></tr>
<tr><th>  7.10         </th>
<th></th>
<th></th>
<th>  Yes    </th>
<th>  Yes    </th>
<th>  Yes   </th>
<th>  Yes   </th>
<th></th></tr>
<tr><th>  8.0          </th>
<th></th>
<th></th>
<th></th>
<th>  Yes    </th>
<th>  Yes   </th>
<th>  Yes   </th>
<th>  Yes    
</th></tr></table>

## Building 32-bit Windows programs


Current releases of GHC provide a 32-bit Windows version for building 32-bit programs on Windows. It ships with a small [ MinGW-w64](http://mingw-w64.sourceforge.net/) system bundled, which provides tools (such as a C compiler, linker and assembler) that are used by the compiler.

## Building 64-bit Windows programs


Releases of GHC since 7.6.1 also provide a 64bit Windows version for building 64bit programs on Windows. It ships with a [ MinGW-w64](http://mingw-w64.sourceforge.net/) system bundled, which provides tools (such as a C compiler, linker and assembler) that are used by the compiler.

## Building on Windows


Build instructions for Windows are incorporated in the [Building Guide](building).  In particular, here is how to [set up your Windows system for building GHC](building/preparation/windows).
