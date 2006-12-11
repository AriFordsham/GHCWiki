
GHC currently has some support for dynamic linking.


GHC can generate position independent code (when given the `-fPIC` flag) on Mac OS X (Darwin/PPC) and PowerPC (32-bit) Linux. Some *minor* work is required to make it generate position independent code for x86 ELF platforms (Linux and others). Position independent code is not needed for Windows and PowerPC64 Linux.


The `-dynamic` flag does two things:

- It causes GHC to assume that everything that is in a different package is in a different shared/dynamic library or binary.
- It causes GHC to link to dynamic libraries instead of static ones.

# Build System


There is now minimal support in the build system for building dynamic libraries. It's a hack.


I need help, volunteers report at wolfgang.thaller@… or to the cvs-ghc mailing list.


You need to add a few things to your build.mk file:

```wiki
SplitObjs = NO
GhcStage2HcOpts=-dynamic
GhcLibHcOpts=-O -fasm -fPIC -dynamic
GhcRtsHcOpts=-fPIC -fvia-C -dynamic
GhcBuildDylibs=YES
#GHC_CC_OPTS+=-fPIC
```


Uncomment the last line after compiling libghccompat and stage1 in ghc, but before compiling in libraries. Sorry for the inconvenience, feel free to fix it. For Powerpc64 Linux, you only need the first two lines.

# Platform-Specific TODO


Profiled code isn't yet really position-independent even when `-fPIC` is specified. Building profiled dynamic libraries therefore fails on Mac OS X (Linux silently accepts relocations - it's just slightly bad for performance).

## x86 and powerpc32 ELF/Linux TODO


The NCG works. Support for `-fPIC` in the mangler is buggy on PowerPC and nonexistent on x86. Due to the strange way that dynamic linking is implemented, it will be very hard to generate position-dependent code that correctly links to (Haskell code in) dynamic libraries without the NCG.


As the RTS can't currently be compiled with the NCG (a volunteer opportunity!), the RTS has to be compiled as position dependent code.

## Windows TODO


There used to be a Windows-specific hack involving static closures because Windows doesn't support references to dynamically imported symbols in static data. This hack has bitrotted and needs to be resurrected. The Windows-specific code for supporting `-dynamic` in the NCG has never been tested, to my knowledge.


When building a DLL, you have to specify which libraries it depends on; the build system will need to support this. There is a lot of code for building DLLs in the Makefiles, but it probably no longer works and needs to be merged with the new shared library building support.


Also, since the last time that DLLs worked with GHC on Windows, the GNU linker has gained a new feature. It can now "auto-import" data from shared libraries, making all the windows-specific hacks unnecessary. However, auto-imported data will prevent all sharing of code between processes, every page of code with a reference to data will get written to at load time.

## Darwin TODO


Done.

## PowerPC 64-bit Linux TODO


This platform has a great ABI which didn't require anything to be done for dynamic linking to work. Both `-fPIC` and `-dynamic` will just be ignored.

# AIX TODO


We don't need any special dynamic linking support here, either, so all that needs to be done is the build system.

# Platform-Specific NOTES


The following tables show which combinations of the `-dynamic` and `-fPIC` flags work in which situations on which platforms. The row indicates how a Haskell module is built, and the column indicates what it is being used for. A YES means it should work, a NO means it will fail to link or even crash.

## Darwin

<table><tr><th></th>
<th>dynamic libraries</th>
<th>static code</th>
<th>dynamically linked executables</th>
<th>statically linked plugins</th>
<th>dynamically linked plugins
</th></tr>
<tr><th>(no flags)    </th>
<th>NO               </th>
<th>YES        </th>
<th>NO                            </th>
<th>NO            </th>
<th>NO
</th></tr>
<tr><th>-dynamic      </th>
<th>NO               </th>
<th>YES        </th>
<th>YES                           </th>
<th>NO            </th>
<th>NO
</th></tr>
<tr><th>-fPIC         </th>
<th>NO               </th>
<th>YES        </th>
<th>NO                            </th>
<th>YES           </th>
<th>NO
</th></tr>
<tr><th>-dynamic -fPIC</th>
<th>YES              </th>
<th>YES        </th>
<th>YES                           </th>
<th>YES           </th>
<th>YES
</th></tr></table>

## PowerPC 32-Bit and x86 Linux

<table><tr><th></th>
<th>dynamic libraries</th>
<th>static code</th>
<th>dynamically linked executables</th>
<th>statically linked plugins</th>
<th>dynamically linked plugins
</th></tr>
<tr><th>(no flags)    </th>
<th>NO               </th>
<th>YES        </th>
<th>NO                            </th>
<th>YES**NO
-dynamic      NO               YES        YES\*                          YES****NO
-fPIC         NO               YES        NO                            YES           NO
-dynamic -fPICYES              YES        NO                            YES           YES

(\*) Dynamically linked executables have to be built via the native code genetator (when using `-O`, specify `-fasm`).

(**) Position-dependent code theoretically leads to increased load times and prevents sharing between multiple instances of the code.
**
Via-C compilation with `-fPIC` or `-dynamic` currently doesn't work (not implemented on x86, buggy on PPC).
PowerPC 64-Bit Linuxdynamic librariesstatic codedynamically linked executablesstatically linked pluginsdynamically linked plugins
(no flags)    YES              YES        YES                           YES           YES
-dynamic      YES              YES        YES                           YES           YES
-fPIC         YES              YES        YES                           YES           YES
-dynamic -fPICYES              YES        YES                           YES           YES

Now that's a boring table... `-fPIC` is ignored, and `-dynamic` doesn't affect code generation. Everything just works.
x86 Windows
Windows support isn't there yet. In theory, though, this is how it will look like:
dynamic librariesstatic codedynamically linked executablesstatically linked pluginsdynamically linked plugins
(no flags)    NO               YES        NO                            YES           NO
-dynamic      YES              NO         YES                           NO            YES
-fPIC         NO               YES        NO                            YES           NO
-dynamic -fPICYES              NO         YES                           NO            YES
`-fPIC` is ignored.
Download in other formats:[Plain Text](/trac/ghc/wiki/DynamicLinking?version=2&format=txt)[](http://trac.edgewall.org/)Powered by [Trac 1.2.2](/trac/ghc/about)

        By [Edgewall Software](http://www.edgewall.org/).Visit the Trac open source project at
[http://trac.edgewall.org/](http://trac.edgewall.org/)**</th>
<th></th></tr></table>