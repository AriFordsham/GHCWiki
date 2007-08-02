# Shared Libraries: distribution and build-system issues

# Platform support for locating shared libraries


The following analysis is mostly from Reilly Hayes on the cvs-ghc mailing list.

## On Linux


An ELF executable can have an embedded list of paths to search for dynamic libraries (the DT_RPATH entry).  This can be set by using -rpath with ld.  DT_RPATH is deprecated.  This list applies to all shared libraries used by the executable (it is not per shared library).  There is no default value placed in the DT_RPATH entry.  You must use -rpath to set it.


There is a new entry, DT_RUNPATH.  DT_RUNPATH works similarly to DT_RPATH.  However, when it is set, DT_RPATH is ignored.  DT_RUNPATH is also set using -rpath, but you must use the --enable-new-dtags switch as well.  


When looking for a shared library, the dynamic linker(ld.so) checks the paths listed in DT_RPATH (unless DT_RUNPATH Is set) , the paths listed in the environment variable LD_LIBRARY_PATH, the paths listed in DT_RUNPATH, the libraries listed in /etc/ld.so.cache, and finally /usr/lib and /lib.  It checks in that order and takes the first library found.  At least on my linux box, LD_LIBRARY_PATH does NOT override the paths in DT_RPATH even though the documentation implies that it does.   LD_LIBRARY_PATH does override DT_RUNPATH.


You CAN override the search path embedded using DT_RPATH by using the LD_PRELOAD environment variable.  This variable contains a \*whitespace-separated\* list of libraries (not directories to search) to load prior to the search process.  The listed libraries are loaded whether or not they are needed to resolve a dependency in the executable.


Finally, an ELF shared library can also have a DT_RPATH entry.  This only impacts the search for shared libraries that are dependencies of the shared library and not the executable.  As with the DT_RPATH entry in an ELF executable, this is not overridden by LD_LIBRARY_PATH but can be overridden using LD_PRELOAD as above.  

## On Mac OS X


A Mach-O executable can embed the full path name for each shared library (as well as rules for acceptable substitutes).  This is called the "install name" for the library and it is included by default when building an executable.  The install name for the library is NOT based on where the static linker (ld) found the library when the executable was built.  The static linker (ld) extracts the install name from the shared library when building the executable.  The install name of the shared library is set when building the shared library.  When you build a shared library you should know where the library is going to be installed so that the install name is set correctly.


When looking for shared libraries, the dynamic linker (dyld) first scans the directories in DYLD_LIBRARY_PATH, then checks the location in the install name (which is per library), and finally checks the standard locations.


DYLD_LIBRARY_PATH successfully overrides the the path embedded in the executable.


Caveat 1: LD_LIBRARY_PATH has no runtime impact, but it does impact where the static linker looks for share libraries.  It looks first in the directories specified using -L, the the directories in LD_LIBRARY_PATH, and finally in /lib, /usr/lib, & /usr/local/lib.  This is particularly confusing  because many configure scripts seem to ignore LD_LIBRARY_PATH and you can get inconsistent results from configure and gcc/ld on whether a library is present.


Caveat 2: Mac OS X has a set of compiler/linker switches for dealing with Frameworks (packages of shared libraries and include files).  These are installed outside the typical \*nix directory structure.  These switches act like -I (to gcc) and -L (to ld).  If you end up totally confused about where to find something, read up on this.  The OpenGL and OpenAL headers and libraries are in Frameworks, for example.

## Conclusions


For Mac: The -rpath switch is not available on Mac OS X because it is superfluous.  The default behavior of embedding a location for each individual shared library is at least as good.  Cabal (and the GHC build process) should use their knowledge of the ultimate install location to set the install name when shared libraries are built.  In-place compilation can override this with DYLD_LIBRARY_PATH


For Linux: On linux, we should be sure to use the --enable-new-dtags switch if we use -rpath.  Otherwise we risk having paths that can't be overridden by LD_LIBRARY_PATH.
