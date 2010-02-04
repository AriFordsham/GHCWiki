# Building on Solaris


These instructions have only been checked for GHC 6.12.1 on Solaris 10 on SPARC. It should mostly apply to later versions of GHC, Solaris 8 and later and perhaps Solaris on x86. 


GHC versions 6.10.1 and earlier don't have a working SPARC native code generator, and have many small build issues with Solaris. Use GHC 6.12.1 or later.

## Installing GNU packages


GHC relies on many GNU-isms that are not supported by the native Solaris build tools. The following environment is known to work. Later versions may work but have not been tested. Taking the time to install these tools is likely to be less painful than debugging build problems due to unsupported versions (and this is your official warning).

> <table><tr><th> GNU binutils 2.20  </th>
> <th> for GNU ld, maybe others 
> </th></tr>
> <tr><th> GNU coreutils 8.4  </th>
> <th> for GNU tr, maybe others 
> </th></tr>
> <tr><th> GNU make 3.81     </th>
> <th> make files use GNU extensions 
> </th></tr>
> <tr><th> GNU m4 1.4.13     </th>
> <th></th></tr>
> <tr><th> GNU sed 4.2           </th>
> <th> build scripts use GNU extensions 
> </th></tr>
> <tr><th> GNU tar 1.20         </th>
> <th> Solaris tar doesn't handle large file names 
> </th></tr>
> <tr><th> GNU grep 2.5      </th>
> <th> build scripts use GNU extensions 
> </th></tr>
> <tr><th> GNU readline 5 </th>
> <th></th></tr>
> <tr><th> GNU ncurses 5.5 </th>
> <th></th></tr>
> <tr><th> Python 2.6.4 </th>
> <th> needed to run the testsuite with multiple threads 
> </th></tr>
> <tr><th> GCC 4.1.2       </th>
> <th> this exact version is needed 
> </th></tr></table>


Some of these can be obtained as binary versions from the  [ blastwave.org](http://www.blastwave.org/) collection, others need to be downloaded as source from [ gnu.org](http://www.gnu.org).


The blastwave libraries are usually installed under `/opt/csw`, so you may need to manually set `LD_LIBRARY_PATH` to point to them:

```wiki
export LD_LIBRARY_PATH=/opt/csw/lib
```

## Using a bootstrapping GHC


You can either get a binary distribution from the GHC download page or use some other pre-existing GHC binary. These binaries usually assume that required libraries are reachable via LD_LIBRARY_PATH, or are in `/opt/csw`. If you get errors about missing libraries or header files, then the easiest solution is to create soft links to them in, `lib/ghc-6.12.1` and `lib/ghc-6.12.1/include` of the installed binary distribution. These paths are always searched for libraries / headers.

# What can go wrong


The rest of this page discusses problems with specific tool versions. If you stick to the versions in the above list then you shouldn't have to read further.

## Only some GCC versions work

- GCC version 4.1.2 is known to work. Use this version if possible.


On Solaris 10, `/usr/bin/gcc` is "GCC for Sun Systems (gccfss)". This is a version that uses Sun's code generator backend. This is completely unusable for GHC because GHC has to post-process (mangle) the assembly output of GCC. It expects the format and layout that the normal GCC uses.


The version of `/usr/sfw/bin/gcc` on Solaris 10 is 3.4.x which has problems, see below.


GCC version 4.3.x produces assembly files that GHC's "evil mangler" does not yet deal with.


GCC version 4.2.x works but takes hours and hours to build the large `.hc` files that GHC generates. It is reported ([\#1293](https://gitlab.haskell.org//ghc/ghc/issues/1293), [\#2906](https://gitlab.haskell.org//ghc/ghc/issues/2906)) that particular modules can take upwards of 5 hours and the overall build takes a couple days. This is due to complexity issues with respect to GCC moving to a unit-at-a-time compilation scheme instead of function-at-a-time.


GCC version 4.0.2 does not support thread local state (TLS), at least on SPARC.


GCC version 3.4.x is reported ([\#951](https://gitlab.haskell.org//ghc/ghc/issues/951)) to mis-compile the runtime system leading to a runtime error `schedule: re-entered unsafely`.
But such a gcc version is sufficient for most user programs in case you just installed a ghc binary distribution. 

## Split objects


With the right toolchain this can work. It was tested on Solaris 10 with ghc-6.8.3, gcc-4.1.2 and the system (not GNU) binutils (ie as, ld etc from /usr/ccs/bin).


If you run into problems however turn it off by adding this to your `mk/build.mk`:

```wiki
SplitObjs=NO
```