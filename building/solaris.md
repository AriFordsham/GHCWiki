# Building on Solaris


These instructions have only been checked for GHC 6.12.1 on Solaris 10 on SPARC. It should mostly apply to later versions of GHC, Solaris 8 and later and perhaps Solaris on x86.


A common theme in these instructions is the issue that required tools and libraries are not part of the standard system set and the need for us to set various flags to tell the build system where to find them. 

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
> <th></th></tr>
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


The rest of this page lists problems with specific tool versions. If you stick to the versions in the above list then you shouldn't have to read further.

## Only some GCC versions work

- GCC version 4.1.2 is known to work. Use this version if possible.


On Solaris 10, `/usr/bin/gcc` is a version that uses Sun's code generator backend. This is completely unusable for GHC because GHC has to post-process (mangle) the assembly output of GCC. It expects the format and layout that the normal GCC uses.


The version of `/usr/sfw/bin/gcc` on Solaris 10 is 3.4.x which has problems, see below.


GCC version 4.3.x produces assembly files that GHC's "evil mangler" does not yet deal with.


GCC version 4.2.x works but takes hours and hours to build the large `.hc` files that GHC generates. It is reported ([\#1293](https://gitlab.haskell.org//ghc/ghc/issues/1293), [\#2906](https://gitlab.haskell.org//ghc/ghc/issues/2906)) that particular modules can take upwards of 5 hours and the overall build takes a couple days. This is due to complexity issues with respect to GCC moving to a unit-at-a-time compilation scheme instead of function-at-a-time.


GCC version 4.0.2 does not support thread local state (TLS), at least on SPARC.


GCC version 3.4.x is reported ([\#951](https://gitlab.haskell.org//ghc/ghc/issues/951)) to mis-compile the runtime system leading to a runtime error `schedule: re-entered unsafely`.
But such a gcc version is sufficient for most user programs in case you just installed a ghc binary distribution. 

## Using GMP from a non-standard location


The gmp library is not a standard system library on Solaris. It can usually be installed from a third party binary package collection or built from source. Either way it will usually not be on the standard cpp include path or the standard static linker path, or the standard dynamic linker path.


We can handle the first two aspects with these `./configure` flags `--with-gmp-includes` and `--with-gmp-libraries`.


For example:

```wiki
./configure --with-gmp-includes=/opt/csw/include --with-gmp-libraries=/opt/csw/lib
```


However to actually run programs compiled by ghc (such as stage1) that use gmp from this location we need to link them in such a way that they will find the gmp lib at runtime. The best way is to use the `-R` linker flag to 'bake in' the right path.


This can be specified in the `mk/build.mk` file by using:

```wiki
SRC_HC_OPTS=-optl-R/opt/csw/lib
```

TODO check this works, it was only tested with a bootstrapping ghc that always used the above flag, baked into the driver shell script. In that case only `GhcStage2HcOpts=-optl-R/opt/csw/lib` was needed.


Additionally, the `--with-gmp-`\* flags ensure that when using the resulting ghc, that it will be able to link programs to gmp. That is `ghc --make Hello.hs` will actually compile because it will pass `-L/opt/csw/lib` when linking it. However as before, while it links this does not ensure that the resulting program will run. We also need to tell the dynamic linker to look in the gmp lib dir. To get ghc to pass `-R` as well as `-L` we need to alter the registration information for the rts package.


Note that this currently needs to be done after installation. See [\#2933](https://gitlab.haskell.org//ghc/ghc/issues/2933) about integrating it into the build process.

```wiki
ghc-pkg describe rts > rts.pkg
vim rts.pkg
ghc-pkg update rts.pkg
```


In the editing step you need to add the `-R/path/to/gmp/lib` to the `ld-options` field.


Again, if you are building a relocatable binary package then you will want to avoid `-L` or `-R` linker flags being baked in and instead require that the end user set an appropriate `$LD_LIBRARY_PATH`.

## Using readline from a non-standard location


As with gmp, we need to tell `./configure` about the location of `readline`. Be careful here because it may look like you are building with readline support when in fact you are not.


Using the `--with-gmp-includes=``--with-gmp-libraries=` flags are enough to get the top level `./configure` script to believe that using readline will work, if you happen to have gmp and readline installed under the same prefix. However it is not enough for the Haskell readline package's configure script. Unfortunately that one gets run half way through the build process (after building stage1) and if it fails it does so silently and the readline feature is simply not used. This means you end up with a useless ghci. See [\#2970](https://gitlab.haskell.org//ghc/ghc/issues/2970).


So it is necessary to pass these flags to `./configure`:

```wiki
./configure --with-readline-includes=/opt/csw/include --with-readline-libraries=/opt/csw/lib
```


If you want to double-check that ghci really did get built with readline support before you install it then run:

```wiki
ldd compiler/stage2/ghc-6.8.3
```


And check that it really does link to readline.

## Split objects


With the right toolchain this can work. It's been tested on Solaris 10 with ghc-6.8.3, gcc-4.1.2 and the system (not GNU) binutils (ie as, ld etc from /usr/ccs/bin).


If you run into problems however turn it off by adding this to your `mk/build.mk`:

```wiki
SplitObjs=NO
```


Note that to use split objects at the moment you need your gcc to default to the SPARC V9 ABI or to tell ghc to tell gcc to use the V9 ABI it when assembling (`-opta-mcpu=v9`) otherwise you'll hit bug [\#2872](https://gitlab.haskell.org//ghc/ghc/issues/2872).

## Putting it all together


This example uses ghc-6.8.3 on Solaris 10, using gmp and other tools installed in `/opt/csw`. The gcc is 4.1.2 installed in `/opt/ghc-vanilla/4.1.2/bin`. The bootstrapping ghc is the binary from the ghc download page installed in `/opt/ghc-bin`.


The `mk/build.mk` file is

```wiki
SRC_HC_OPTS=-optl-R/opt/csw/lib
SplitObjs=YES
```


The `$PATH`

```wiki
export PATH=/opt/gcc-vanilla/4.1.2/bin:/opt/csw/bin:/usr/bin:/usr/ccs/bin
```


The `LD_LIBRARY_PATH`

```wiki
export LD_LIBRARY_PATH=/opt/csw/lib
```


The `./configure` options

```wiki
./configure --prefix=/opt/ghc \
            --with-ghc=/opt/ghc-bin/bin/ghc \
            --with-gcc=/opt/gcc-vanilla/4.1.2/bin/gcc \
            --with-gmp-includes=/opt/csw/include \
            --with-gmp-libraries=/opt/csw/lib
            --with-readline-includes=/opt/csw/include \
            --with-readline-libraries=/opt/csw/lib
```


Remember of course that you must use GNU make, not the system make. The Solaris 10 `/usr/sfw/bin/gmake` should do. Any other GNU make that you install from a third party repository should also be ok.

```wiki
gmake -j4
```


If you are lucky enough to have a box with lots of CPU cores then use them! Sadly the maximum number that it can actually use effectively is around 4. Hopefully the new build system in ghc-6.11 and later will be able to use more.


It is worth checking at this point that ghc will run without `LD_LIBRARY_PATH` set:

```wiki
unset LD_LIBRARY_PATH
ldd compiler/stage2/ghc-6.8.3
```


This should report all libs as being found. (If you expect to use readline then check it is also linked to readline.)


Now to install:

```wiki
sudo gmake install
```


Remember that you will now need to modify the `rts` package for the newly installed ghc so that the programs it builds will be able to find the gmp lib at runtime. See the section above on using gmp from a non-standard location for more details.

```wiki
export PATH=/opt/ghc/bin:$PATH
ghc-pkg describe rts > rts.pkg
vim rts.pkg     # add -R/opt/csw/lib to the ld-options field.
ghc-pkg update rts.pkg
```


Now check that compiling and running a hello world program works:

```wiki
$ unset LD_LIBRARY_PATH
$ which ghc
/opt/ghc/bin/ghc
$ echo 'main = print "hello"' > Hello.hs
$ ghc --make Hello.hs
$ ./Hello
"hello"
```

## TODO

- link to expected testsuite results.
