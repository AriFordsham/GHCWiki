# Cross Compiling GHC


As of this moment (GHC 6.12) GHC does not support cross-compilation.  There are reasons that we would like it to:

- [ TakeoffGW](http://takeoffgw.sourceforge.net/) is a distribution of Unix tools for Windows, built by cross-compiling on a Linux machine.  They would like to be able to build and distribute GHC this way.  It might be useful for us to be able to cross-compile a Windows GHC from Linux too.

- We could build a 64-bit GHC on OS X, by cross-compiling using the 32-bit version.

- We could port to Win64 ([\#1884](https://gitlab.haskell.org//ghc/ghc/issues/1884)) by cross-compiling using a 32-bit Windows GHC.

- Other porting tasks might be easier, given a suitable cross-compilation toolchain.


By way of example, let's suppose we have an x86/Linux platform and we want to cross-compile to PPC64/OSX.  Then our build is going to look like this:

<table><tr><th>**Compiler**</th>
<th>**Runs on**</th>
<th>**Generates code for**</th></tr>
<tr><th> Stage 0       </th>
<th> x86/Linux  </th>
<th> x86/Linux          
</th></tr>
<tr><th> Stage 1       </th>
<th> x86/Linux  </th>
<th> PPC64/OSX          
</th></tr>
<tr><th> Stage 2       </th>
<th> PPC64/OSX  </th>
<th> PPC64/OSX          
</th></tr></table>


Where stage 0 is the bootstrap compiler (the one you specify using `--with-ghc` when configuring the build), and stages 1 and 2 are the compilers being built.


Now some general nomenclature:

- **Build platform**: the platform on which the software is being built
- **Host platform**: the platform on which the software will run
- **Target platform**: for a compiler, the platform on which the generated code will run


These correspond to CPP symbols that are defined when compiling both Haskell and C code:

- *xxx*`_BUILD_ARCH`, *xxx*`_BUILD_OS`: the build platform
- *xxx*`_HOST_ARCH`, *xxx*`_HOST_OS`: the host platform
- *xxx*`_TARGET_ARCH`, *xxx*`_TARGET_OS`: the target platform


The important thing to realise about the 2-stage bootstrap is that each stage has a different notion of build/host/target: these CPP symbols will map to different things when compiling stage 1 and stage 2.  Furthermore the RTS and libraries also have a notion of build and host (but not target: they don't generate code).


The overall build has a build/host/target, supplied on the `configure` command line:

> `$ ./configure --build=`*build*` --host=`*host*` --target=`*target*


And here is how we map those platforms onto the platforms used by the different stages, and the RTS and libraries:

<table><tr><th></th>
<th>**Overall build**</th>
<th>**Stage 0**</th>
<th>**Stage 1**</th>
<th>**Stage 2**</th>
<th>**libs-host**</th>
<th>**libs-target**</th></tr>
<tr><th>**Build platform**</th>
<th>*build*</th>
<th>--- </th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th></tr>
<tr><th>**Host platform**</th>
<th>*host*</th>
<th>*build*</th>
<th>*build*</th>
<th>*host*</th>
<th>*host*</th>
<th>*target*</th></tr>
<tr><th>**Target platform**</th>
<th>*target*</th>
<th>*build*</th>
<th>*host*</th>
<th>*target*</th>
<th> ---       </th>
<th> --- 
</th></tr></table>


Where **libs-host** refers to the libraries and RTS that we are building to link with the stage 2 compiler, and **libs-target** refers to the libraries and RTS that will be linked with binaries built by the stage 2 compiler to run on the target platform.


In the special case where we are using cross compilation to bootstrap a new platform, as in the above example, we have *host* == *target*:

<table><tr><th></th>
<th>**Overall build**</th>
<th>**Stage 1**</th>
<th>**Stage 2**</th>
<th>**libs-host**</th></tr>
<tr><th>**Build platform**</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th></tr>
<tr><th>**Host platform**</th>
<th>*target*</th>
<th>*build*</th>
<th>*target*</th>
<th>*target*</th></tr>
<tr><th>**Target platform**</th>
<th>*target*</th>
<th>*target*</th>
<th>*target*</th>
<th></th></tr></table>


Note that with *host* == *target*, **libs-host** == **libs-target**, so we only need to build the RTS and libraries once (fortunately, because the GHC build system only supports building them once).


Suppose we wanted to build a cross-compiler to run on the current platform.  Then we could configure with *build* == *host*, but *target* is different:

<table><tr><th></th>
<th>**Overall build**</th>
<th>**Stage 1**</th>
<th>**Stage 2**</th>
<th>**libs-host**</th>
<th>**libs-target**</th></tr>
<tr><th>**Build platform**</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th></tr>
<tr><th>**Host platform**</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*build*</th>
<th>*target*</th></tr>
<tr><th>**Target platform**</th>
<th>*target*</th>
<th>*build*</th>
<th>*target*</th>
<th></th>
<th></th></tr></table>


Note in this configuration that we need both **libs-host** and **libs-target**, so currently the GHC build system does not support building this kind of cross-compiler.  Fortunately, most of the things you would want to do with this kind of cross-compiler are supported by the first kind, the only caveat is that you can't *install* a cross-compiler that way.

## Plan


Here is how it should work:

```wiki
$ ./configure --build=<here> --host=<there> --target=<there>
```


note that we're cross-compiling from the *build* machine to the *host* machine.  The *target* machine is the same as the *host*: the GHC that we're trying to create will generate binaries for *host*.


No doubt we'll also need to specify some additional configuration parameters to tell the build system where to find our cross-compilation tools.  Perhaps something like

```wiki
--with-host-cc=...
--with-host-as=...
--with-host-ld=...
--with-host-ar=...
--with-host-strip=...
```

- stage 1: runs on `build`, compiles for `host`
- stage 2: runs on `host`, compiles for `host`

## Things that probably need fixing

- The configure script doesn't let you specify different `build`, `host`, and `target` right now
- The build systme has no distinction between the gcc used to compile from build-\>build and build-\>host.
- We can't build anything with stage2 when cross-compiling, e.g. Haddock and DPH must be disabled.
