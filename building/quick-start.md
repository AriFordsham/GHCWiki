# Quick Start to just building GHC


Below are quick instructions for **just building and installing GHC**. If you are an aspiring GHC developer, read the [Newcomers](newcomers) page instead.


The following instructions assume that you have [got the sources](building/getting-the-sources) and [installed the necessary tools](building/preparation).


If you are starting from a `git` checkout then:

```wiki
$ ./boot
```


(this step isn't necessary if you have a source distribution).


Next on non-Windows:

```wiki
$ ./configure
$ make
```


On Windows:


NOTE: On Windows you need to download some binary distributables before being able to build.
This only has to be done once and can be done by adding a flag to the call to configure

```wiki
$ ./configure --enable-tarballs-autodownload
$ make
```


This will do a 2-stage bootstrap build of the compiler, with profiling libraries.

## Run without installing


You can find the binaries built by make in `inplace` directory under the root of the ghc source tree. The binaries can be run from here without installing.

## Installing


After building, to install GHC (by default in `/usr/local`):

```wiki
$ make install
```