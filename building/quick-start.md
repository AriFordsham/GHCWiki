# Quick Start to building and installing GHC


Below are quick instructions for **just building and installing GHC**. For instructions on how to **work on GHC**, go to the [Quick Start to using the build system as a developer](building/hacking).


The following instructions assume that you have [got the sources](building/getting-the-sources) (note: not just a `git clone`) and [installed the necessary tools](building/preparation).


If you are starting from a `git` checkout then:

```wiki
$ perl boot
```


(this step isn't necessary if you have a source distribution).


Next:

```wiki
$ ./configure
$ make
$ make install
```


This will do a 2-stage bootstrap build of the compiler, with profiling libraries, and install the results in the default location (under `/usr/local` on Unix, for example).


The `configure` script is a standard GNU `autoconf` script, and accepts the usual options for changing install locations and the like.  Run `./configure --help` for a list of options.

**NOTE**: *If you had to install Happy and/or Alex using cabal you will need to override your PATH so cabal's version of happy and alex are used instead of the system versions.*

```wiki
$ PATH=/home/user/.cabal/bin:$PATH ./configure
$ make
$ make install
```