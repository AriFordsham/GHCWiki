# Quick Start: Just building and installing GHC

>
> "I just want to build it!"


No problem.  This recipe should build and install a working GHC with
all the default settings.  (unless you're on Windows, in which case go
to [Building/Windows](building/windows)).


The following instructions assume that you have [got the sources](building/getting-the-sources) (note: not just a `darcs get`) and [installed the necessary tools](building/prerequisites).


If you are starting from a `darcs` checkout(or rebuilding from darcs), then:

```wiki
$ sh boot
```


(this step isn't necessary if you have a source distribution).


Now, everybody:

```wiki
$ ./configure
$ make
$ make install
```


For GHC, this will do a 2-stage bootstrap build of the compiler, with
profiling libraries, and install the results in the default location
(under `/usr/local` on Unix, for example).


The `configure` script is a standard GNU
`autoconf` script, and accepts the usual options for
changing install locations and the like.  Run
`./configure --help` for a list of
options.


If you want to do anything at all non-standard, or you
want to do some development, read on...
