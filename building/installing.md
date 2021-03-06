# Installing GHC from a build tree


After GHC has been built, it can be installed as follows:

## Non-Windows


There are two methods available; you can install directly from your built source tree, or you can build a binary distribution and then unpack and install it.

### Installing from a built source tree

```wiki
$ make install
```


or

```wiki
$ make install-strip
```


This will build anything that isn't up-to-date, copy the files into the right places (see below) and make sure all the packages are registered properly. The `install-strip` variant strips executable files while installing them.


GHC will be installed to the directory that you specified at configure time with the `--prefix` option. If you forgot to do so, then rather than reconfiguring and rebuilding, it's probably faster to build and install from a binary distribution as described below.

### Building and installing from a binary distribution

```wiki
$ make binary-dist
```


This will produce a binary distribution in the root of source tree with a name like `ghc-XXXXXX.tar.bz2`. Unpack it somewhere, `cd` into the unpacked tree and run

```wiki
$ ./configure --prefix=<my-dir>                # to specify the installation directory
$ make install
```

## Windows

```wiki
$ make binary-dist
```


This will make a compressed tar file and place it in the root of the source directory. On Windows the binary distribution is simply the install tree containing GHC and its libraries, so you can install it like any Windows distribution. The Windows installation is relocatable (see below) so there is no need to specify the final installation directory in advance.

## Layout of the installed files


This section describes how the files of a GHC installation are laid out.  The root of the GHC installation is specified via the `--prefix` flag to `configure` (see [Running the configure script](building/using#run-the-configure-script)), and we refer to that location as `$(prefix)`.  If you are wondering what `$(prefix)` is, and hence where the make system is going to install GHC, just say (see [debugging the build system](building/modifying#debugging))

```wiki
$ make show VALUE=prefix
```


A GHC installation typically has three parts:

- **`bindir`**

  (default: `$(prefix)/bin`)
  This is where the programs that you can run are installed, such as `ghc`, `ghci`, `ghc-pkg`, and `haddock`.

- **`libdir`**

  (default: Unix: `$(prefix)/lib/ghc-<version>`, Windows: `$(prefix)/lib`)

  Where all of GHC's support files are kept, including `package.conf`, the header files, and the libraries.

The location of `libdir` can be found by asking GHC: `ghc --print-libdir`.  Normally you shouldn't have to
look in here, and you shouldn't install extra files here.  The only reason you might need to know the location
of `libdir` at all is for passing to the GHC API, and the best way to do that is to use the [ghc-paths package](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/ghc-paths).

- **`docdir`**

  (default: Unix: `$(prefix)/share/doc/ghc`, Windows: `$(prefix)/doc`)
  Where the documentation is installed.


On Unix systems you can change `libdir` and `bindir` using the `--libdir` and `--bindir` options respectively, and the location of the documentation can be changed using `--datadir`.  On Windows all you can do is change `$(prefix)`, because GHC finds the rest of its files by knowing their location relative to the `ghc.exe` binary, so the layout of the install tree is fixed (see [How GHC finds its files](building/installing#how-ghc-finds-its-files), below).


To see how the install directories are derived from `$(prefix)`, look in [mk/install.mk.in](https://gitlab.haskell.org/ghc/ghc/tree/master/mk/install.mk.in).

## DESTDIR


It is common to want `make install` to install the files somewhere other than their final location.  That is, we want to perform the normal installation operations but have the files be copied to a new empty location so that we can pack up the files for distribution and later installation.  This is achieved by setting `DESTDIR` when installating:

```wiki
$ make install DESTDIR=/tmp/ghc-install
```


this will install as usual, but prepend $(DESTDIR) to every file that is installed.  So for example if $(bindir) is `/usr/bin`, then the `ghc` binary will end up in `/tmp/ghc-install/usr/bin/ghc`.


Note that you will not then be able to actually run the compiler from this location, since the installed tree contains absolute paths that refer to the final location where it expects to be installed, as described below. If you want to install to a location other than the default, you must specify that location when configuring either the source tree or a binary distribution.

## The installed copy of MinGW on Windows


On Windows, GHC also comes with a copy of (most of) [MinGW](http://www.mingw.org), in `$(prefix)/mingw`.  So for instance, you can invoke the `gcc` that comes with GHC as `$(prefix)/mingw/bin/gcc` (replacing `$(prefix)` appropriately).

## How GHC finds its files


GHC, when it starts, needs to find `libdir`, so that it can read `package.conf`, and find things like the `unlit` program.  It does this in one of two ways:

- On **Windows**, `libdir` is in a fixed location relative to the `ghc.exe` binary, namely `../lib`.  The advantage of the Windows way is that the installed GHC on Windows is independent of its location; it can be moved anywhere on the system, as long as the layout of the tree remains intact (however, links from the start menu and other shortcuts will break if you do this). 

- On **Unix**, it is standard to be able to change `libdir` relative to `bindir`, and also it is typically hard (or
  at least non-portable) to find the pathname to a running binary.  So on Unix systems we use a different method: we make a wrapper script that passes the location of `libdir` to the GHC binary
  proper using the `-B` flag.  Therefore, `$(bindir)/ghc` is a script that invokes `$(libdir)/ghc`
  passing it `-B<libdir>`, and the rest of the command-line arguments.  All the other files that GHC
  needs are found by reading the `package.conf` file.

## Relocating a GHC installation


On Windows, because GHC finds its files relative to itself, the whole installed tree can be relocated elsewhere in the filesystem, and everything will continue to work (except that if there are shortcuts or links from elsewhere, such as start menu items, these will need to be updated to point to the new location).


On Unix, GHC installations contain hardcoded paths in the `package.conf` file and also in the wrapper scripts for `ghc` and `ghc-pkg` in `$bindir`.  So relocating the bits of a GHC installation on a Unix system is much harder; these paths would have to be updated manually.
