# Compiling for 32 bits on 64 bits


For now, this page is focused on installing and using a stock x86 (i386) binary GHC distribution on x86_64 (amd64) Ubuntu computers. But, in principle, compiling for 32 bits on 64 bits should be possible on any architecture and OS that has multi-arch capabilities. The problems and workarounds may be similar, too (and similar to problems with cross-compilation). Please add your experience reports here. So far, this is known to work on Ubuntu precise, LTS 12.04.4, GHC 7.8.3, cabal 1.20.


Compiling i386 Haskell programs on a x86_64 system can be accomplished by building a proper [cross-compiler](cross-compilation). The method described below doesn't involve building GHC with different host and target, but does involve running a (stock binary distribution of) GHC with i386 target (and intended host) on a by-default x86_64 OS (the actual host). See #9421 for context and common problems. Please report any new spotted problems there.

## Installing the i386 GHC

- Make sure you have the x86_64/i386 multi-arch system in place and install the i386 libs you are going to link to, e.g., zlib1g-dev:i386 and the ia32-libs set on Ubuntu 12.04 and individual libraries on newer Ubuntu versions.
- Either install libgmp-dev:i386 if your OS permits both the i386 and x86_64 versions (Ubuntu 12.04 doesn't; you can also force-override the ban) or hack around by making a symlink, e.g., 

  ```wiki
  sudo ln -s /usr/lib/i386-linux-gnu/libgmp.so.10.0.2 /usr/lib/i386-linux-gnu/libgmp.so
  ```
- Download a stock binary distribution of i386 GHC.
- Optionally isolate your i386 and x86_64 GHCs. They correctly keep their installed packages in .cabal and .ghc separated, but some 64bit tools in .cabal/bin or /usr/local may not work with 32bit code (and vice versa) and diagnosing this takes time. You can set up isolated GHC sandboxes using the instructions in [http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes) or using [ http://hackage.haskell.org/package/virthualenv](http://hackage.haskell.org/package/virthualenv)
- Unpack the 32bit GHC.
- Run 

  ```wiki
  CFLAGS=-m32 ./configure --prefix=your_prefix
  ```
- Run 

  ```wiki
  make install
  ```
- For GHC 7.10.3, setting `CFLAGS` at configure time is enough to allow the installation to succeed, but doesn't tell the installed compiler to pass `-m32` to `gcc`. Edit the settings file (`your_prefix/lib/ghc-X.Y.Z/settings`) to add `-m32` to `"C compiler flags"`.
- To avoid linking to the wrong version of zlib, when you compile packages that depend on it, you may need to install the haskell package zlib specially:

  ```wiki
  cabal install zlib --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386" --hsc2hs-options="--cflag=-m32 --lflag=-m32"
  ```

## Compiling i386 binaries


Assuming that the i386 ghc is in you path (otherwise add `-w your_prefix/bin/ghc`), you should now be able to build 32bit GHC packages via

```wiki
cabal install --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386"
```


Please report any variations (or simplifications) that are required on your system.
