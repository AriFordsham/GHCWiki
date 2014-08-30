# Compiling for 32 bits on 64 bits


This is Cross-Compilation *Light*. It doesn't involve building GHC with different host and target, but does involve running 
a GHC with i386 target (and host) on a by-default x86_64 OS. This should be possible on any OS that has multi-arch capabilities,
but the instructions are only for Ubuntu (tested on Ubuntu precise, LTS 12.04.4, GHC 7.8.3, cabal 1.20). Some of the problems are similar
to those encountered when really cross-compiling. See [\#9421](https://gitlab.haskell.org//ghc/ghc/issues/9421) for context and common problems. Please report any new spotted problems there.

## Installing the i386 GHC

- Make sure you have the x86_64/i386 multi-arch system in place and install the i386 libs you are going to link to, e.g., zlib1g-dev:i386 and the ia32-libs set on Ubuntu 12.04 and individual libraries on newer Ubuntu versions.
- To avoid linking to the wrong version of zlib, you may need to install the haskell package zlib specially:

  ```wiki
  cabal install zlib --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386" --hsc2hs-options="--cflag=-m32 --lflag=-m32"
  ```
- Either install libgmp-dev:i386 if your OS permits both the i386 and x86_64 versions (Ubuntu 12.04 doesn't; you can also force-override the ban) or hack around by making a symlink, e.g., 

  ```wiki
  sudo ln -s /usr/lib/i386-linux-gnu/libgmp.so.10.0.2 /usr/lib/i386-linux-gnu/libgmp.so
  ```
- Download a i386 GHC.
- Optionally isolate your i386 and x86_64 GHCs. They correctly keep their installed packages in .cabal and .ghc separated, but some 64bit tools in .cabal/bin or /usr/local may not work with 32bit code (and vice versa) and diagnosing this takes time. You can set up isolated GHC sandboxes using the instructions in [ http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes) or using [ http://hackage.haskell.org/package/virthualenv](http://hackage.haskell.org/package/virthualenv)
- Unpack the 32bit GHC.
- Run 

  ```wiki
  CFLAGS=-m32 ./configure --prefix=your_prefix
  ```
- Run 

  ```wiki
  make install
  ```

## Compiling i386 binaries


Assuming that the i386 ghc is in you path (otherwise add `-w your_prefix/bin/ghc`), you should now be able to build 32bit GHC packages via

```wiki
cabal install --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386"
```


Please report any variations (or simplifications) that are required on your system.
