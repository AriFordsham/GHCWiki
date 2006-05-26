
This page is meant to document the current state of GHC on Apple Mac OS X on Intel x86 hardware.


DarwinPorts still has GHC as not supported on x86.


This GHC CVS build seems to work quite well:
[ http://www.uni-graz.at/imawww/haskell/ghc-6.5.20060409-i386-apple-darwin.tar.bz2](http://www.uni-graz.at/imawww/haskell/ghc-6.5.20060409-i386-apple-darwin.tar.bz2)
Some issues:

- You need to manually install GMP.framework in /Library/Frameworks, and libreadline.dylib (and the libreadline\*.dylib symlinks, I guess) in /usr/local/lib (assuming prefix=/usr/local).
- When using runghc to build some cabal packages (e.g. [ xhtml](http://www.cs.chalmers.se/~bringert/darcs/haskell-xhtml/doc/)), the setup program dies silently after building the archive, but before writing the .installed-pkg-config file. If the setup program is compiled with ghc, this does not happen.
- When using "./configure --prefix=/usr/local; make install", GHC gets installed in /usr/local/lib/-6.5/.
