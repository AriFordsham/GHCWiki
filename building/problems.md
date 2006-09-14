# Common build problems

## General builds

### GCC 4 issues


It has been observed on Gentoo systems that GCC 4 may fail, complaining about there being no `-nopie` option. Try using GCC 3.

## Building from the Darcs tree


If you see ...

```wiki
  /usr/bin/ghc -M -optdep-f -optdep.depend  -osuf o -optdep--exclude-module=System.Directory.Internals   -H32m -O -fasm -Rghc-timing -I. -Iinclude -Rghc-timing -O0 -ignore-package Cabal -I../libraries -fglasgow-exts -no-recomp Compat/Directory.hs Compat/RawSystem.hs Compat/Unicode.hs Distribution/Compat/FilePath.hs Distribution/Compat/ReadP.hs Distribution/Compiler.hs Distribution/GetOpt.hs Distribution/InstalledPackageInfo.hs Distribution/License.hs Distribution/Package.hs Distribution/ParseUtils.hs Distribution/Version.hs Language/Haskell/Extension.hs
  Distribution/Compat/FilePath.hs:2: error: Cabal/Distribution/Compat/FilePath.hs: No such file or directory
  <<ghc: 13635708 bytes, 2 GCs, 104796/104796 avg/max bytes residency (1 samples), 24M in use, 0.00 INIT (0.00 elapsed), 0.02 MUT (0.12 elapsed), 0.00 GC (0.01 elapsed) :ghc>>
  make[1]: *** [depend] Error 1
  make: *** [stage1] Error 1
```


... be sure you have run `sh darcs-all get` to get all necessary packages.
