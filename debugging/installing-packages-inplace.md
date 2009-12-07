# Installing packages in your test compiler


You have your compiler built, so you can use the inplace compiler to compile test programs.  This lives in `$(TOP)/inplace/bin/ghc-stage2` (see [Commentary/SourceTree](commentary/source-tree)).  


But sometimes a test case will require the installation of some packages from Hackage.  There are two routes.

## Plan A: Cabal is up to date


If your installed `cabal` is sufficiently up to date, you can say

```wiki
cabal install --with-ghc=<inplace-ghc> --global <package>
```


where `<inplace ghc>` is the path to your inplace GHC (usually `$(TOP)/inplace/bin/ghc-stage2`), and \<package\> is the name of the package.
The `--global` says to register the package in the global database, which for the inplace compiler is something like `$(TOP)/inplace/lib/package.conf.d/`.


But sometimes Cabal changes, so you might get a message like

```wiki
cabal: failed to parse output of 'ghc-pkg dump'
```


In that case you need to use the Cabal code that comes with the new version of GHC (ie the one in your build tree).  So use Plan B.

## Plan B: Cabal is out of date


Go to a directory where you are happy to keep the newly-downloaded code.

```wiki
cabal unpack <package>
cd <package>
<inplace-ghc> --make Setup.lhs -o Setup
./Setup configure --with-ghc=<inplace-ghc> --global <package>
./Setup build
./Setup register --inplace
```


The first step can be done manually, by downloading from Hackage, but it should work even with old versions of Cabal.
