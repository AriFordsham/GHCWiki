I have this when i "cabal build all" 

Configuring library for aeson-1.5.2.0..
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../haskeline-0.7.4.3/libHShaskeline-0.7.4.3-ghc8.6.5.so)
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../ghc-8.6.5/libHSghc-8.6.5-ghc8.6.5.so)
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../terminfo-0.4.1.2/libHSterminfo-0.4.1.2-ghc8.6.5.so)
/usr/local/lib/ghc-8.6.5/bin/ghc-pkg: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/ghc-pkg)
/usr/local/lib/ghc-8.6.5/bin/ghc-pkg: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../terminfo-0.4.1.2/libHSterminfo-0.4.1.2-ghc8.6.5.so)
Preprocessing library for aeson-1.5.2.0..
Building library for aeson-1.5.2.0..
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../haskeline-0.7.4.3/libHShaskeline-0.7.4.3-ghc8.6.5.so)
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../ghc-8.6.5/libHSghc-8.6.5-ghc8.6.5.so)
/usr/local/lib/ghc-8.6.5/bin/ghc: /lib64/libtinfo.so.5: no version information available (required by /usr/local/lib/ghc-8.6.5/bin/../terminfo-0.4.1.2/libHSterminfo-0.4.1.2-ghc8.6.5.so)
[ 1 of 24] Compiling Data.Aeson.Internal.Functions ( Data/Aeson/Internal/Functions.hs, dist/build/Data/Aeson/Internal/Functions.o )
[ 2 of 24] Compiling Data.Aeson.Parser.UnescapePure ( pure/Data/Aeson/Parser/UnescapePure.hs, dist/build/Data/Aeson/Parser/UnescapePure.o )
[ 3 of 24] Compiling Data.Aeson.Parser.Unescape ( Data/Aeson/Parser/Unescape.hs, dist/build/Data/Aeson/Parser/Unescape.o )
[ 4 of 24] Compiling Data.Aeson.Types.Generic ( Data/Aeson/Types/Generic.hs, dist/build/Data/Aeson/Types/Generic.o )
[ 5 of 24] Compiling Data.Aeson.Types.Internal ( Data/Aeson/Types/Internal.hs, dist/build/Data/Aeson/Types/Internal.o )
[ 6 of 24] Compiling Data.Aeson.Parser.Internal ( Data/Aeson/Parser/Internal.hs, dist/build/Data/Aeson/Parser/Internal.o )
ghc: internal error: Unable to commit 1048576 bytes of memory
    (GHC version 8.6.5 for x86_64_unknown_linux)
    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
cabal: Failed to build aeson-1.5.2.0 (which is required by
test:cardano-node-test from cardano-node-1.19.0, exe:cardano-node from
cardano-node-1.19.0 and others). The build process terminated with exit code
-6

if you know where the problem is... 

