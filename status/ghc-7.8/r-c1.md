**Known issues**:

- Mavericks suffers from some preprocessing bugs. We're going to try using `cpphs` as an alternative.
- The Linux binary builds require glibc 2.15 (Ubuntu 12.04.) RC2 will use glibc 2.13 (Debian 7/stable) instead.
- [ hackage:cabal-install-1.18.0.2](http://hackage.haskell.org/package/cabal-install-1.18.0.2) is not build-able with 7.8.1; if you want to build `cabal-install` with RC1 you need check out latest version from the [ Cabal 1.18 branch](https://github.com/haskell/cabal/tree/1.18) until [ hackage:cabal-install-1.18.0.3](http://hackage.haskell.org/package/cabal-install-1.18.0.3) is released.

## Tickets scheduled for 7.8.1


See [milestone:7.8.1](/trac/ghc/milestone/7.8.1)


Note however, that not all tickets with a 7.8.1 milestone will be addressed for the final 7.8.1 release.

## Tickets filed against RC1

## Status: closed (44 matches)

<table><tr><th>Ticket (Ticket query: version: 7.8.1-rc1, milestone: 7.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: version: 7.8.1-rc1, milestone: 7.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: version: 7.8.1-rc1, milestone: 7.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: version: 7.8.1-rc1, milestone: 7.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: version: 7.8.1-rc1, milestone: 7.8.1, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#5013](https://gitlab.haskell.org//ghc/ghc/issues/5013)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[sporadic failures during compilation under solaris](https://gitlab.haskell.org//ghc/ghc/issues/5013)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5682](https://gitlab.haskell.org//ghc/ghc/issues/5682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Properly parse promoted data constructor operators](https://gitlab.haskell.org//ghc/ghc/issues/5682)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7134](https://gitlab.haskell.org//ghc/ghc/issues/7134)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.6.0.20120810-x86_64-windows.exe -\> internal error R_X86_64_PC32](https://gitlab.haskell.org//ghc/ghc/issues/7134)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8631](https://gitlab.haskell.org//ghc/ghc/issues/8631)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Need ImpredicativeTypes for GeneralizedNewtypeDeriving?](https://gitlab.haskell.org//ghc/ghc/issues/8631)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8698](https://gitlab.haskell.org//ghc/ghc/issues/8698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[.ctors handling does not work on Windows 64-bit ghci](https://gitlab.haskell.org//ghc/ghc/issues/8698)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>ezyang</th></tr>
<tr><th>[\#8700](https://gitlab.haskell.org//ghc/ghc/issues/8700)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cross-compilation perf-cross BuildFlavour](https://gitlab.haskell.org//ghc/ghc/issues/8700)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8705](https://gitlab.haskell.org//ghc/ghc/issues/8705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type inference regression with local dictionaries](https://gitlab.haskell.org//ghc/ghc/issues/8705)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8706](https://gitlab.haskell.org//ghc/ghc/issues/8706)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Kind operators not parsed](https://gitlab.haskell.org//ghc/ghc/issues/8706)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8717](https://gitlab.haskell.org//ghc/ghc/issues/8717)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Segfault in 64-bit Windows GHCi](https://gitlab.haskell.org//ghc/ghc/issues/8717)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8719](https://gitlab.haskell.org//ghc/ghc/issues/8719)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[clarify prefetch release notes + remove some deadcode](https://gitlab.haskell.org//ghc/ghc/issues/8719)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8722](https://gitlab.haskell.org//ghc/ghc/issues/8722)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[powerpc64: error Cannot find a way to declare the thread-local gc variable](https://gitlab.haskell.org//ghc/ghc/issues/8722)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thoughtpolice</th></tr>
<tr><th>[\#8724](https://gitlab.haskell.org//ghc/ghc/issues/8724)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[x86_64 cas references unsigned int rather than StgWord](https://gitlab.haskell.org//ghc/ghc/issues/8724)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8726](https://gitlab.haskell.org//ghc/ghc/issues/8726)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[integer-gmp division regression](https://gitlab.haskell.org//ghc/ghc/issues/8726)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>hvr</th></tr>
<tr><th>[\#8728](https://gitlab.haskell.org//ghc/ghc/issues/8728)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Segmentation fault in Cabal](https://gitlab.haskell.org//ghc/ghc/issues/8728)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8734](https://gitlab.haskell.org//ghc/ghc/issues/8734)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[7.8.1 rc1 ghci won't load compiled files](https://gitlab.haskell.org//ghc/ghc/issues/8734)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8735](https://gitlab.haskell.org//ghc/ghc/issues/8735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hpc crashes on platforms using dynamic linking](https://gitlab.haskell.org//ghc/ghc/issues/8735)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8739](https://gitlab.haskell.org//ghc/ghc/issues/8739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[($) returning kind \# no longer type checks](https://gitlab.haskell.org//ghc/ghc/issues/8739)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8741](https://gitlab.haskell.org//ghc/ghc/issues/8741)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\`System.Directory.getPermissions\` fails on read-only filesystem](https://gitlab.haskell.org//ghc/ghc/issues/8741)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>AlainODea</th></tr>
<tr><th>[\#8743](https://gitlab.haskell.org//ghc/ghc/issues/8743)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[The impossible happened : Prelude.(!!): index too large](https://gitlab.haskell.org//ghc/ghc/issues/8743)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>nomeata</th></tr>
<tr><th>[\#8744](https://gitlab.haskell.org//ghc/ghc/issues/8744)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["Thread-local storage not supported" for Mac](https://gitlab.haskell.org//ghc/ghc/issues/8744)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8745](https://gitlab.haskell.org//ghc/ghc/issues/8745)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GeneralizedNewtypeDeriving is still not Safe](https://gitlab.haskell.org//ghc/ghc/issues/8745)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8748](https://gitlab.haskell.org//ghc/ghc/issues/8748)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc-7.8-rc1/HEAD: --enable-unregisterised fails to build and run with threaded RTS, profiling mode](https://gitlab.haskell.org//ghc/ghc/issues/8748)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8749](https://gitlab.haskell.org//ghc/ghc/issues/8749)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms crash GHCi](https://gitlab.haskell.org//ghc/ghc/issues/8749)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8754](https://gitlab.haskell.org//ghc/ghc/issues/8754)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[:set +s always says space usage is 0 bytes](https://gitlab.haskell.org//ghc/ghc/issues/8754)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#8757](https://gitlab.haskell.org//ghc/ghc/issues/8757)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Disallow local pattern synonym declarations](https://gitlab.haskell.org//ghc/ghc/issues/8757)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>cactus</th></tr>
<tr><th>[\#8758](https://gitlab.haskell.org//ghc/ghc/issues/8758)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GeneralizedNewtypeDeriving sometimes needs RankNTypes](https://gitlab.haskell.org//ghc/ghc/issues/8758)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#8759](https://gitlab.haskell.org//ghc/ghc/issues/8759)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonyms and TH](https://gitlab.haskell.org//ghc/ghc/issues/8759)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#8760](https://gitlab.haskell.org//ghc/ghc/issues/8760)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc 7.8: ghc-split not installed](https://gitlab.haskell.org//ghc/ghc/issues/8760)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8762](https://gitlab.haskell.org//ghc/ghc/issues/8762)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic involving unboxed tuples and phantom types](https://gitlab.haskell.org//ghc/ghc/issues/8762)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8764](https://gitlab.haskell.org//ghc/ghc/issues/8764)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Usage of \`sed' in GHC build system (Solaris build failure).](https://gitlab.haskell.org//ghc/ghc/issues/8764)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8766](https://gitlab.haskell.org//ghc/ghc/issues/8766)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[length \[Integer\] is twice as slow but length \[Int\] is 10 times faster](https://gitlab.haskell.org//ghc/ghc/issues/8766)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8770](https://gitlab.haskell.org//ghc/ghc/issues/8770)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghci dynamic loading on MacOSX assumes .dylib extension (should check .so also)](https://gitlab.haskell.org//ghc/ghc/issues/8770)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8773](https://gitlab.haskell.org//ghc/ghc/issues/8773)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Require -XIncoherentInstances to write role annotations on class definitions](https://gitlab.haskell.org//ghc/ghc/issues/8773)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#8786](https://gitlab.haskell.org//ghc/ghc/issues/8786)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[libraries/primitive build failure on Solaris 10](https://gitlab.haskell.org//ghc/ghc/issues/8786)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8792](https://gitlab.haskell.org//ghc/ghc/issues/8792)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[doc incorrect: Mac OS X does not supply lllvm i.e. opt and llc](https://gitlab.haskell.org//ghc/ghc/issues/8792)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8794](https://gitlab.haskell.org//ghc/ghc/issues/8794)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Unresolved \@ArSupportsAtFile@ on Solaris distribution.](https://gitlab.haskell.org//ghc/ghc/issues/8794)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th>kgardas</th></tr>
<tr><th>[\#8795](https://gitlab.haskell.org//ghc/ghc/issues/8795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Build fails on Solaris 10 due to missing ranlib](https://gitlab.haskell.org//ghc/ghc/issues/8795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>kgardas</th></tr>
<tr><th>[\#8797](https://gitlab.haskell.org//ghc/ghc/issues/8797)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Generics instances for monoid and applicative newtypes](https://gitlab.haskell.org//ghc/ghc/issues/8797)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8800](https://gitlab.haskell.org//ghc/ghc/issues/8800)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[hpc dyld issue](https://gitlab.haskell.org//ghc/ghc/issues/8800)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#8801](https://gitlab.haskell.org//ghc/ghc/issues/8801)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Exclude extralibs from sdist](https://gitlab.haskell.org//ghc/ghc/issues/8801)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8805](https://gitlab.haskell.org//ghc/ghc/issues/8805)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[QuasiQuote should imply -dynamic-too, too](https://gitlab.haskell.org//ghc/ghc/issues/8805)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8810](https://gitlab.haskell.org//ghc/ghc/issues/8810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[rts/RetainerProfile.c: include missing header for 'markStableTables'](https://gitlab.haskell.org//ghc/ghc/issues/8810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8813](https://gitlab.haskell.org//ghc/ghc/issues/8813)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[further support deriving instances Typeable1, Typeable2, etc](https://gitlab.haskell.org//ghc/ghc/issues/8813)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>dreixel</th></tr>
<tr><th>[\#8817](https://gitlab.haskell.org//ghc/ghc/issues/8817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[segmentation fault in 7.8 RC1](https://gitlab.haskell.org//ghc/ghc/issues/8817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>simonmar</th></tr></table>