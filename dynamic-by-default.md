# Dynamic by default


Currently, GHCi doesn't use the system linker to load libraries, but instead uses our own "GHCi linker". Unfortunately, this is a large blob of unpleasant code that we need to maintain, it already contains a number of known bugs, and new problem have a tendency to arise as new versions of OSes are released. We are therefore keen to get rid of it!


Even removing it only on particular OSes, arches, or OS/arch pairs would be useful, as much of the code is used only for a particular platform. However, the best outcome would be to remove it on all platforms, as that would allow us to simplify a lot more code.


Our solution is to switch GHCi from using the "static way", to using the "dynamic way". GHCi will then use the system linker to load the `.dll` for the library, rather than using the GHCi linker to load the `.a`.


For this to work, there is technically no need to change anything else: ghc could continue to compile for the static way by default. However, there are 2 problems that arise:

1. cabal-install would need to install libraries not only for the static way (for use by ghc), but also for the dynamic way (for use by ghci). This would double library installation times and disk usage.
1. GHCi would no longer be able to load modules compiled with `ghc -c`.

## Bugs


As well as the [ticket for implementing dynamic-by-default (\#3658)](https://gitlab.haskell.org//ghc/ghc/issues/3658), the table below lists the related tickets and the platforms that they affect. Most, if not all, of these would be immediately fixed by switching to dynamic-by-default.

<table><tr><th>Ticket</th>
<th>Affects OS X x86_64?</th>
<th>Affects OS X x86?</th>
<th>Affects Linux x86_64?</th>
<th>Affects Linux x86?</th>
<th>Affects Windows x86_64?</th>
<th>Affects Windows x86?</th>
<th>Affects other platforms?
</th></tr>
<tr><th>[\#781 GHCi on x86_64, cannot link to static data in shared libs](https://gitlab.haskell.org//ghc/ghc/issues/781)</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown
</th></tr>
<tr><th>[\#1883 GHC can't find library using "short" name](https://gitlab.haskell.org//ghc/ghc/issues/1883)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#2283 WIndows: loading objects that refer to DLL symbols](https://gitlab.haskell.org//ghc/ghc/issues/2283)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#3242 ghci: can't load .so/.DLL for: m (addDLL: could not load DLL)](https://gitlab.haskell.org//ghc/ghc/issues/3242)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#3654 Mach-O GHCi linker lacks support for a range of relocation entries](https://gitlab.haskell.org//ghc/ghc/issues/3654)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#4244 Use system linker in GHCi to support alpha, ia64, ppc64](https://gitlab.haskell.org//ghc/ghc/issues/4244)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th></tr>
<tr><th>[\#5062 Patch: Debug output for OS X linker and coding standard upgrades](https://gitlab.haskell.org//ghc/ghc/issues/5062)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#5197 Support static linker semantics for archives and weak symbols](https://gitlab.haskell.org//ghc/ghc/issues/5197)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th></tr>
<tr><th>[\#5435 GHCi linker should run constructors for linked libraries](https://gitlab.haskell.org//ghc/ghc/issues/5435)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th></tr>
<tr><th>[\#6107 GHCi runtime linker cannot link with duplicate common symbols](https://gitlab.haskell.org//ghc/ghc/issues/6107)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th></tr>
<tr><th>[\#7043 32-bit GHC ceiling of negative float SEGFAULT: 11](https://gitlab.haskell.org//ghc/ghc/issues/7043)</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown
</th></tr>
<tr><th>[\#7056 GHCi loadArchive "libiconv.a":failed Unknown PEi386 section name \`.drectve'](https://gitlab.haskell.org//ghc/ghc/issues/7056)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7072 GHC interpreter does not find stat64 symbol on Linux](https://gitlab.haskell.org//ghc/ghc/issues/7072)</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown</th>
<th>Unknown
</th></tr>
<tr><th>[\#7097 linker fails to load package with binding to foreign library](https://gitlab.haskell.org//ghc/ghc/issues/7097)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7103 Compiler panic, when loading wxc in GHCi](https://gitlab.haskell.org//ghc/ghc/issues/7103)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**probably**</th>
<th>**YES**</th>
<th>no
</th></tr>
<tr><th>[\#7134 ghc-7.6.0.20120810-x86_64-windows.exe -\> internal error R_X86_64_PC32](https://gitlab.haskell.org//ghc/ghc/issues/7134)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7207 linker fails to load package with binding to foreign library (win64)](https://gitlab.haskell.org//ghc/ghc/issues/7207)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7299 threadDelay broken in ghci, Mac OS X](https://gitlab.haskell.org//ghc/ghc/issues/7299)</th>
<th>**YES**</th>
<th>**YES**</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no
</th></tr>
<tr><th>[\#7357 GHC.exe gives an internal error while linking vector's Monadic.hs](https://gitlab.haskell.org//ghc/ghc/issues/7357)</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>no</th>
<th>**YES**</th>
<th>no</th>
<th>no
</th></tr></table>

## Performance


Full nofib results showing the effect of switching to dynamic-by-default are available for 
[ OS X x86_64](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86_64.html),
[ OS X x86](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86.html),
[ Linux x86_64](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-linux-x86_64.html) and
[ Linux x86](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-linux-x86.html). There is also a table of the highlights below. In summary:


Binary sizes are way down across the board, as we are now dynamically linking to the libraries.


Things are rosiest on OS X x86_64. On this platform, `-fPIC` is always on, so using dynamic libraries doesn't mean giving up a register for PIC. Overall, performance is a few percent *better* with dynamic by default.


On OS X x86, the situation is not so nice. On x86 we are very short on registers, and giving up another for PIC means we end up around 15% down on performance.


On Linux x86_64 we have more registers, so the effect of giving one up for PIC isn't so pronounced, but we still lose a few percent performance overall.


For unknown reasons, x86 Linux suffers even worse than x86 OS X, with around a 30% performance penalty.

<table><tr><th></th>
<th>static -\> dynamic
on OS X x86_64</th>
<th>static -\> dynamic
on OS X x86</th>
<th>static -\> dynamic
on Linux x86_64</th>
<th>static -\> dynamic
on Linux x86</th></tr>
<tr><th>Binary Sizes</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-95.8%</th>
<th>-95.8%</th>
<th>-95.8%</th>
<th>-95.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>-93.1%</th>
<th>-92.8%</th>
<th>-92.6%</th>
<th>-92.4%</th></tr>
<tr><th>Average</th>
<th>-94.6%</th>
<th>-94.5%</th>
<th>-94.5%</th>
<th>-94.4%</th></tr>
<tr><th>Run Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.2%</th>
<th>+11.7%</th>
<th>-2.5%</th>
<th>+16.6%</th></tr>
<tr><th>+1 s.d.</th>
<th>+1.6%</th>
<th>+20.0%</th>
<th>+9.6%</th>
<th>+40.3%</th></tr>
<tr><th>Average</th>
<th>+0.2%</th>
<th>+15.8%</th>
<th>+3.3%</th>
<th>+27.9%</th></tr>
<tr><th>Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-6.9%</th>
<th>+10.3%</th>
<th>-2.5%</th>
<th>+16.6%</th></tr>
<tr><th>+1 s.d.</th>
<th>-0.3%</th>
<th>+20.4%</th>
<th>+9.6%</th>
<th>+40.3%</th></tr>
<tr><th>Average</th>
<th>-3.7%</th>
<th>+15.2%</th>
<th>+3.3%</th>
<th>+27.9%</th></tr>
<tr><th>Mutator Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.3%</th>
<th>+8.9%</th>
<th>-5.0%</th>
<th>+18.3%</th></tr>
<tr><th>+1 s.d.</th>
<th>+1.9%</th>
<th>+18.3%</th>
<th>+7.5%</th>
<th>+46.8%</th></tr>
<tr><th>Average</th>
<th>+0.3%</th>
<th>+13.5%</th>
<th>+1.1%</th>
<th>+31.8%</th></tr>
<tr><th>Mutator Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-4.5%</th>
<th>+7.7%</th>
<th>-5.0%</th>
<th>+18.3%</th></tr>
<tr><th>+1 s.d.</th>
<th>+0.3%</th>
<th>+18.8%</th>
<th>+7.5%</th>
<th>+46.8%</th></tr>
<tr><th>Average</th>
<th>-2.1%</th>
<th>+13.1%</th>
<th>+1.1%</th>
<th>+31.8%</th></tr>
<tr><th>GC Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.4%</th>
<th>+16.3%</th>
<th>+5.6%</th>
<th>+13.4%</th></tr>
<tr><th>+1 s.d.</th>
<th>+1.8%</th>
<th>+27.1%</th>
<th>+11.2%</th>
<th>+24.0%</th></tr>
<tr><th>Average</th>
<th>+0.2%</th>
<th>+21.6%</th>
<th>+8.4%</th>
<th>+18.6%</th></tr>
<tr><th>GC Elapsed Time</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.5%</th>
<th>+15.8%</th>
<th>+5.6%</th>
<th>+13.4%</th></tr>
<tr><th>+1 s.d.</th>
<th>+1.3%</th>
<th>+25.6%</th>
<th>+11.2%</th>
<th>+24.0%</th></tr>
<tr><th>Average</th>
<th>-0.1%</th>
<th>+20.6%</th>
<th>+8.4%</th>
<th>+18.6%</th></tr>
<tr><th>Compile Times</th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-11.7%</th>
<th>+6.2%</th>
<th>-1.8%</th>
<th>+27.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>-0.5%</th>
<th>+18.2%</th>
<th>+7.8%</th>
<th>+37.8%</th></tr>
<tr><th>Average</th>
<th>-6.3%</th>
<th>+12.1%</th>
<th>+2.9%</th>
<th>+32.3%</th></tr></table>

## OS X x86 vs x86_64


Currently, some people use the x86 version of GHC on OS X for performance reasons. It's not clear for how much longer this will be viable, as other OS X libraries start dropping x86 support.


Full nofib results comparing the two are
[ here for static by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-base.html), and
[ here for dynamic by default](http://lambda.haskell.org/~igloo/dynamic-by-default/nofib-osx-x86-x86_64-dyn.html), but the highlights are in the table below.


The left-hand column shows the status quo: x86_64 only beats x86 in mutator time, and that is a shallow victory as the higher GC time means that total runtime is worse for x86_64.


The right-hand column shows what the situation would be if we switch to dynamic instead. Allocations, memory use etc remain higher due to all word-sized things being twice as big. However, the combination of x86_64's performance improving, and x86's performance getting worse, means that x86_64 is now faster overall.

<table><tr><th></th>
<th>x86 -\> x86_64
when static by default</th>
<th>x86 -\> x86_64
when dynamic by default</th></tr>
<tr><th>Binary Sizes</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+38.0%</th>
<th>+7.4%</th></tr>
<tr><th>+1 s.d.</th>
<th>+38.6%</th>
<th>+30.6%</th></tr>
<tr><th>Average</th>
<th>+38.3%</th>
<th>+18.5%</th></tr>
<tr><th>Allocations</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+63.2%</th>
<th>+63.2%</th></tr>
<tr><th>+1 s.d.</th>
<th>+114.4%</th>
<th>+114.4%</th></tr>
<tr><th>Average</th>
<th>+87.0%</th>
<th>+87.0%</th></tr>
<tr><th>Run Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-23.5%</th>
<th>-31.6%</th></tr>
<tr><th>+1 s.d.</th>
<th>+36.1%</th>
<th>+14.7%</th></tr>
<tr><th>Average</th>
<th>+2.1%</th>
<th>-11.4%</th></tr>
<tr><th>Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-18.2%</th>
<th>-30.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>+40.1%</th>
<th>+17.0%</th></tr>
<tr><th>Average</th>
<th>+7.0%</th>
<th>-9.5%</th></tr>
<tr><th>Mutator Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-32.4%</th>
<th>-38.8%</th></tr>
<tr><th>+1 s.d.</th>
<th>+20.1%</th>
<th>+3.0%</th></tr>
<tr><th>Average</th>
<th>-9.9%</th>
<th>-20.6%</th></tr>
<tr><th>Mutator Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-28.7%</th>
<th>-37.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+22.5%</th>
<th>+4.4%</th></tr>
<tr><th>Average</th>
<th>-6.6%</th>
<th>-19.5%</th></tr>
<tr><th>GC Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+4.5%</th>
<th>-11.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+74.8%</th>
<th>+54.1%</th></tr>
<tr><th>Average</th>
<th>+35.2%</th>
<th>+16.5%</th></tr>
<tr><th>GC Elapsed Time</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+7.9%</th>
<th>-8.0%</th></tr>
<tr><th>+1 s.d.</th>
<th>+75.1%</th>
<th>+56.7%</th></tr>
<tr><th>Average</th>
<th>+37.4%</th>
<th>+20.0%</th></tr>
<tr><th>Total Memory in use</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>-1.7%</th>
<th>-1.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+88.9%</th>
<th>+88.9%</th></tr>
<tr><th>Average</th>
<th>+36.3%</th>
<th>+36.1%</th></tr>
<tr><th>Compile Times</th>
<th></th>
<th></th></tr>
<tr><th>-1 s.d.</th>
<th>+11.9%</th>
<th>-8.9%</th></tr>
<tr><th>+1 s.d.</th>
<th>+21.1%</th>
<th>+2.9%</th></tr>
<tr><th>Average</th>
<th>+16.4%</th>
<th>-3.1%</th></tr></table>