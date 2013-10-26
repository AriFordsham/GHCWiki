# Building a GHC cross-compiler for Apple iOS targets

## Xcode 5 Notes


If you use ghc-7.6.3 on OS X as your bootstrap compiler and you are using Xcode version 5 or higher, you'll need to pass certain options to clang to work around some problems. A wrapper for this, written in Haskell, can be found at [ https://github.com/ghc-ios/ghc-ios-scripts](https://github.com/ghc-ios/ghc-ios-scripts).


To use it, compile `clang-xcode5-wrapper.hs`, add it to your path, then edit `/usr/local/lib/ghc-7.6.3/settings` and change `"C compiler command"`'s value to `"clang-xcode5-wrapper"`.


(The problem is that 1. GHC tries to run a C pre-processor over Haskell code, and 2. Xcode versions \>= 5 use clang instead of gcc, and 3. clang doesn't like Haskell code.). 


GHC 7.8 already includes a fix for this.

## Steps

### 1. Read ARM-specific notes


You must use LLVM 3.0's versions of opt and llc — there seem to be issues with other versions. It's easiest to just download the binaries and put them in your path:
[ http://llvm.org/releases/3.0/clang+llvm-3.0-x86_64-apple-darwin11.tar.gz](http://llvm.org/releases/3.0/clang+llvm-3.0-x86_64-apple-darwin11.tar.gz)


See [Cross-compiling GHC](building/cross-compiling) for more details.

### 2. Scripts


You will need to check out the scripts at [ https://github.com/ghc-ios/ghc-ios-scripts](https://github.com/ghc-ios/ghc-ios-scripts) and add the checked out directory to your PATH. You may need to edit these scripts if you are using a different iOS / iOS simulator platform version than the one the scripts are pointed at.


If you're using Xcode 5, do "git checkout xcode5" after cloning. Also see the note at the top of this page regarding clang-xcode5-wrapper.

### 3. Check out GHC


Check out as described at [Building and Porting GHC](building), except use the following for your sync-all to omit dph packages, because Template Haskell doesn't work yet, and dph depends on it:

```wiki
./sync-all --no-dph get
perl boot
```

### 4. Create a build.mk file


GHC requires you to write a **mk/build.mk** file — we've integrated the correct configuration into `build.mk.sample`, so just copy/rename that to `build.mk` and uncomment the line:

```wiki
BuildFlavour  = quick-cross
```

### 5. Configure & build


For iOS:

```wiki
./configure --target=arm-apple-darwin10  --with-gcc=arm-apple-darwin10-gcc
make
sudo mkdir -p /usr/local/ghc-ios/
sudo make install
```


For the iOS simulator:

```wiki
./configure --target=i386-apple-darwin11 --with-gcc=i386-apple-darwin11-gcc
make
sudo mkdir -p /usr/local/ghc-ios-sim/
sudo make install
```


For Xcode 5, change `--with-gcc=arm-apple-darwin10-gcc` / `--with-gcc=i386-apple-darwin11-gcc` to `--with-gcc=arm-apple-darwin10-clang` / `--with-gcc=i386-apple-darwin11-clang` , respectively.

### 6. Make sure your Cabal and cabal-install are new enough


You need a recent change,...

```wiki
commit 9f374ab45e62924506b992db9157c970c7259a03
Author: Stephen Blackheath <stephen.blackheath@ipwnstudios.com>
Date:   Thu Aug 29 13:09:18 2013 +1200

    Give the xxx_HOST_OS and xxx_HOST_ARCH options that were probed from ghc's target
    platform, rather than assuming HOST == BUILD. This fixes things for cross compiling.
```


...so the best thing would be to check out the latest from [ https://github.com/haskell/cabal/](https://github.com/haskell/cabal/), and build both Cabal and cabal-install.


The ghc-ios-scripts directory you checked out earlier contains two wrappers called `arm-apple-darwin10-cabal` and `i386-apple-darwin11-cabal`. These will pass the right arguments to cabal, so you can do, for example:

```wiki
arm-apple-darwin10-cabal install text
```


If you get errors like "Could not find module Prelude" when installing cabal packages, you probably have cabal's library profiling option on, which our compilation instructions don't enable for GHC's libraries.
You can either disable it by setting

```wiki
library-profiling: False
```


in your `~/.cabal/config` file, or by passing the `--disable-library-profiling` like

```wiki
i386-apple-darwin11-cabal install text --disable-library-profiling
```

### 7. Make sure hsc2hs is new enough


The easiest way to do this is as follows, in the ghc build directory:

```wiki
cd utils/hsc2hs
cabal install
```

### 8. Create an Xcode project


Create a new skeleton Xcode project using the wizard, and make sure it runs on your device.

### 9. Compile your Haskell code


Here's a skeleton **haskell.hs** to get you started:

```wiki
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C

foreign import ccall safe "c_main" c_main :: IO ()

main = do
    putStrLn "Haskell start"
    c_main
```


The main() function in **main.m** must be changed to something like this, because Haskell's main now runs first.

```wiki
int c_main(void)
{
    int argc = 1;
    char* argv[2];
    argv[0] = "dummy";
    argv[1] = NULL;		
    printf("Welcome!\n");
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```


Now compile it:

```wiki
arm-apple-darwin10-ghc -staticlib haskell.hs -threaded
```


Ignore the copious warnings about "truncation and blank padding" and "has no symbols" (until someone fixes them).


This will create (in this example) **haskell.a**.


(n.b. we're only modifying main for the sake of example — you can also initialize Haskell when the Objective-C runtime loads, as shown in [ https://gist.github.com/lukexi/20b3aae6ac0f0f7892be](https://gist.github.com/lukexi/20b3aae6ac0f0f7892be) and then call Haskell at will.)

### 10. Set up your Xcode project for Haskell


Now configure it as follows:

- Click on the top node in the project tree, then go to the **Build Settings** tab. Set **Dead Code Stripping** to **No**. This is needed because GHC generates "tables next to code", and without this setting, Xcode thinks the tables are dead code and strips them, causing a crash.

- Click on the top node in the project tree, then go to the **Build Phases** tab. Click on **Link Binary With Libraries** to open it then click +. Choose `libiconv.dylib` then click Add.

- When you've compiled your Haskell code to a .a (e.g. **haskell.a**) file, add it to the project anywhere in the hierarchy with **Add files to (project)** in the right-mouse button menu.

### 11. Build and run


Run the project again as usual, and Xcode will pick up the haskell.a file and your Haskell code should now run on your iOS device. Anything printed with putStrLn will appear in the Xcode runtime console.


Each time you modify your Haskell code you'll need to re-compile from the command line before re-building in Xcode. It is possible to automate this in Xcode if you wish.

### 12. Next steps


Take a look at [ https://github.com/ghc-ios/](https://github.com/ghc-ios/) for patched versions of Hackage packages and other useful things. Ask us if you want to join the ghc-ios project on GitHub, and feel free to raise bugs there.


The Haskell-iPhone mailing list is at [ http://www.haskell.org/mailman/listinfo/iphone](http://www.haskell.org/mailman/listinfo/iphone)

## Loose ends


Outstanding issues we should fix in rough priority order.

- Fat binaries (done by lukexi)
- Cross-compiler for the iOS simulator (done by lukexi)
- Template Haskell for cross compilers! Could be done by (in order of increasing complexity):

  - Evil Splicer / zeroth method
  - A less powerful template-haskell extension
  - stage 2 in cross compilers
- Packaging with the wrapper scripts and perhaps release of binaries of official ghc releases
- Would be nice to not have to disable dead-code removal. (Simon Marlow says "we have special hacks so that you don't have to disable dead-code removal on OS X, in the native code generator and (I presume) in the LLVM backend. Perhaps this just needs to be adapted to work on iOS too?")
- Fix the copious link warnings
- Stop llvm generating an unnecessary 'bx lr' (return) instruction after the GHC calling convention (which is actually a goto)
- Programs outputting substantial text to the console can cause hangs in the RTS. (fixed by lukexi in ticket [\#8307](https://gitlab.haskell.org//ghc/ghc/issues/8307)).

## Wish List

- ARM64 support (need support in LLVM, llvmCodeGen, Platform.hs, and calling conventions)
- SIMD vectorization port of the SSE/AVX SIMD infrastructure
