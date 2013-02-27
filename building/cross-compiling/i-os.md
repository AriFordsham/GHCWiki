# Building a GHC cross-compiler to target iOS


Status of cross-compilation to iOS is in ticket [\#7724](https://gitlab.haskell.org//ghc/ghc/issues/7724). It also requires a pull request for Cabal, which is at [ https://github.com/haskell/cabal/pull/1214](https://github.com/haskell/cabal/pull/1214)


Currently we do not build fat binaries (we'd like to fix this), which means you need to choose the right architecture for your device.

## Steps

### 1. Read ARM-specific notes


See [Cross-compiling GHC](building/cross-compiling) at the bottom. In particular, you need to install llvm version 3.0 or \>= 3.2.

### 2. Scripts


Place these scripts somewhere in your path:

**arm-apple-darwin10-gcc**

```wiki
#!/bin/sh

TARGET_PLATFORM=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.1.sdk
TARGET_BIN="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin"

TARGET_GCC=$TARGET_BIN/arm-apple-darwin10-llvm-gcc-4.2
TARGET_CFLAGS="-isysroot $TARGET_PLATFORM -march=armv7 -mcpu=cortex-a8 -mfpu=neon"

exec $TARGET_GCC $TARGET_CFLAGS "$@"
```

**arm-apple-darwin10-ld**

```wiki
#!/bin/sh

TARGET_PLATFORM=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.1.sdk
TARGET_BIN="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin"

TARGET_LD=$TARGET_BIN/ld
TARGET_LDFLAGS="-L$TARGET_PLATFORM/usr/lib/"
```

**arm-apple-darwin10-nm**

```wiki
#!/bin/sh

TARGET_BIN="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin"

TARGET_NM=$TARGET_BIN/nm
exec $TARGET_NM "$@"
```

**arm-apple-darwin10-cabal** (not needed during the build, but useful afterwards)

```wiki
#!/bin/sh
exec cabal --with-ghc=arm-apple-darwin10-ghc --with-ghc-pkg=arm-apple-darwin10-ghc-pkg --with-ld=arm-apple-darwin10-ld \
--configure-option=--host=arm-apple-darwin10 --host-arch=arm --host-os=ios \
"$@"
```


Edit these scripts to ensure:

1. The -march option is correct for your device

1. The platform version matches what you are compiling to in Xcode

### 3. Check out GHC


Check out as described at [Building and Porting GHC](building), except use the following for your sync-all to omit dph packages, because Template Haskell doesn't work yet, and dph depends on it:

```wiki
./sync-all --no-dph get
perl boot
```

### 4. Create a build.mk file


GHC requires you to write a **mk/build.mk** file, and the following one works. `integer-simple` must be used, because the default implementation doesn't compile on iOS. `Stage1Only` is needed for cross-compiling.

```wiki
HADDOCK_DOCS       = NO
BUILD_DOCBOOK_HTML = NO
BUILD_DOCBOOK_PS   = NO
BUILD_DOCBOOK_PDF  = NO
SPLIT_OBJS         = NO
INTEGER_LIBRARY    = integer-simple
Stage1Only 	   = YES
```

### 5. Configure & build

```wiki
./configure --target=arm-apple-darwin10 --prefix=/usr/local/ghc-ios/
make
sudo mkdir -p /usr/local/ghc-ios/
sudo make install
```

### 6. Create an Xcode project


Create a new skeleton Xcode project using the wizard, and make sure it runs on your device.

### 7. Compile your Haskell code


Open a terminal and add `/usr/local/ghc-ios/bin` to your `PATH` environment variable.


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
arm-apple-darwin10-ghc haskell.hs -threaded
```


Ignore the copious warnings about "truncation and blank padding" and "has no symbols" (until someone fixes them).


This will create (in this example) **haskell.a**.

### 8. Set up your Xcode project for Haskell


Now configure it as follows:

- Click on the top node in the project tree, then go to the **Build Settings** tab. Set **Dead Code Stripping** to **No**. This is needed because GHC generates "tables next to code", and without this setting, Xcode thinks the tables are dead code and strips them, causing a crash.

- Click on the top node in the project tree, then go to the **Build Phases** tab. Click on **Link Binary With Libraries** to open it then click +. Choose `libiconv.dylib` then click Add.

- When you've compiled your Haskell code to a .a (e.g. **haskell.a**) file, add it to the project anywhere in the hierarchy with **Add files to (project)** in the right-mouse button menu.

### 9. Build and run


Run the project again as usual, and Xcode will pick up the haskell.a file and your Haskell code should now run on your iOS device. Anything printed with putStrLn will appear in the Xcode runtime console.


Each time you modify your Haskell code you'll need to re-compile from the command line before re-building in Xcode. It is possible to automate this in Xcode if you wish.
