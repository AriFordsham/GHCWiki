# Building GHC for Android ARM64


I have \*not\* successfully got GHC running on my Arm64 tablet (yet). I've created this page just to document my attempts thus far, since it might help someone in the future. I had no intentions of building for any other target device, so I didn't worry about compatibility with other equipment. My end goal way to make whatever sacrifices were necessary to get a basic frankenGHC ("stage 2") running on the tablet, which I would then use to natively compile a proper GHC (I call that "stage 3", but this might conflict with the official definition of "stage 3", if there is one).


A lot of this was worked out in conjunction with `rwbarton` in `#ghc` on Freenode. I have absolutely no idea what I'm doing, but he does (or he seems to, from my ignorant eyes). He was a huge help in getting as far as I/we did.

## Target Device


Google Pixel C (nVidia Tegra X1, octa-core: four ARM Cortex-A53 and four ARM Cortex-A57, with 3GB ram)


Android 6.0.1 Build MXB48J


Termux version 0.27 (from Google Play)

## Build machine


Core2 Duo, with 4GB ram


Gentoo x86-64


Android NDK version 10e


GHC 7.10.3


gcc 4.8.5 (for building x86-64 binaries)


GNU binutils 2.25.1


GNU Make 4.1


GNU libc6 (glibc2) 2.21


Perl 5.20.2


GNU Automake 1.15


GNU Autoconf 2.69


llvm 3.7.1


Source tarball of the GHC 8 release candidate. (ghc-8.0.0.20160111)


Source tarball for libiconv version 1.14

## Preparing Toolchain


Environment variables (for me, on Gentoo, yours will probably be different):

```wiki
# Location of Android NDK installation
export NDK=/opt/android-ndk

# Project directory
export PROJDIR=~/arm

# Standalone toolchain location
export TOOLCHAIN=$PROJDIR/toolchain

# Standalone toolchain system root
export SYSROOT=$TOOLCHAIN/sysroot

# libiconv sub-project directory
export ICONVDIR=$PROJDIR/iconv

# Update path to include the NDK
export PATH=$NDK:$PATH
```


Used this command to create a standalone toolchain (reminder: I was specifically targeting my Android 6.0 ARM64 tablet):

```wiki
$NDK/build/tools/make-standalone-toolchain.sh \
    --platform=android-21 \
    --install-dir=$TOOLCHAIN \
    --toolchain=aarch64-linux-android-clang3.6 \
    --system=linux-x86_64
```


The Android NDK doesn't have everything we need, so I needed to compile a version of libiconv as well. I used [this link](http://danilogiulianelli.blogspot.com/2012/12/how-to-cross-compile-libiconv-for.html) to figure out how to do it.

```wiki
cd $ICONVDIR
tar xvzf /path/to/libiconv-1.14.tar.gz
```


The copies of `config.sub` and `config.guess` included in this version of libiconv didn't recognize my target architecture, so we had to fix that. I had `=sys-devel/gnuconfig-20150727` (Gentoo package) already installed on my system, but feel free to grab the latest versions from wherever you like.

```wiki
cp /usr/share/gnuconfig/config.sub $ICONVDIR/libiconv-1.14/build-aux/config.sub
cp /usr/share/gnuconfig/config.guess $ICONVDIR/libiconv-1.14/build-aux/config.guess
cp /usr/share/gnuconfig/config.sub $ICONVDIR/libiconv-1.14/libcharset/lib/localcharset.c
cp /usr/share/gnuconfig/config.guess $ICONVDIR/libiconv-1.14/libcharset/build-aux/config.guess
```


I also had to make the edits described in [localcharset.c.patch](/trac/ghc/attachment/wiki/Arm64/localcharset.c.patch). I can't say whether these changes are appropriate, but they allowed it to compile. Put the patch in `$ICONVDIR`, and then do:

```wiki
cd $ICONVDIR/libiconv-1.14
patch -b -p1 -i ../localcharset.c.patch
```


Now we create some files that tell Android's build system what to do. I don't really understand these files, I just made some guesses and modified them until they worked. Download both files and put them into `$ICONVDIR`, for now. The first file is [Android.mk](/trac/ghc/attachment/wiki/Arm64/Android.mk), and the second file is [Application.mk](/trac/ghc/attachment/wiki/Arm64/Application.mk).

```wiki
mkdir $ICONVDIR/jni
mv $ICONVDIR/Android.mk $ICONVDIR/jni/Android.mk
mv $ICONVDIR/Application.mk $ICONVDIR/jni/Application.mk
```


Finally it's ready to build!

```wiki
cd $ICONVDIR/jni
ndk-build
```


Hopefully that worked. I doubt this is the right thing to do, but rather than involving an extra directory in the rest of the build process, I just copied the resulting file over into the sysroot provided by the standalone toolchain:

```wiki
cp $ICONVDIR/libs/arm64-v8a/libiconv.so $SYSROOT/usr/lib/libiconv.so
```


Now we've got everything we need to build GHC! (or not, because this doesn't actually work)

## Building GHC


Start by preparing the environment variables:

```wiki
# Project directory
export PROJDIR=~/arm

# Standalone toolchain location
export TOOLCHAIN=$PROJDIR/toolchain

# Standalone toolchain system root
export SYSROOT=$TOOLCHAIN/sysroot

# I had to set these to get the gcc cross-compiling toolchain to find the right libraries.
export CFLAGS=--sysroot=$SYSROOT
export CPPFLAGS=--sysroot=$SYSROOT
```


First extract the GHC source tarball:

```wiki
cd $PROJDIR
tar xvjf ghc-8.0.0.20160111-src.tar.bz2
```


Now we have to patch a bunch of files in the GHC source. Download [ghc_android.patch](/trac/ghc/attachment/wiki/Arm64/ghc_android.patch) and put it in `$PROJDIR`.

- The first patch is for `compiler/llvmGen/LlvmCodeGen/Ppr.hs`. It doesn't recognize our target triple, so we have to fix that. At different points in time, I saw it mention `aarch64-unknown-linux-android` and `aarch64-none-linux-android`, so without knowing which is correct, I added both. As suggested in the sourcecode of `Ppr.hs`, I used `clang -target aarch64-none-linux-android hello.c -o hello.ll -emit-llvm -S` to figure out what the datalayout should be. I didn't install my own clang, just used version 3.6 from the Android ndk. My `hello.c` was just `main() { return(4); }`.

- Next is `compiler/main/DriverPipeline.hs`. Android's libc has pthread built right in, so `-lpthread` is unnecessary. This just adds `OSAndroid` to the list of platforms for which `-lpthread` should not be used.

- Next is `compiler/main/DynFlags.hs`. Recent versions of android require Position Independent Executables (PIE). This patch is part of my attempts to enable PIC and PIE. The first chunk should be obvious, and in the second chunk I'm copying the behavior of Linux on ARM (which I guess also needs PIC?).

- Next is `ghc.mk`. Terminfo depends on ncurses, which I didn't feel like cross-compiling. Shouldn't be necessary for anything but GHCI, which I don't need right now. (I'd worry about that in stage 3.)

- Next is `libraries/haskeline/haskeline.cabal`. Terminfo is an optional dependency of Haskeline, which defaults to `True`. We're not building Terminfo, so we'll set it to `False`

- Next is `libraries/unix/System/Posix/Terminal/Common.hsc`. This is probably really, really bad. I have no idea what I'm doing here. `_POSIX_VDISABLE` doesn't seem to exist in the android libraries, or not in the way that `Common.hsc` wants to use it, or... honestly, I don't know. After some googling, it looked as though `-1` is a valid definition for `_POSIX_VDISABLE`, so I just swapped that out so it would compile. I have no idea what the consequences for this actually are.

- Next is `mk/config.mk.in`. This was part of me trying to get PIC and PIE enabled. Didn't know what triple to add, so I guessed at two different ones. I'm not entirely sure why this made sense at the time, perhaps `rwbarton` remembers.

- Next is `rts/posix/OSThreads.c`. Well, `cpu_set_t` isn't defined in the Android libraries either. Since I don't know what I'm doing, I commented out almost the entire `if` block, leaving just the last branch because that seemed pretty foolproof. Again, I have no idea what the consequences for this may be.

- Next is `utils/ghc-pkg/ghc-pkg.cabal`. It depended on Terminfo, which I wasn't building, so I removed this dependency. The extent of this dependency is fixed in the next (and final!) patch.

- And the final patch is for `utils/ghc-pkg/Main.hs`. All dependencies on Terminfo are bracketed in `#if` blocks which check for the `BOOTSTRAPPING` define. Quick fix? Define `BOOTSTRAPPING`!


Now that all the patches are explained, apply them with:

```wiki
cd $PROJDIR/ghc-8.0.0.20160111
patch -b -p1 -i ../ghc_android.patch
```


My [build.mk](/trac/ghc/attachment/wiki/Arm64/build.mk) is pretty straightforward. Somewhat of a mix between `quick.mk` and `quick-cross.mk`, with some extra options to turn off dynamic linking (I forget why, but it had something to do with making it use `-pie`... maybe `rwbarton` knows.) and to turn on `-fPIC`. Download it to `$PROJDIR`.

```wiki
mv $PROJDIR/build.mk $PROJDIR/ghc-8.0.0.20160111/mk/build.mk
```


In order to get ld and ld.gold to do what I want (link as much with `-pie` as possible), I created some wrapper scripts to eliminate `pie` from the list of arguments whenever `-r` or `-shared` is found. There is probably a better way to do this, perhaps making use of substitution parameters (`-S`), but this worked for now. Download the two wrapper scripts, [aarch64-linux-android-ld](/trac/ghc/attachment/wiki/Arm64/aarch64-linux-android-ld) and [aarch64-linux-android-ld.gold](/trac/ghc/attachment/wiki/Arm64/aarch64-linux-android-ld.gold), and put them in `$PROJDIR`.


There's also a problem with android-ndk shipping llvm 3.6, but we need llvm 3.7, so we'll replace the `opt` and `llc` binaries with symlinks to our system versions. `clang` might also need the same treatment, but I forgot, and it seemed to work OK. Maybe it's not used?

```wiki
mv $TOOLCHAIN/bin/aarch64-linux-android-ld{,.orig}
mv $PROJDIR/aarch64-linux-android-ld $TOOLCHAIN/bin/aarch64-linux-android-ld
mv $TOOLCHAIN/bin/aarch64-linux-android-ld.gold{,.orig}
mv $PROJDIR/aarch64-linux-android-ld.gold $TOOLCHAIN/bin/aarch64-linux-android-ld.gold
mv $TOOLCHAIN/bin/opt{,.bak}
ln -s /usr/bin/opt $TOOLCHAIN/bin/opt
mv $TOOLCHAIN/bin/llc{,.bak}
ln -s /usr/bin/llc $TOOLCHAIN/bin/llc
```


My configure command got very long because I didn't trust it to find anything. Some of these options may be unnecessary. I've said it a thousand times, and I'll say it again: I don't know what I'm doing.

```wiki
cd $PROJDIR/ghc-8.0.0.20160111
./configure \
    --target=aarch64-linux-android \
        --with-gcc=$TOOLCHAIN/bin/aarch64-linux-android-gcc \
      --with-clang=$TOOLCHAIN/bin/clang \
         --with-ld=$TOOLCHAIN/bin/aarch64-linux-android-ld \
    --with-ld.gold=$TOOLCHAIN/bin/aarch64-linux-android-ld.gold \
         --with-nm=$TOOLCHAIN/bin/aarch64-linux-android-nm \
    --with-objdump=$TOOLCHAIN/bin/aarch64-linux-android-objdump \
         --with-ar=$TOOLCHAIN/bin/aarch64-linux-android-ar \
     --with-ranlib=$TOOLCHAIN/bin/aarch64-linux-android-ranlib \
        --with-llc=$TOOLCHAIN/bin/llc \
        --with-opt=$TOOLCHAIN/bin/opt \
        --prefix=$PROJDIR \
    CONF_CC_OPTS_STAGE1="-fPIC -fPIE -pie" \
    CONF_GCC_LINKER_OPTS_STAGE1="-fPIC -fPIE -pie" \
    CONF_LD_LINKER_OPTS_STAGE1="-fPIC -fPIE -pie" \
    CONF_CC_OPTS_STAGE2="-fPIC -fPIE -pie" \
    CONF_GCC_LINKER_OPTS_STAGE2="-fPIC -fPIE -pie" \
    CONF_LD_LINKER_OPTS_STAGE2="-fPIC -fPIE -pie"
```


Assuming that completes correctly, we start the build. I used `-j5` to run five jobs at once, but you should tweak this to fit your processor's multitasking capabilities. I read somewhere not to exceed eight, though. This builds stage 1 and stage 2.

```wiki
cd $PROJDIR/ghc-8.0.0.20160111
make -j5
```


If that works, then next we'll pack it up to move it to the tablet:

```wiki
cd $PROJDIR/ghc-8.0.0.20160111
make binary-dist
```


That should produce `ghc-8.0.0.20160111-aarch64-unknown-linux-android.tar.bz2` in `$PROJDIR/ghc-8.0.0.20160111`.

## Installing GHC


Start up Termux on the tablet. I installed a huge list of software from their repo, most of which has nothing to do with GHC, but I'll list it below, just in case:

```wiki
apt, bash, bc, binutils, busybox, 
bzip2, ca-certificates, clang, clang-dev, command-not-found,
coreutils, dash, diffutils, dnsutils, dpkg,
file, findutils, flex, fontconfig, freetype,
g++, gawk, gcc, git, glib,
gnupg, gnuplot, grep, gzip, harfbuzz,
ldns, less, libandroid-glob, libandroid-support, libandroid-support-dev,
libbz2, libcairo, libcurl, libffi, libgmp,
libgnustl, liblzma, libmpc, libmpfr, libpixman,
libpng, libuuid, libxml2, make, man,
ncurses, ndk-stl, ndk-sysroot, openssh, openssl,
pango, patch, pcre, perl, readline,
resolv-conf, sed, tar, termux-tools, vim,
vim-runtime, wget, xz-utils
```


Now copy over your tarball, and extract it.

```wiki
cd
mkdir ghc
scp user@buildmachine:arm/ghc-8.0.0.20160111/ghc-*.bz2 ./ghc/
cd ghc
tar xvjf ghc-*.bz2
cd ghc-8.0.0.20160111
```


Now things get a little questionable. Because this is Android, `/bin/sh` doesn't exist. We need to replace all references to it with the one Termux supplies:

```wiki
cd ~/ghc/ghc-8.0.0.20160111
grep -IlR /bin/sh . | xargs -L1 sed -i s?/bin/sh?`which sh`?
```


Almost there, now let's try installing GHC:

```wiki
cd ~/ghc/ghc-8.0.0.20160111
# just FYI, the home directory in Termux is /data/data/com.termux/files/home
./configure --prefix=/data/data/com.termux/files/home/ghc
```


And...! This is where the story ends, for now. I get the following error:

```wiki
checking for path to top of build tree... CANNOT LINK EXECUTABLE: "/data/data/com.termux/files/usr/lib/libandroid-support.so" is 32-bit instead of 64-bit
page record for 0x7fa2c78090 was not found (block_size=64)
configure: error: cannot determine current directory
```


From what I can tell, all of the Termux packages (including shared libraries) are compiled as 32-bit. The next step would be to recompile them as 64-bit, which the Termux author appears to be working on.


Another approach may be to forget PIC and PIE and just try statically linking the GHC binaries. I don't know if that will circumvent the 32-bit vs. 64-bit issue.


If I come back to this project and make any more progress, I'll update this page.
