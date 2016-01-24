# Building GHC for Android ARM64


I have \*not\* successfully got GHC running on my Arm64 tablet (yet). I've created this page just to document my attempts thus far, since it might help someone in the future. I had no intentions of building for any other target device, so I didn't worry about compatibility with other equipment. My end goal way to make whatever sacrifices were necessary to get a basic frankenGHC running on the tablet, which I would then use to natively compile a proper GHC.

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


GNU libc6 (glibc2) 2.21-r1<sup>s
</sup>


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
NDK=/opt/android-ndk/

# Project directory
PROJDIR=~/arm

# Standalone toolchain location
TOOLCHAIN=$PROJDIR/toolchain

# libiconv sub-project directory
ICONVDIR=$PROJDIR/iconv
```


Used this command to create a standalone toolchain (reminder: I was specifically targeting my Android 6.0 ARM64 tablet):

```wiki
$NDK/build/tools/make-standalone-toolchain.sh \
    --platform=android-21 \
    --install-dir=$TOOLCHAIN \
    --toolchain=aarch64-linux-android-4.9 \
    --system=linux-x86_64
```


The Android NDK doesn't have everything we need, so I needed to compile a version of libiconv as well. I used [ this link](http://danilogiulianelli.blogspot.com/2012/12/how-to-cross-compile-libiconv-for.html) to figure out how to do it.

```wiki
cd $ICONVDIR
tar xvzf /path/to/libiconv-1.14.tar.gz
```


The copies of `config.sub` and `config.guess` included in this version of libiconv didn't recognize my target architecture, so we had to fix that. I had `=sys-devel/gnuconfig-20150727` (Gentoo package) already installed on my system, but feel free to grab the latest versions from wherever you like.

```wiki
cd $ICONVDIR
cp /usr/share/gnuconfig/config.sub libiconv-1.14/build-aux/config.sub
cp /usr/share/gnuconfig/config.guess libiconv-1.14/build-aux/config.guess
cp /usr/share/gnuconfig/config.sub libiconv-1.14/libcharset/lib/localcharset.c
cp /usr/share/gnuconfig/config.guess libiconv-1.14/libcharset/build-aux/config.guess
```


I also had to make the edits described in [localcharset.c.patch](/trac/ghc/attachment/wiki/Arm64/localcharset.c.patch)[](/trac/ghc/raw-attachment/wiki/Arm64/localcharset.c.patch). I can't say whether these changes are appropriate, but they allowed it to compile. Put the patch in `$ICONVDIR`, and then do:

```wiki
cd $ICONVDIR/libiconv-1.14
patch -b -p1 -i ../localcharset.c.patch
```


Now we create some files that tell Android's build system what to do. I don't really understand these files, I just made some guesses and modified them until they worked. Download both files and put them into `$ICONVDIR`, for now. The first file is [Android.mk](/trac/ghc/attachment/wiki/Arm64/Android.mk)[](/trac/ghc/raw-attachment/wiki/Arm64/Android.mk), and the second file is [Application.mk](/trac/ghc/attachment/wiki/Arm64/Application.mk)[](/trac/ghc/raw-attachment/wiki/Arm64/Application.mk).

```wiki
cd $ICONVDIR
mkdir jni
mv ../Android.mk Android.mk
mv ../Application.mk Application.mk
```