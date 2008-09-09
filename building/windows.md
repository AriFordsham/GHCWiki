# Instructions for building under Windows


This section gives detailed instructions for how to build 
GHC from source on your Windows machine. Similar instructions for
installing and running GHC may be found in the user guide. In general,
Win95/Win98 behave the same, and WinNT/Win2k behave the same.


Make sure you read the preceding section on [platforms](building/platforms-scripts-file-names)
before reading section.
You don't need Cygwin or MSYS to *use* GHC, 
but you do need one or the other to *build* GHC.

## Summary

1. Install *either*MSYS? (including the MSYS Development ToolKit) *or*Cygwin?.  This is the interactive shell plus development tools (make etc) in which you're going to build GHC.

1. Configure SSH?.

1. Get other tools you need for development?:

  - Darcs
  - GHC itself (used for bootstrapping)
  - Happy
  - Alex
  - Python (already in Cygwin)
  - An editor 

1. Get MinGW?. This is used solely for the C compiler and linker that are bundled into the GHC distribution.  

1. Get the [GHC sources](building/getting-the-sources)

1. [Do the build](building/windows#building-ghc)

## Vista users


If you're on Vista, first of all you need to disable "installer-detection", which causes strange things to happen for binaries called "setup.exe", amongst other things.  Go to `Start -> All Programs -> Accessories > Run` and enter `secpol.msc`.  Then under `Security Settings -> Local Policies -> Security Options`,  disable `UAC: Detect application installations and prompt for elevation`.  Then reboot.


When building against MingW, make sure that the paths of MingW's gcc.exe and cc1.exe are in your PATH environment variable.
Best put them at the front. Otherwise Cygwin's executables might be found rather than MingW's. If you do not
set the path's correctly, you may get the following error in config.log:

```wiki
  configure:3321: checking for C compiler default output file name
  configure:3348: c:/MinGW/bin/gcc    conftest.c  >&5
  ld: /mingw/lib/crt2.o: No such file: No such file or directory
  configure:3351: $? = 1
  configure:3389: result:
  configure: failed program was:
  configure:3396: error: C compiler cannot create executables
  See `config.log' for more details.
```


From within a Cygwin terminal, you can set PATH like:

```wiki
  export PATH=/cygdrive/c/MingW/bin/:/cygdrive/c/MingW/libexec/gcc/mingw32/3.4.2/:$PATH
```


If you are unsure whether you have set PATH correctly, try to compile a simple C program
with MingW's gcc first.

## Building GHC


OK!  
Now go read the documentation above on building from source ([Quick start: just building and installing GHC](building/quick-start)); 
the bullets below only tell
you about Windows-specific wrinkles. Also look in the section that immediately follows
this one for typical failure cases and what do to about them.

- After `sh boot` run `./configure` in
  `$(TOP)/` thus:

  ```wiki
  $ ./configure --host=i386-unknown-mingw32
           --with-gcc=c:/mingw/bin/gcc
           --with-ld=c:/mingw/bin/ld.exe
  ```

  This is the point at which you specify that you are building GHC-mingw
  (see [MinGW](building/platforms-scripts-file-names#mingw)). 

  Both these options are important! It's possible to get into
  trouble using the wrong C compiler!

  Furthermore, it's *very important* that you specify a 
  full MinGW path for `gcc`, not a Cygwin path, because GHC (which
  uses this path to invoke `gcc`) is a MinGW program and won't
  understand a Cygwin path.  For example, if you 
  say `--with-gcc=/mingw/bin/gcc`, it'll be interpreted as
  `/cygdrive/c/mingw/bin/gcc`, and GHC will fail the first
  time it tries to invoke it.   Worse, the failure comes with
  no error message whatsoever.  GHC simply fails silently when first invoked, 
  typically leaving you with this:

  ```wiki
  make[4]: Leaving directory `/cygdrive/e/ghc-stage1/ghc/rts/gmp'
  ../../ghc/compiler/ghc-inplace -optc-mno-cygwin -optc-O 
  -optc-Wall -optc-W -optc-Wstrict-prototypes -optc-Wmissing-prototypes 
  -optc-Wmissing-declarations -optc-Winline -optc-Waggregate-return 
  -optc-Wbad-function-cast -optc-Wcast-align -optc-I../includes 
  -optc-I. -optc-Iparallel -optc-DCOMPILING_RTS 
  -optc-fomit-frame-pointer -O2 -static 
  -package-name rts -O -dcore-lint -c Adjustor.c -o Adjustor.o
  make[2]: *** [Adjustor.o] Error 1
  make[1]: *** [all] Error 1
  make[1]: Leaving directory `/cygdrive/e/ghc-stage1/ghc'
  make: *** [all] Error 1
  ```

  Be warned!

  If you want to build GHC-cygwin ([MinGW](building/platforms-scripts-file-names#mingw))
  you'll have to do something more like:

  ```wiki
  $ ./configure --with-gcc=...the Cygwin gcc...
                --with-ld=...the Cygwin ld.exe...
  ```
- If you are paranoid, delete `config.cache` if it exists.
  This file occasionally remembers out-of-date configuration information, which 
  can be really confusing.
- You almost certainly want to set

  ```wiki
  SplitObjs = NO
  ```

  in your `build.mk` configuration file (see [Getting the build you want](building/using#getting-the-build-you-want)).
  This tells the build system not to split each library into a myriad of little object files, one
  for each function.  Doing so reduces binary sizes for statically-linked binaries, but on Windows
  it dramatically increases the time taken to build the libraries in the first place.
- Do not attempt to build the documentation.
  It needs all kinds of wierd Jade stuff that we haven't worked out for
  Win32.

## What to look for if your build fails


This section collects typical failure cases, and what to do about them.

### Using autoconf by mistake


If you used `autoconf` instead of `sh boot`,

>
> you'll get an error when you run `./configure`:
>
> ```wiki
> ...lots of stuff...
> creating mk/config.h
> mk/config.h is unchanged
> configuring in ghc
> running /bin/sh ./configure  --cache-file=.././config.cache --srcdir=.
> ./configure: ./configure: No such file or directory
> configure: error: ./configure failed for ghc
> ```

### Cannot create configure

`autoreconf` (which gets run by `sh boot`) seems to create the file `configure`

>
> read-only.  So if you need to run `sh boot` again (which I sometimes do for safety's sake),
> you get
>
> ```wiki
> /usr/bin/autoconf: cannot create configure: permission denied
> ```


Solution: delete `configure` first.

### Configure can't find darcs version


When you run your configure script, it falls over with 

```wiki
sh-2.04$ ./configure --with-gcc=c:/mingw/bin/gcc --with-ld=c:/mingw/bin/ld.exe --host=i386-unknown-mingw32
configure: WARNING: If you wanted to set the --build type, don't use --host.
    If a cross compiler is detected then cross compile mode will be used.
checking for GHC version date... -nThe system cannot find the file specified.
configure: error: failed to detect version date: check that darcs is in your path
```


This error is nothing to do with `darcs`!  The darcs-version test in `configure` uses `sort`, and it is picking up the Windows sort (in `c:\windows\system32`) instead of the MSYS or Cygwin sort.  


Solution: either hack the configure script by hand, or (better) make sure that MSYS/Cygwin are in your PATH before Windows. Since `c:\windows\system32` is, by default, in the System Environment Variable called PATH, and System Variables come first when searching for paths, you'll have to put MSYS/Cygwin bin directory in the System PATH, before `c:\windows\system32`.


(Incidentally, `find` is another program that Windows has too, with different functionality to Unix.)

### Aregument list too long


You may find this towards the end of compiling the base library:

```wiki
c:\ghc\ghc-6.6.1\bin\ar.exe: creating libHSbase.a
xargs: c:/ghc/ghc-6.6.1/bin/ar: Argument list too long
make[2]: *** [libHSbase.a] Error 126
make[2]: *** Deleting file `libHSbase.a'
Failed making all in base: 1
make[1]: *** [all] Error 1
make[1]: Leaving directory `/cygdrive/c/GHC6.6.1/ghc-6.6.1/libraries'
make: *** [stage1] Error 2
```


Sadly the argument list has a limited length in Windows.  This may be fixable
somehow (Windows expertise welcomed here), but what we do is to set

```wiki
SplitObjs = NO
```


in `build.mk`.  That stops the splitting-up of object files, and dramatically reduces
the number of object files involved.  Link times are also improved.  (Binary size increases
though.)


Also, you can arrange for the (huge) list of files to be processed iteratively, rather all at once, and that would probably be a principal solution. `xargs` feeds the file names to the appropriate command (e.g. `ar`). In `$(GHC_TOP)/mk/target.mk` find the place where it is called and add this switch

```wiki
xargs -n NNN
```


where NNN is the number of arguments processed at a time. It should be small enough to be less than the limit and large enough for the whole thing not to be too slow.


Note, that it's not good to edit `target.mk` in general.

## A Windows build log using Cygwin


Here is a complete, from-scratch, log of all you need to build GHC using
Cygwin, kindly provided by Claus Reinke.  It does not discuss alternative
choices, but it gives a single path that works. Please help us to keep this
up to date: if you are using newer versions, let us know whether you succeed
or run into issues while following this log.


Note: starting with the [ August 2008 version of 'setup.exe'](http://cygwin.com/ml/cygwin-announce/2008-08/msg00001.html), adding '[http://www.haskell.org/ghc/cygwin](http://www.haskell.org/ghc/cygwin)' will not work unless you disable verification (not recommended) - until that site has a signature, you can add the dependencies from [Devel-\>ghc-depends](http://www.haskell.org/ghc/cygwin/setup.ini) manually.

```wiki
- Install some editor (vim, emacs, whatever)

- Install cygwin (http://www.cygwin.com)
    ; i used 1.5.16-1, installed in c:\cygwin
  - run 'setup.exe'
    Choose a Download Source:
	select 'download from internet';
    Select Root Install Directory:
	root dir: c:\cygwin; 
	install for: all users;
	default file type: unix
    Select Local Package Directory
	choose a spare temporary home
    Select Your Internet Connection
	Use IE5 settings
    Choose a Download Site
	Choose your preferred main mirror and
        Add 'http://www.haskell.org/ghc/cygwin'
    Select Packages
	In addition to 'Base' (default install), 
	select 'Devel->ghc-depends'

- Install mingw (http://www.mingw.org/)
    ; i used MinGW-3.1.0-1.exe
    ; installed in c:\mingw
  - you probably want to add GLUT 
    ; (http://www.xmission.com/~nate/glut.html)
    ; i used glut-3.7.3-mingw32.tar

- Get recent binary snapshot of ghc-6.4.1 for mingw 
    ; (http://www.haskell.org/ghc/dist/stable/dist/)
  - unpack in c:/ghc
  - add C:\ghc\ghc-6.4.1\bin to %PATH%
    (Start->Control Panel->System->Advanced->Environment Variables)

- Get and install binary release of darcs
    ; (http://zooko.com/darcs/darcsdir-cygwin-1.0.7.tar.bz2)

- In the following, shell commands are entered in cygwin bash

- Get darcs version of ghc
    ; also, subscribe to cvs-ghc@haskell.org, and possibly
    ; to cvs-libraries@haskell.org, or follow the mailing list
    ; archives, in case you checkout a version with problems
    ; http://www.haskell.org/mailman/listinfo/cvs-ghc/
    ; http://www.haskell.org/mailman/listinfo/cvs-libraries/

  - mkdir c:/ghc-build; cd c:/ghc-build
    ; (or whereever you want your darcs tree to be)
  - darcs get --partial http://darcs.haskell.org/ghc
  - cd ghc
  - chmod +x darcs-all
  - ./darcs-all get

- Build ghc, using cygwin and mingw, targetting mingw
  - export PATH=/cygdrive/c/ghc/ghc-6.4.1:$PATH
    ; for haddock, alex, happy (*)
  - export PATH=/cygdrive/c/mingw/bin:$PATH
    ; without, we pick up some cygwin tools at best!
  - cd c:/ghc-build/ghc
    ; (if you aren't there already)
  - sh boot
  - ./configure --host=i386-unknown-mingw32 -with-gcc=C:/Mingw/bin/gcc.exe --with-ld=C:/Mingw/bin/ld.exe
    ; we use cygwin, but build for windows
  - cp mk/build.mk.sample mk/build.mk
  - in mk/build.mk:
    add line:       SplitObjs = NO
	(MSYS seems slow when there are zillions of object files)
    uncomment line: BuildFlavour = perf
	(or BuildFlavour = devel, if you are doing development)
    add line:       BIN_DIST=1
  - make 2>&1 | tee make.log
    ; always useful to have a log around

- Package up binary distribution
  - make binary-dist 2>&amp;1 | tee make-bin-dist.log
    ; always useful to have a log around
  - unpack ghc-<version>-i386-unknown-mingw32.tar.bz2 somewhere in your filesystem...
```

```wiki
Additional notes from Neil Mitchell:

- cygwin installation doesn't quite work with the latest version because the ghc
  depends file doesn't have a .sig file with it

- for mingw installation just select the minimal package (or at least i did...)

- I got the following hiccup, which seemed transient:
----------
nmitche6@wlon1207009001 /cygdrive/c/ghc-build/ghc
$ sh boot
Booting .
/usr/bin/m4:configure.ac:1281: cannot create temporary file for diversion: Permi
ssion denied
autom4te-2.61: /usr/bin/m4 failed with exit status: 1
Booting libraries/base
Booting libraries/directory
Booting libraries/editline
Booting libraries/old-time
Booting libraries/process
Booting libraries/unix

nmitche6@wlon1207009001 /cygdrive/c/ghc-build/ghc
$ ./configure --host=i386-unknown-mingw32 --with-gcc=C:/Mingw/bin/gcc.exe --wit
h-ld=C:/Mingw/bin/ld.exe
configure: WARNING: If you wanted to set the --build type, don't use --host.
    If a cross compiler is detected then cross compile mode will be used.
mk/config.h.in doesn't exist: perhaps you haven't run 'sh boot'?

nmitche6@wlon1207009001 /cygdrive/c/ghc-build/ghc
$ sh boot
Booting .
Booting libraries/base
Booting libraries/directory
Booting libraries/editline
Booting libraries/old-time
Booting libraries/process
Booting libraries/unix
---------

- I required happy, haddock and alex to be installed

- Everything failed a few minutes in to the compile:
  http://www.haskell.org/pipermail/glasgow-haskell-users/2008-September/015429.html
  No solutions are yet known

- http://www.nabble.com/cc1-not-found-td9742088.html - looks interesting, tried adding
  c:\mingw\libexec\gcc\mingw32\3.4.5 to the $PATH
```