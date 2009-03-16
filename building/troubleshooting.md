# Troubleshooting the GHC build


Here we keep track of failures that can occur when building GHC, with solutions.


We don't expect anyone to read this page from beginning to end.  The only way you get here is by searching, so remember when adding a new entry the most important thing to do is to **include the error message verbatim**, so searches will find it.  If a build failure is caused by a bug in GHC or the build system, please link to the ticket number so we can tell when it's safe to remove the entry and keep this page from getting too crufty.

### Using autoconf by mistake


If you used `autoconf` instead of `sh boot`, you'll get an error when you run `./configure`:

```wiki
...lots of stuff...
creating mk/config.h
mk/config.h is unchanged
configuring in ghc
running /bin/sh ./configure  --cache-file=.././config.cache --srcdir=.
./configure: ./configure: No such file or directory
configure: error: ./configure failed for ghc
```

### Cannot create configure

`autoreconf` (which gets run by `sh boot`) seems to create the file `configure` read-only.  So if you need to run `sh boot` again (which I sometimes do for safety's sake), you get

```wiki
/usr/bin/autoconf: cannot create configure: permission denied
```


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

### Argument list too long


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

### Space in TMPDIR


One difficulty that comes up from time to time is running out of space
in `TMPDIR`.  (It is impossible for the configuration stuff to
compensate for the vagaries of different sysadmin approaches to temp
space.)


The quickest way around it is `setenv TMPDIR /usr/tmp` or
even `setenv TMPDIR .` (or the equivalent incantation with your shell
of choice).


The best way around it is to say

```wiki
export TMPDIR=<dir>
```


in your `build.mk` file.  Then GHC and the other
tools will use the appropriate directory in all cases.

### Warning "warning: assignment from incompatible pointer type"


You may occasionally see a warning from the C compiler when compiling some
Haskell code, eg. "warning: assignment from
incompatible pointer type".  These are usually harmless, but it's a good idea to
report it on the mailing list so that we can fix it.

### Warning "ar: filename `GlaIOMonad__1_2s.o` truncated to `GlaIOMonad_`"


Similarly, `ar`chiving warning messages like the following are not a problem:

```wiki
ar: filename GlaIOMonad__1_2s.o truncated to GlaIOMonad_
ar: filename GlaIOMonad__2_2s.o truncated to GlaIOMonad_
...
```

### Cpp variations


GHC's sources go through `cpp` before being compiled, and `cpp` varies
a bit from one Unix to another.  One particular gotcha is macro calls
like this:

```wiki
SLIT("Hello, world")
```


Some `cpp`s treat the comma inside the string as separating two macro
arguments, so you get

```wiki
:731: macro `SLIT' used with too many (2) args
```


Alas, `cpp` doesn't tell you the offending file!
Workaround: don't put weird things in string args to `cpp` macros.

### Cabal/Distribution/Compat/FilePath.hs: No such file or directory


You may see this:

```wiki
Distribution/Compat/FilePath.hs:2: 
  error: Cabal/Distribution/Compat/FilePath.hs: No such file or directory
make[1]: *** [depend] Error 1
make: *** [stage1] Error 1
```

**Possible Solution**::
Be sure you have run `sh darcs-all get` to get all necessary packages. Don't forget to run `sh boot` again after you pull in new packages.

### xargs: /usr/bin/ar: terminated by signal 11


You may see this when compiling libraries:

```wiki
(echo Control/Concurrent_stub.o System/CPUTime_hsc.o System/Time_hsc.o ;
/usr/bin/find Control/Applicative_split Control/Arrow_split
Control/Concurrent_split Control/Concurrent/Chan_split 
   ...long mess...
Text/PrettyPrint/HughesPJ_split Text/Printf_split Text/Read_split
Text/Read/Lex_split Text/Show_split Text/Show/Functions_split -name '*.o'
-print) | xargs /usr/bin/ar q libHSbase.a
/usr/bin/ar: creating libHSbase.a
xargs: /usr/bin/ar: terminated by signal 11
make[2]: *** [libHSbase.a] Error 125
make[2]: *** Deleting file `libHSbase.a'
make[1]: *** [all] Error 1
```


What is happening is that the ghc build system is linking thousands and
thousands of tiny .o files into `libHSbase.a`. GNU `ar` isn't optimised for
this use-case and it takes far more memory than it really needs to. So
what happens is that ar takes \>500Mb of memory and your virtual
machine / virtual server probably isn't configured with that much memory
and so the linux kernel OOM killer terminates the ar process.


To make this worse, since there are so many .o files, it takes several
invocations of ar to link them all. On each invocation `ar` is building
the symbol index (-q is ignored) and this is what takes the most time
and memory. It's a good deal quicker to use a custom program (100 lines
of Haskell) to build `libHSbase.a` and then use `ranlib` just once to build
the symbol index.


\[Duncan Coutts\] I submitted a patch to gnu `binutils` to make ar take less memory when
linking 1000's of files so it now only takes around 100Mb rather than
500Mb when linking `libHSbase.a`. That patch is included in version 2.17 I
think (in other words most systems don't have it yet).


What you can do in the mean time is either configure your virtual
machine with more memory or turn off the split-objs feature when you
configure ghc. Just add `SplitObjs=NO` to your `mk/build.mk` file (which
may not exist to start with). (The Gentoo ebuild does this
automatically)

### Crippled `ld`


It turns out that on both Cygwin and MSYS, the `ld` has a
limit of 32kbytes on its command line.  Especially when using split object
files, the make system can emit calls to `ld` with thousands
of files on it.  Then you may see something like this:

```wiki

(cd Graphics/Rendering/OpenGL/GL/QueryUtils_split && /mingw/bin/ld -r -x -o ../QueryUtils.o *.o)
/bin/sh: /mingw/bin/ld: Invalid argument

```


The solution is either to switch off object file splitting (set
`SplitObjs` to `NO` in your
`build.mk`),
or to make the module smaller.

### `CYGWIN` environment variable in MSYS


When using MSYS, check that the `CYGWIN` environment variable is *not* set.  It's a bad bug
that MSYS is affected by this, but if you have CYGWIN set to "ntsec ntea", which is right for Cygwin, it
causes the MSYS `ssh` to bogusly fail complaining that your `.ssh/identity`
file has too-liberal permissinos. 


ToDo: what's the error message for this?

### Forgetting to install `automake`


If you get a message like this:

```wiki
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
autoreconf: /usr/bin/autoconf failed with exit status: 1
```


then you have probably not got `automake` installed (or at least findable).

### Vista installer detection


Vista has a "feature" called "installer detection" which tries to elevate permissinos for executables named things like `Setup` and `Install`.  There are lots of programs called `Setup` in a GHC build, and if you see permission-denied errors relating to programs called `Setup` you may need to disable installer detection.  Go to `Start -> All Programs -> Accessories > Run` and enter `secpol.msc`.  Then under `Security Settings -> Local Policies -> Security Options`,  disable `UAC: Detect application installations and prompt for elevation`.  Then reboot.


We added a workaround for install-detection in GHC 6.8.1 (see [\#1271](https://gitlab.haskell.org//ghc/ghc/issues/1271)), so if you're using that version or later you shouldn't encounter this issue.
