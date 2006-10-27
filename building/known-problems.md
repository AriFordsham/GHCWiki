# Known pitfalls in building Glasgow Haskell


WARNINGS about pitfalls and known "problems":

1. One difficulty that comes up from time to time is running out of space
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
1. In compiling some support-code bits, e.g., in `ghc/rts/gmp` and even
  in `ghc/lib`, you may get a few C-compiler warnings.  We think these
  are OK.
1.  When compiling via C, you'll sometimes get "warning: assignment from
  incompatible pointer type" out of GCC.  Harmless.
1. Similarly, `ar`chiving warning messages like the following are not
  a problem:

  ```wiki
  ar: filename GlaIOMonad__1_2s.o truncated to GlaIOMonad_
  ar: filename GlaIOMonad__2_2s.o truncated to GlaIOMonad_
  ...
  ```
1. In compiling the compiler proper (in `compiler/`), you *may*
  get an "Out of heap space" error message.  These can vary with the
  vagaries of different systems, it seems.  The solution is simple:

  - If you're compiling with GHC 4.00 or later, then the
    *maximum* heap size must have been reached.  This
    is somewhat unlikely, since the maximum is set to 64M by default.
    Anyway, you can raise it with the
    `-optCrts-M<size>` flag (add this flag to
    `<module>_HC_OPTS``make` variable in the appropriate
    `Makefile`).
  - For GHC \> 4.00, add a suitable `-H` flag to the `Makefile`, as
    above.
  - and try again: `make`.  (see \<xref linkend="sec-suffix"/\> for information about
    `<module>_HC_OPTS`.)

    Alternatively, just cut to the chase:

    ```wiki
    $ cd ghc/compiler
    $ make EXTRA_HC_OPTS=-optCrts-M128M
    ```
1. If you try to compile some Haskell, and you get errors from GCC about
  lots of things from `/usr/include/math.h`, then your GCC was
  mis-installed.  `fixincludes` wasn't run when it should've been.

  As `fixincludes` is now automagically run as part of GCC installation,
  this bug also suggests that you have an old GCC.
1. You *may* need to re-`ranlib` your libraries (on Sun4s).

  ```wiki
  $ cd $(libdir)/ghc-x.xx/sparc-sun-sunos4
  $ foreach i ( `find . -name '*.a' -print` ) # or other-shell equiv...
  ?    ranlib $i
  ?    # or, on some machines: ar s $i
  ? end
  ```

  We'd be interested to know if this is still necessary.
1. GHC's sources go through `cpp` before being compiled, and `cpp` varies
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
