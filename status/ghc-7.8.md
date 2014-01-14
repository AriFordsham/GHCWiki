# Release plans for GHC 7.8

## Tickets

*Note that anything not listed here is off Austin's radar.*

## Pending RC tickets

- [\#7602](https://gitlab.haskell.org//ghc/ghc/issues/7602) - OS X 10.8 seemed OK with Austin's patch, but OS X 10.9 needs investigation

## RC Checklist


Things tested:

- Builds cleanly
- `validate` runs OK
- The compiler can bootstrap itself and `validate` from a binary distribution

<table><tr><th></th>
<th>Linux (i386)</th>
<th>Linux (x86_64)</th>
<th>OS X 10.7 (x86_64)</th>
<th>OS X 10.8 (x86_64)</th>
<th>OS X 10.9 (x86_64)</th>
<th>Windows i386</th>
<th>Windows x86_64
</th></tr>
<tr><th>Builds clean</th>
<th>**OK**</th>
<th>**OK**</th>
<th>**OK**</th>
<th>Probably **OK**\[2\]</th>
<th>**OK**</th>
<th>**OK**</th>
<th>**OK**</th></tr>
<tr><th>`validate`</th>
<th>In progress\[1\]</th>
<th>**OK**</th>
<th>**OK**</th>
<th>Probably **OK**</th>
<th>**NOT OK**\[3\]\[4\]</th>
<th>**OK**\[1\]</th>
<th>**OK**\[5\]
</th></tr>
<tr><th>bootstrap </th>
<th>In progress</th>
<th>**OK**</th>
<th>**OK**</th>
<th>Probably **OK**</th>
<th>In progress</th>
<th>**OK**</th>
<th>In progress\[5\]</th></tr></table>

- \[1\] The testsuite performance numbers need to be updated for 32 bit platforms.
- \[2\] Austin has not directly tested 10.8 yet, but he speculates it is OK based on the 10.7 and preliminary 10.9 results.
- \[3\] Mavericks was tested using **Clang**, and there are some `make fast` failures (mostly minor driver related things, but not critical.)
- \[4\] Austin is hitting a `validation` error while building `xhtml`, which might be due to some interaction with the new Haddock. Otherwise, the compiler builds fine and the testsuite works.
- \[5\] Some of Herbert's latest patches to `integer-gmp` seem to have broke the 64bit build and need to be re-investigated, but once reverted the build goes smoothly.

## The Dynamic Story


The dynamic story is complex. Here's the breakdown:

<table><tr><th></th>
<th>Linux (i386)</th>
<th>Linux (x86_64)</th>
<th>FreeBSD</th>
<th>OS X 10.7 (x86_64)</th>
<th>OS X 10.8 (x86_64)</th>
<th>OS X 10.9 (x86_64)</th>
<th>Windows i386</th>
<th>Windows x86_64
</th></tr>
<tr><th>Dynamic GHCi  </th>
<th>**YES**</th>
<th>**YES**</th>
<th>**NO**\[1\]</th>
<th>**YES**</th>
<th>Probably **YES**\[2\]</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**</th></tr>
<tr><th>`-dynamic-too`</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>Probably **YES**</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**</th></tr>
<tr><th>`-dynamic`</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>**YES**</th>
<th>Probably **YES**</th>
<th>**YES**</th>
<th>**NO**</th>
<th>**NO**</th></tr></table>

- \[1\] Dynamic GHCi is disabled due to a bug in FreeBSD's rtld, but we're waiting for it to make it into a release.
- \[2\] Austin has not tested 10.8 yet, but he speculates it is OK based on the 10.7 and preliminary 10.9 results.


Where:

- **Dynamic GHCi**: `ghci` uses the system linker and loads dynamic libraries by default, to avoid linker bugs. This is controlled by `DYNAMIC_GHC_PROGRAMS=YES`.
- **`-dynamic-too`**: strictly an optimization, `-dynamic-too` allows the compiler to build static and dynamic object files at once. This is convenient for Dynamic GHCi support.
- **`-dynamic`**: allows dynamic linking and dynamic libraries.

## The Windows Conundrum

- Windows is a bit difficult right now.

  - 64bit builds work using the MSYS2 environment **with some failures**
  - 32bit builds work well using the **old** environment

    - Austin confirmed the latest HEAD worked in the old 32bit environment, but not the msys2 one: the `ghc-stage2.exe` segfaults, and Austin hasn't tracked down why.
  - It seems `-dynamic` is busted, as well as `-dynamic-too`
  - We're punting all three of them for the RC.

    - This leaves GHC in the same place it was before essentially (but 64bit is in a difficult spot, see [\#7134](https://gitlab.haskell.org//ghc/ghc/issues/7134))


(Related but not immediately critical: we have too many DLL symbols, and are very close to the limit ([\#5987](https://gitlab.haskell.org//ghc/ghc/issues/5987)). Linking also takes a long time ([\#8229](https://gitlab.haskell.org//ghc/ghc/issues/8229)))

## Other things

- Austin Seipp needs to upload the primops compatibility package for 7.8. This is is easy: mostly a copy of `compiler/utils/ExtsCompat64.hs` into a Cabal package. See also [ the compatibility module page](http://www.haskell.org/haskellwiki/Compatibility_Modules).
