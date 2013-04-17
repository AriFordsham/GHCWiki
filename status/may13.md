# GHC Status Report May 2013


\[work in progress\]


As planned, we made another minor release 7.6.2 from the 7.6 branch in January 2013. This included only bug and performance fixes; no new features were added.


We plan to put out a new major release 7.8.1 in November 2013. This will include several significant changes, including:

- polykinded Typeable library \[**Jose Pedro Magalhaes**\]

- major improvements in DPH (vectorisation avoidance, new vectoriser) \[**Ben Lippmeier**\]

- type holes \[**Thijs Alkemade**\]

- **Rebindable list syntax.** A GHC extension called [OverloadedLists](overloaded-lists) was added. When this is turned on, the way GHC desugars explicit lists and lists in arithmetic sequence notation is changed. Instead of directly desugaring to built-in lists, a polymorphic witness function is used, similar to the desugaring of numeric literals. This allows for a more flexible use of list notations, supporting many different list-like types. In addition, the functions used in this desugaring process are completely rebindable.

- major changes to the type inference engine \[**Simon Peyton Jones**\]

- type level natural numbers \[**Iavor S. Diatchki**\]

- overlapping type families \[**Richard Eisenberg**\]

- **The new code generator.** \[entry copied from Oct 2012 status report\] Several years since this project was started, the new code generator is finally working  \[1\], and is now switched on by default in `master`.  It will be in GHC 7.8.1.  From a user's perspective there should be very little difference, though some programs will be faster.

>
> There are three important improvements in the generated code.  One is that `let-no-escape` functions are now compiled much more efficiently: a recursive `let-no-escape` now turns into a real loop in C--.  The second improvement is that global registers (R1, R2, etc.) are now available for the register allocator to use within a function, provided they aren't in use for argument passing.  This means that there are more registers available for complex code sequences.  The third improvement is that we have a new sinking pass that replaces the old "mini-inliner" from the native code generator, and is capable of optimisations that the old pass couldn't do.

>
> Hand-written C-- code can now be written in a higher-level style with real function calls, and most of the hand-written C-- code in the RTS has been converted into the new style.  High-level C-- does not mention global registers such as R1 explicitly, nor does it manipulate the stack; all this is handled by the C-- code generator in GHC.  This is more robust and simpler, and means that we no longer need a special calling-convention for primops - they now use the same calling convention as ordinary Haskell functions.

>
> We're interested in hearing about both performance improvements and regressions due to the new code generator.

- support for vector (SSE/AVX) instructions \[**Geoffrey Mainland**\]

- The new parallel I/O manager \[**Andreas Voellmy**\]

- **Dynamic ghci.** Ian Lynagh has changed GHCi to use dynamic libraries rather than static libraries. This means that we are now able to use the system linker to load packages, rather than having to implement our own linker. From the user's point of view, that means that a number of long-standing bugs in GHCi will be fixed, and it also reduces the amount of work needed to get a fully functional GHC port to a new platform. Currently, on Windows GHCi still uses static libraries, but we hope to have dynamic libraries working on Windows too by the time we release.

- cross-compilation \[**Stephen Blackheath**\]


There remains more to do than we will ever have time for, so please do come and join in the fun!

\[1\] The new codegen is nearly ready to go live [ http://hackage.haskell.org/trac/ghc/blog/newcg-update](http://hackage.haskell.org/trac/ghc/blog/newcg-update)