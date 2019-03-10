# GHC Commentary: The Word


The most important type in the runtime is `StgWord`, defined in [includes/StgTypes.h](/trac/ghc/browser/ghc/includes/StgTypes.h).  A word is defined to be the same size as a pointer on the current platform.  All these types are interconvertible without losing information, and have the same size (as reported by `sizeof`):

<table><tr><th>`StgWord`</th>
<td>
An unsiged integral type of word size
</td></tr></table>

<table><tr><th>`StgInt`</th>
<td>
A signed integral type of word size
</td></tr></table>

<table><tr><th>`StgPtr`</th>
<td>
Pointer to `StgWord`</td></tr></table>


The word is the basic unit of allocation in GHC: the heap and stack are both allocated in units of a word.  Throughout the runtime we often use sizes that are in units of words, so as to abstract away from the real word size of the underlying architecture.


C-- only understands units of bytes, so we have various macros in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) to make manipulating things in units of words easier in `.cmm` files.
