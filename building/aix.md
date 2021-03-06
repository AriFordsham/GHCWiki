# Building GHC on AIX


Starting with GHC 8.0 it is possible again to build GHC on IBM AIX


This page documents which hoops to jump through to make this happen and what the current limitations are.

## Prerequisites

- GNU Make
- GCC (known to work: GCC 4.8.3, other versions may work as well though)
- GMP (unless `integer-simple` is used)
- libiconv-1.14 (compiled with `-D_THREAD_SAFE`)
- binutils (for `objdump` and `gas`)
- ...TODO...


The [AIX Open Source Packages](http://www.perzl.org/aix/) repository is a convenient source to get the prerequisites.


A consequence of this is that building a GHC crosscompiler targetting AIX is quite difficult.


Sadly (at the time of writing), `binutils`'s `ld` has only partial support for AIX. IBM's `ld` must be used. Also, IBM's `ar` is recommended over `binutils`'s. 



Having said that, IBM's `as` has deficiencies which cause problems when compiling GHC. So `gas` from `binutils` is recommended for building GHC.


## Building



TODO


