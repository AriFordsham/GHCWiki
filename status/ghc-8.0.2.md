# GHC plans for 8.0.2


This page is our road-map for what will be in 8.0.2, the first bug-fix release in the 8.0 series.

- We hope to incorporate all the "Landed" and "In-flight" stuff under "Release highlights" below.

- We'll include (or at least review) all patches in tickets in "Status: patch" below.

- We will address all the tickets under "Status: new" below with "highest" or "high" priority.  We love help to do more, but there are far too many "normal" tickets to make any promises.


If you believe your favourite thing belongs in this list, but isn't there, please yell.  If it's not in the road map, it probably won't get done.  Without a lot of support, many things in the road map won't get done either, so we need your help!

## Dates


Release candidate by **mid-November 2016**.

## Libraries Status


See Libraries?

## Release highlights


Below are the major highlights of 8.0.2.

- Interface file build determinism (#4012).

- Compatibility with macOS Sierra and GCC compilers which compile 
  position-independent executables by default

- Runtime linker fixes on Windows (see #12797)

- A compiler bug which resulted in undefined reference errors while
  compiling some packages (see #12076)

- Compatibility with systems which use the gold linker

- A number of memory consistency bugs in the runtime system

- A number of efficiency issues in the threaded runtime which manifest
  on larger core counts and large numbers of bound threads.

- A typechecker bug which caused some programs using
  -XDefaultSignatures to be incorrectly accepted.

- More than two-hundred other bugs. See Trac \[1\] for a complete
  listing.

## Tickets slated for 8.0.2

See the %8.0.2 milestone.

