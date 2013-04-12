# GHC Status Report May 2013


\[work in progress\]


As planned, we made another minor release 7.6.2 from the 7.6 branch in January 2013. This included only bug and performance fixes; no new features were added.


We plan to put out a new major release 7.8.1 in November 2013. This will include several significant changes, including:

- polykinded Typeable library \[**Jose Pedro Magalhaes**\]

- major improvements in DPH (vectorisation avoidance, new vectoriser) \[**Ben Lippmeier**\]

- type holes \[**Thijs Alkemade**\]

- rebindable list syntax \[**Achim Krause**\]

- major changes to the type inference engine \[**Simon Peyton Jones**\]

- type level natural numbers \[**Iavor S. Diatchki**\]

- overlapping type families \[**Richard Eisenberg**\]

- the new code generator \[**Simon Marlow**\]

- support for vector (SSE/AVX) instructions \[**Geoffrey Mainland**\]

- Scheduler changes to the RTS to improve latency \[**Simon Marlow**\]

- **Dynamic ghci.** Ian Lynagh has changed GHCi to use dynamic libraries rather than static libraries. This means that we are now able to use the system linker to load packages, rather than having to implement our own linker. From the user's point of view, that means that a number of long-standing bugs in GHCi will be fixed, and it also reduces the amount of work needed to get a fully functional GHC port to a new platform. Currently, on Windows GHCi still uses static libraries, but we hope to have dynamic libraries working on Windows too by the time we release.

- cross-compilation \[**Stephen Blackheath**\]


There remains more to do than we will ever have time for, so please do come and join in the fun!
