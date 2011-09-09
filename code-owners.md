# The GHC Team


The success of GHC has a lot to do with the large band of highly
talented people who contribute to it; this page is mainly to
acknowledge those contributions but also to give users of GHC an idea
of "who is responsible for what".  

## Porters/Packagers (no particular order)


The following table lists the people who have generously agreed to be responsible for the GHC port to a particular platform, or to produce binary packages for a certain platform.


For a full description of GHC support for various platforms, see [Platforms](platforms).

- **x86**

  - Windows (targetting [ mingw](http://www.mingw.org)): Sigbjorn Finne
  - Windows (targetting [ Cygwin](http://www.cygwin.com)): Position open; port bitrotted
  - Linux (generic): Simon Marlow,
    Simon Peyton Jones
  - FreeBSD/i386: FreeBSD Haskell Team
  - OpenBSD: Matthias Kilian
  - SuSE Linux/x86 packages: Sven Panne,Ralf Hinze
  - Gentoo Linux/x86 packages: Gentoo Haskell team
  - Debian GNU/Linux/x86 packages: Kari Pahula
  - MacOS X: Manuel Chakravarty
  - Fedora packages: Fedora Haskell SIG

- **PowerPC**

  - AIX: Audrey Tang
  - Linux: Ryan Lortie
  - Gentoo Linux packages (32 and 64bit): Gentoo Haskell team
  - Debian GNU/Linux/powerpc packages: Kari Pahula
  - MacOS X: Wolfgang Thaller,Thorkil Naur
  - Fedora packages: Fedora Haskell SIG (until F12)

- **Sparc**

  - Solaris: Ben Lippmeier
  - Linux:   Position open
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/sparc packages: Kari Pahula

- **x86_64**

  - Linux: Simon Marlow
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/amd64 packages: Kari Pahula
  - FreeBSD/amd64: FreeBSD Haskell Team
  - OpenBSD: Matthias Kilian
  - Fedora packages: Fedora Haskell SIG

- **Mips64**

  - Irix: Don Stewart

- **Mips**

  - Debian GNU/Linux/mips packages: Kari Pahula

- **Mipsel**

  - Debian GNU/Linux/mipsel packages: Kari Pahula

- **IA-64**

  - Linux: Matt Chapman
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/ia64 packages: Kari Pahula

- **Alpha**

  - Dec OSF: Ken Shan
  - Linux: Position open.
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/alpha packages: Kari Pahula

- **HPPA**

  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/hppa packages: Kari Pahula

- **S/390**

  - Debian GNU/Linux/s390 packages: Kari Pahula

- **Arm**

  - Debian GNU/Linux/arm packages: Kari Pahula

## Current committers (alphabetical order)


This is a list of the people that currently have commit access to GHC,
and a short description of what they're doing or have done.

- Manuel Chakravarty (FFI, Data Parallel Haskell, associated types & type families, Mac OS X port)
- [ Tim Chevalier](http://catamorphism.org/) (External Core, strictness analysis, ticky-ticky profiling)
- Duncan Coutts (Gentoo packages)
- [ Iavor S Diatchki](http://www.csee.ogi.edu/~diatchki) ('monadLib' library)
- Andy Gill (Haskell Program Coverage)
- David Himmelstrup (GHC developer)
- Roman Leshchinskiy
- Ben Lippmeier (SPARC, native code generator, register allocation)
- Andres Loeh
- Ian Lynagh (GHC support engineer)
- Simon Marlow (GHC developer, x86_64 port, SMP support)
- John Meacham
- Ross Paterson
- Sven Panne
- Simon Peyton Jones
- [ Norman Ramsey](http://www.eecs.harvard.edu/nr) (a little refactoring in the back end)
- Don Stewart (OpenBSD support)
- Josef Svenningsson
- Audrey Tang (Perl-related tools (ghc-asm, ghc-split), AIX port)
- David Terei (LLVM, Safe Haskell)
- Wolfgang Thaller (Darwin/Mac OS X ports, position-independent code generation)
- David Waern (Haddock comment support)
- Malcolm Wallace
- Ashley Yakeley (Time library)

## Past contributors


These people have contributed to GHC in the past, in various
wonderful ways:

- Krasimir Angelov
- Lennart Augustsson
- Jean-Philippe Bernardy (Maintenance of Data.\*Map/\*Set libraries)
- [ Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold) (Parallel Haskell: Eden)
- Bjorn Bringert
- Sebastien Carlier
- Andrew Cheadle
- Sigbjorn Finne
- Kevin Glynn
- John Goerzen
- Cordy Hall
- Kevin Hammond
- Tim Harris (Transactional memory, concurrency)
- José Iborra (GHCi Debugger)
- Isaac Jones (Cabal, libraries)
- Ralf Laemmel
- Hans Wolfgang Loidl
- John Launchbury
- Ryan Lortie (PowerPC Linux port, packages)
- Jim Mattson
- Darren Moffat
- Nick Nethercote
- Thomas Nordin
- Bryan O'Sullivan
- Sungwoo Park
- Will Partain
- Juan Quintela
- Alastair Reid
- Ben Rudiak-Gould
- Patrick Sansom
- André Santos
- Sean Seefried (Plug-in optimisations (in pluggable-branch))
- Julian Seward
- Dominic Steinitz (Cryptographic library)
- Volker Stolz
- Dinko Tenev
- Mike Thomas
- Reuben Thomas
- Christopher D. Thompson-Walsh
- Dylan Thurston
- Phil Trinder
- Mark Tullsen
- David N Turner
- Philip Wadler
- Michael Weber
- N. Xu
