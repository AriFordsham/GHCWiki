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
  - FreeBSD/i386: Simon Marlow, FreeBSD Haskell Team
  - OpenBSD: Matthias Kilian
  - SuSE Linux/x86 packages: Sven Panne,Ralf Hinze
  - Gentoo Linux/x86 packages: Gentoo Haskell team
  - Debian GNU/Linux/x86 packages: Kari Pahula
  - MacOS X: Manuel Chakravarty
  - Fedora Extras packages: Jens Petersen

- **PowerPC**

  - AIX: Audrey Tang
  - Linux: Ryan Lortie
  - Gentoo Linux packages (32 and 64bit): Gentoo Haskell team
  - Debian GNU/Linux/powerpc packages: Kari Pahula
  - MacOS X: Wolfgang Thaller,Thorkil Naur
  - Fedora Extras packages: Jens Petersen

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
  - Fedora Extras packages: Jens Petersen

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

- Krasimir Angelov
- Lennart Augustsson
- Jean-Philippe Bernardy (Maintenance of Data.\*Map/\*Set libraries)
- [ Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold) (Parallel Haskell: Eden)
- Bjorn Bringert
- Manuel Chakravarty (FFI, Data Parallel Haskell, associated types & type families, Mac OS X port)
- Andrew Cheadle
- [ Tim Chevalier](http://catamorphism.org/) (External Core, strictness analysis, ticky-ticky profiling)
- Duncan Coutts (Gentoo packages)
- [ Iavor S Diatchki](http://www.csee.ogi.edu/~diatchki) ('monadLib' library)
- Sigbjorn Finne
- Andy Gill (Haskell Program Coverage)
- John Goerzen
- Tim Harris (Transactional memory, concurrency)
- David Himmelstrup (GHC developer)
- José Iborra (GHCi Debugger)
- Isaac Jones (Cabal, libraries)
- Hans W. Loidl
- Ralf Laemmel
- Roman Leshchinskiy
- Ben Lippmeier (SPARC, native code generator, register allocation)
- Andres Loeh
- Ryan Lortie (PowerPC Linux port, packages)
- Ian Lynagh (GHC support engineer)
- Simon Marlow (GHC developer, x86_64 port, SMP support)
- John Meacham
- Ross Paterson
- Sven Panne
- Simon Peyton Jones
- [ Norman Ramsey](http://www.eecs.harvard.edu/nr) (a little refactoring in the back end)
- Ben Rudiak-Gould
- Sean Seefried (Plug-in optimisations (in pluggable-branch))
- Dominic Steinitz (Cryptographic library)
- Don Stewart (OpenBSD support)
- Volker Stolz
- Josef Svenningsson
- Audrey Tang (Perl-related tools (ghc-asm, ghc-split), AIX port)
- Wolfgang Thaller (Darwin/Mac OS X ports, position-independent code generation)
- Mike Thomas
- Christopher D. Thompson-Walsh
- Dylan Thurston
- Dinko Tenev
- Mark Tullsen
- David Waern (Haddock comment support)
- Malcolm Wallace
- N. Xu
- Ashley Yakeley (Time library)

## Past contributors


These people have contributed to GHC in the past, in various
wonderful ways:

- Sebastien Carlier
- Kevin Glynn
- Cordy Hall
- Kevin Hammond
- Hans Wolfgang Lloidl
- John Launchbury
- Jim Mattson
- Darren Moffat
- Nick Nethercote
- Thomas Nordin
- Bryan O'Sullivan
- Sungwoo Park
- Will Partain
- Juan Quintela
- Alastair Reid
- Patrick Sansom
- André Santos
- Julian Seward
- Reuben Thomas
- Phil Trinder
- David N Turner
- Philip Wadler
- Michael Weber
