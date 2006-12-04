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
  - FreeBSD: Simon Marlow,Oliver Braun,Volker Stolz
  - OpenBSD: Don Stewart
  - SuSE Linux/x86 packages: Sven Panne,Ralf Hinze
  - Gentoo Linux/x86 packages: Gentoo Haskell team
  - Debian GNU/Linux/x86 packages: Ian Lynagh
  - MacOS X (port in progress): Wolfgang Thaller
  - Fedora Extras packages: Jens Petersen

- **PowerPC**

  - AIX: Audrey Tang
  - Linux: Ryan Lortie
  - Gentoo Linux packages (32 and 64bit): Gentoo Haskell team
  - Debian GNU/Linux/powerpc packages: Ian Lynagh
  - MacOS X: Wolfgang Thaller,Thorkil Naur
  - Fedora Extras packages: Jens Petersen

- **Sparc**

  - Solaris: Position open
  - Linux:   Position open
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/sparc packages: Ian Lynagh

- **x86_64**

  - Linux: Simon Marlow
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/amd64 packages: Ian Lynagh
  - OpenBSD: Don Stewart
  - Fedora Extras packages: Jens Petersen

- **Mips64**

  - Irix: Don Stewart

- **Mips**

  - Debian GNU/Linux/mips packages: Ian Lynagh

- **Mipsel**

  - Debian GNU/Linux/mipsel packages: Ian Lynagh

- **IA-64**

  - Linux: Matt Chapman
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/ia64 packages: Ian Lynagh

- **Alpha**

  - Dec OSF: Ken Shan
  - Linux: Position open.
  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/alpha packages: Ian Lynagh

- **HPPA**

  - Gentoo Linux packages: Gentoo Haskell team
  - Debian GNU/Linux/hppa packages: Ian Lynagh

- **S/390**

  - Debian GNU/Linux/s390 packages: Ian Lynagh

- **Arm**

  - Debian GNU/Linux/arm packages: Ian Lynagh

- **m68k**

  - Debian GNU/Linux/m68k packages: Ian Lynagh

## Current committers (alphabetical order)


This is a list of the people that currently have commit access to GHC,
and a short description of what they're doing or have done.

- Krasimir Angelov
- Lennart Augustsson
- Jean-Philippe Bernardy (Maintenance of Data.\*Map/\*Set libraries)
- [ Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold) (Parallel Haskell: Eden)
- Bjorn Bringert
- Manuel Chakravarty (FFI, Data Parallel Haskell, associated types)
- Andrew Cheadle
- Duncan Coutts (Gentoo packages)
- [ Iavor S Diatchki](http://www.csee.ogi.edu/~diatchki) ('monadLib' library)
- Sigbjorn Finne
- Andy Gill (Haskell Program Coverage)
- John Goerzen
- Tim Harris (Transactional memory, concurrency)
- David Himmelstrup (GHC developer)
- Isaac Jones (Cabal, libraries)
- Hans W. Loidl
- Ralf Laemmel
- Roman Leshchinskiy
- Andres Loeh
- Ryan Lortie (PowerPC Linux port, packages)
- Ian Lynagh (GHC support engineer, Debian packages)
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
- Wolfgang Thaller (Darwin/Mac OS X ports, position-independent code generation)
- Mike Thomas
- Christopher D. Thompson-Walsh
- Dylan Thurston
- Dinko Tenev
- Mark Tullsen
- Malcolm Wallace
- N. Xu
- Ashley Yakeley (Time library)
- David Waern (Haddock comment support)

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
