# The GHC Team


The success of GHC has a lot to do with the large band of highly
talented people who contribute to it; this page is mainly to
acknowledge those contributions but also to give users of GHC an idea
of "who is responsible for what".  In particular there are several
people who have generously agreed to look after the port of GHC to a
particular platform, or produce packages for a certain platform - the
section below lists these people and the port or packages that they
maintain.

## Porters/Packagers (no particular order)

- **x86**

  - Windows (targetting [ mingw](http://www.mingw.org)): Sigbjorn Finne
  - Windows (targetting [ Cygwin](http://www.cygwin.com)): Position open; port bitrotted
  - Linux (generic): Simon Marlow,
    Simon Peyton Jones
  - FreeBSD: Simon Marlow,Oliver Braun
  - OpenBSD: Don Stewart
  - SuSE Linux/x86 packages: Sven Panne,Ralf Hinze
  - Gentoo Linux/x86 packages: Andres Loeh
  - Debian GNU/Linux/x86 packages: Ian Lynagh

- **PowerPC**

  - AIX: Autrijus Tang
  - Linux: Ryan Lortie
  - MacOS X: Wolfgang Thaller

- **Sparc**

  - Solaris: Position open
  - Linux:   Position open

- **x86_64**

  - Linux: Simon Marlow

- **Mips64**

  - Irix: Don Stewart

- **IA-64**

  - Linux: Matt Chapman

- **Alpha**

  - Dec OSF: Ken Shan
  - Linux: Position open.

## Current committers (alphabetical order)


This is a list of the people that currently have commit access to GHC,
and a short description of what they're doing or have done.

- Krasimir Angelov: ObjectIO library
- Manuel Chakravarty: Array flattening, GHC Commentary
- Koen Claessen: New Read framework
- Robert Ennals: Eager evaluation
- Sigbjorn Finne: Win32 porting, threaded RTS, lots of other stuff
- Gabrielle Keller: Array flattening
- Marcin Kowalczyk: 31-bit Chars, hsc2hs, various other stuff
- Jeff Lewis: Implicit parameters, functional dependencies
- Ryan Lortie: PowerPC/PowerPC-64 Linux porting
- Ian Lynagh: Template Haskell
- Simon Marlow: Release engineering, general hacking
- Sven Panne: OpenGL/GLUT/OpenAL bindings, configuration/build system, various other stuff
- Ross Paterson: Control.Arrow, haskell-src improvements
- Simon Peyton Jones: general hacking
- Don Stewart: OpenBSD (x86 &amp; m68k) porting
- Volker Stolz: forkProcess\#, thread labels
- Wolfgang Thaller: MacOS X porting
- Andrew Tolmach: External Core
- Keith Wansbrough: Usage analysis
- Michael Weber: Debian packaging

## Past contributors


These people have contributed to GHC in the past, in various
wonderful ways:

- Sebastien Carlier
- Kevin Glynn
- Cordy Hall
- Kevin Hammond
- Andy Gill
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
- Andr&eacute; Santos
- Julian Seward
- Reuben Thomas
- Phil Trinder
- David N Turner
- Philip Wadler
- Michael Weber
