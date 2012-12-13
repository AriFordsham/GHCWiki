# The GHC Team


This page outlines contributors to GHC. The success of GHC has a lot
to do with the large group of highly talented people who contribute to
it.


The page also document who is currently responsible for what in the
GHC code base. The idea being that ideally all parts of GHC should
have someone who claims responsibility for it.

## Current Owners / Maintainers

- **Manuel Chakravarty**

  - DPH vectoriser
  - Type families
  - OS X support

- **Ben Lippmeier**

  - native code generator, register allocation
  - DPH libraries

- **Ian Lynagh**

  - GHC support engineer
  - build system, release process

- **Simon Marlow**

  - **GHC Tsar**
  - Everything, RTS, Cmm CodeGen, x86_64 port, SMP support
  - **Phasing out as changing jobs**

- **Bryan O'Sullivan**

  - Performance Tsar

- **Simon Peyton Jones**

  - **GHC Tsar**
  - Everything, Type system, Simplifier, Cmm CodeGen
  - **Note:** Principle job is research, so more an advisor in the sense that can't spend time on pure engineering efforts.

- **David Terei**

  - Performance Tsar
  - LLVM, Safe Haskell

- **Johan Tibell**

  - Performance Tsar
  - IO Manager

## Directory to Maintainer Mapping

**Compiler (i.e., GHC proper)**

- basicTypes/ -- All
- cmm/        -- None yet
- codeGen/    -- Simon Marlow (phasing out)
- coreSyn/    -- All
- deSugar/    -- Simon Peyton Jones
- ghci/       -- None yet
- hsSyn/      -- Simon Peyton Jones
- iface/      -- All
- llvmGen/    -- David Terei
- main/       -- Ian Lynagh
- nativeGen/  -- Ben Lippmeier
- parser/     -- Simon Peyton Jones
- prelude/    -- None yet
- profiling/  -- None yet
- rename/     -- Simon Peyton Jones
- simplCore/  -- None yet
- simplStg/   -- None yet
- specialise/ -- None yet
- stgSyn/     -- None yet
- stranal/    -- None yet
- typecheck/  -- Simon Peyton Jones
- types/      -- Simon Peyton Jones
- utils/      -- All
- vectorise/  -- Manuel Chakravarty

**Documentation**

- doc/ -- All

**Executables / Drivers**

- driver/
- ghc/

**Run-time System**

- includes/
- rts/

**Testing & Benchmarking**

- nofib/ -- All
- testsuite/ -- All

**Dependencies (Ian Lynagh)**

- libffi/
- libraries/

**Build System (Ian Lynagh)**

- distrib/
- ghc-tarballs/
- mk/
- rules/
- utils/

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
  - Linux: Ryan Lortie, Erik de Castro Lopo
  - Gentoo Linux packages (32 and 64bit): Gentoo Haskell team
  - Debian GNU/Linux/powerpc packages: Kari Pahula
  - MacOS X: Wolfgang Thaller,Thorkil Naur
  - Fedora packages: Fedora Haskell SIG (until F12)

- **Sparc**

  - Solaris: Position open
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

## Past contributors


These people have contributed to GHC and its core libraries in the past, in various
wonderful ways:

- Krasimir Angelov
- Lennart Augustsson (including work on many core libraries, originally for hbc)
- Jean-Philippe Bernardy (Maintenance of Data.\*Map/\*Set libraries)
- [ Jost Berthold](http://www.mathematik.uni-marburg.de/~berthold) (Parallel Haskell: Eden)
- Bjorn Bringert
- Sebastien Carlier
- Andrew Cheadle
- [ Tim Chevalier](http://catamorphism.org/) (External Core, strictness analysis, ticky-ticky profiling)
- Duncan Coutts (parallel profiling, package system)
- [ Iavor S Diatchki](http://www.csee.ogi.edu/~diatchki) ('monadLib' library)
- Sigbjorn Finne
- Andy Gill (Haskell Program Coverage)
- Kevin Glynn
- John Goerzen
- Cordy Hall
- Kevin Hammond
- Tim Harris (Transactional memory, concurrency)
- David Himmelstrup (GHC developer)
- José Iborra (GHCi Debugger)
- Isaac Jones (Cabal, libraries)
- Ralf Laemmel
- Roman Leshchinskiy
- Andres Loeh
- Hans Wolfgang Loidl
- John Launchbury
- Ryan Lortie (PowerPC Linux port, packages)
- Jim Mattson
- John Meacham
- Darren Moffat
- Nick Nethercote
- Thomas Nordin
- Sven Panne
- Sungwoo Park
- Will Partain
- Ross Paterson
- Juan Quintela
- [ Norman Ramsey](http://www.eecs.harvard.edu/nr) (a little refactoring in the back end)
- Alastair Reid
- Ben Rudiak-Gould
- Patrick Sansom
- André Santos
- Sean Seefried (Plug-in optimisations (in pluggable-branch))
- Julian Seward
- Dominic Steinitz (Cryptographic library)
- Don Stewart (OpenBSD support)
- Volker Stolz
- Josef Svenningsson
- Audrey Tang (Perl-related tools (ghc-asm, ghc-split), AIX port)
- Dinko Tenev
- Wolfgang Thaller (Darwin/Mac OS X ports, position-independent code generation)
- Mike Thomas
- Reuben Thomas
- Christopher D. Thompson-Walsh
- Dylan Thurston
- Phil Trinder
- Mark Tullsen
- David N Turner
- Philip Wadler
- David Waern (Haddock comment support)
- Malcolm Wallace
- Michael Weber
- Ashley Yakeley (Time library)
- N. Xu
