# Project suggestions for the 2006 GHC hackathon


Add your suggestions for the hackathon below...

## GHC Projects

- Hack to allow RTS to integrate with an external event loop (eg to give us ideal threading when using Gtk+)

- Add a `ghc --clean` that just executes `find -name '*.o' -o -name '*.hi' -exec rm {} \;`, perhaps in a more portable fashion

- Further work on debugging in GHCi

- Improve the native code generator: see [BackEndNotes](back-end-notes)

- Get the front panel working again ([\#599](https://gitlab.haskell.org//ghc/ghc/issues/599))

- WARNING pragma ([\#657](https://gitlab.haskell.org//ghc/ghc/issues/657))

- Warning suppression ([\#602](https://gitlab.haskell.org//ghc/ghc/issues/602))

- Accept more encodings for source code ([\#690](https://gitlab.haskell.org//ghc/ghc/issues/690))

- Use gcc's libffi to implement Adjustor.c & ByteCodeFFI

- Add :edit support to GHCi ([\#95](https://gitlab.haskell.org//ghc/ghc/issues/95))

- Option to allow compiling from within GHCi ([\#276](https://gitlab.haskell.org//ghc/ghc/issues/276))

- GHC plugins: allow passes to be loaded at runtime from plugins

- Get shared libraries or DLLs working

- Build a Windows-native version of GHC (using MS tools instead of gcc).

- GHC API clients: hstags, code metrics (eg. feature use)

- Implemene the static argument transformation ([\#888](https://gitlab.haskell.org//ghc/ghc/issues/888))

- Whole-program dead-code detection (with `--make`).

- Whole-program overloading elimination (with `--make`).

- Work on Haddock on top of the GHC API (see [ this message](http://www.haskell.org/pipermail/haskell/2006-August/018415.html)).

- `-Ofile`: take the list of optimisation passes to run from a file instead of current hard-wired sequence.

## Library Projects

- Work on the Streams library

- Replace GMP with OpenSSL mp library ([\#601](https://gitlab.haskell.org//ghc/ghc/issues/601))

- A binding for your favourite C/C++ library...  (eg. GStreamer?)

## Cabal Projects

- Cabalisation of the GHC library ([\#654](https://gitlab.haskell.org//ghc/ghc/issues/654))

- Put two solid days of work into getting cabal-get finished and stable.  Additionally, put as many packages into the DB as possible.

## More Project Suggestions

- Generate Windows installers automatically from Cabal packages (or
  other OS-specific package format, eg. RPM)

- Any of the bugs (Ticket query: status: new, status: assigned, status: reopened, type: bug, order: priority), tasks (Ticket query: status: new, status: assigned, status: reopened, type: task, order: priority, group: difficulty), or feature requests (Ticket query: status: new, status: assigned, status: reopened, type: feature+request, order: priority).
