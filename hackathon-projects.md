# Project suggestions for the 2006 GHC hackathon


Add your suggestions for the hackathon below...

- Hack to allow RTS to integrate with an external event loop (eg to give us ideal threading when using Gtk+)

- Add a `ghc --clean` that just executes `find -name '*.o' -o -name '*.hi' -exec rm {} \;`, perhaps in a more portable fashion

- Further work on debugging in GHCi

- Improve the native code generator: see [BackEndNotes](back-end-notes)

- Something packaging or Cabal-related

- Get the front panel working again ([\#599](https://gitlab.haskell.org//ghc/ghc/issues/599))

- Replace GMP with OpenSSL mp library ([\#601](https://gitlab.haskell.org//ghc/ghc/issues/601))

- Generate Windows installers automatically from Cabal packages (or
  other OS-specific package format, eg. RPM)

- WARNING pragma ([\#657](https://gitlab.haskell.org//ghc/ghc/issues/657))

- Warning supperssion ([\#602](https://gitlab.haskell.org//ghc/ghc/issues/602))

- Accept more encodings for source code ([\#690](https://gitlab.haskell.org//ghc/ghc/issues/690))

- Use gcc's libffi to implement Adjustor.c & ByteCodeFFI

- Add :edit support to GHCi ([\#95](https://gitlab.haskell.org//ghc/ghc/issues/95))

- Option to allow compiling from within GHCi ([\#276](https://gitlab.haskell.org//ghc/ghc/issues/276))

- Cabalisation of the GHC library ([\#654](https://gitlab.haskell.org//ghc/ghc/issues/654))

- GHC plugins

- Work on the Streams library

- Get shared libraries or DLLs working

- Build a Windows-native version of GHC (using MS tools instead of gcc).

- A binding for your favourite C/C++ library...  (eg. GStreamer?)

- Any of the bugs (Ticket query: status: new, status: assigned, status: reopened, type: bug, order: priority), tasks (Ticket query: status: new, status: assigned, status: reopened, type: task, order: priority, group: difficulty), or feature requests (Ticket query: status: new, status: assigned, status: reopened, type: feature+request, order: priority).
