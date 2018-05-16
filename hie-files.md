# HIE (.HI Extended) Files


.hie files are a proposed new filetype that should be written by GHC next to .hi files.

## The Problem


GHC builds up a wealth of information about haskell source as it compiles it, but throws all of it away when it's done. Any external tools that need to work with haskell source need to parse, typecheck and rename files all over again. This means haskell tooling is slow and has to rely on hacks to extract information from ghc.
Allowing GHC to dump this information to disk would simplify and speed up tooling significantly, leading to a much richer and productive haskell developer experience.


As a proof of concept, haddocks --hyperlinked-source feature will be rewritten to make use of .hie files, such that it doesn't need to recompile the source.

## File Contents

- The data structure should be a simplified, source aware, annotated AST derived from the Renamed/Typechecked Source
- The type of every Name/symbol in the source should be included. Furthermore, the type should be specialised to the occurrence, so in the following snippet, `return` should be assigned the type `() -> IO ()` instead of `Monad m => a -> m a`

  ```wiki
  main :: IO ()
  main = return ()
  ```

- It will be similar to \[RichToken\] format consumed by haddocks hyperlinker, but structured like a tree to accurately represent the Haskell AST as tooling like haskell-ide-engine might require more detailed information about source structure, like parent/sibling/child nodes in the AST
- It should be possible to exactly recover the source from the .hie file. This will probably be achieved by including the source verbatim in the .hie file, as recovering the source exactly from the AST might be tricky and duplicate the work on ghc-exactprint.
- In the AST, instead of storing copies of the original Token source string, we can simply point to the relevant portions of the source
- There will be a table consisting of all unique types that occur in the source. Elements of the AST with types will point to entries in the table, so that type duplication doesn't blow up the size of the file too much. Even type subtrees might need to be deduplicated.
- The actual representation on disk as well as serialisation/de-serialisation could be done through CBOR, using the package [ serialise](https://hackage.haskell.org/package/serialise-0.2.0.0).
- The first line of the .hie file should be a human readable string containing information about the version of the format, the filename of the original file, and the version of GHC the file was compiled with. Example: (v1.0,GHC8.4.6,Foo.hs)
- The format should be fairly stable across ghc versions, so we need to avoid capturing too much information. For this reason the tree should only capture scoping information, and nothing more. More detailed information about the exact haskell syntactic structure a part of the tree represents could be obtained by inspecting the tokens/keywords in that part.


The RichToken type used in haddock: [ https://github.com/haskell/haddock/blob/master/haddock-api/src/Haddock/Backends/Hyperlinker/Types.hs\#L35](https://github.com/haskell/haddock/blob/master/haddock-api/src/Haddock/Backends/Hyperlinker/Types.hs#L35)

## Use cases

- Haddocks hyperlinked source and haskell-ide-engine

  - Type information on hover
  - Local(in file) usage sites for symbols
  - Supporting global go to/view definition for every symbol in the Package Db
  - Viewing info about arbitrary nodes in the AST - does it have a type? What language construct does it correspond to?
- Along with an indexer that scans .hie files

  - Viewing the usage sites of symbols across the entirety of hackage or a local Package Db
  - Dependency analysis of symbols - what other symbols does something depend on
  - Searching for symbols, and restricting search by type. Example: search for usages of `read` with type `String -> Int` to find out where the instance for `Read Int` is being used.
- More sophisticated analysis of the AST

  - Diffing changes to the AST
  - Viewing typical invocations/example usages of functions

## Modifications to GHC

- HIE file generation will be controlled by a GHC flag(--enable-ide-info?)
- The file will be generated as soon as GHC is done typechecking a file(maybe in [ hscIncrementalCompile](https://www.stackage.org/haddock/nightly-2018-05-04/ghc-8.4.2/src/HscMain.html#hscIncrementalCompile)?)
- Need to coordinate with the Hi Haddock project(Including docstrings in .hi files) as that may push the burden of resolving Names/Symbols in haddock comments onto GHC.
- Other than this, little interaction with the rest of GHC should be needed.

## Why should we be able to recover file contents exactly?


Consider the case when the .hs source file that exists on disk doesn't compile, but with still have a stale .hie file generated the last time the source compiled. We would like to recover as much information as possible from the
stale .hie file to aid the user working on the .hs file. This is possible if we recover the original, compiling source from the .hie file and cross-reference/diff it with the edited file, so that we can still answer user queries for
portions of the file that haven't been edited(Indeed, this is how haskell-ide-engine currently works, but instead of reading from a .hie file, it maintains an in-memory cache of the last good `TypecheckedModule` corresponding to the source)

## Links to additional discussion

[ Initial discussion on \#ghc](https://gist.github.com/wz1000/46bb4b2121f0911bbbf4d4743fafaba8) (The .hie(.hi Extended) name suggested by mpickering, cbor serialisation suggested by hvr)

[ https://github.com/haskell/haddock/issues/715](https://github.com/haskell/haddock/issues/715)

[ Original GSOC Proposal](https://docs.google.com/document/d/1QP4tV-oSJd3X90JKVY4D__Dfr-ypVB57p1yDqyk2aQ8/edit?usp=sharing)


Why CBOR over binary/cereal? [ http://code.haskell.org/\~duncan/binary-experiment/binary.pdf](http://code.haskell.org/~duncan/binary-experiment/binary.pdf)