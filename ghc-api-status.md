# GHC API Improvement Status


This Wiki page shall serve as a central place to collect all issues and ideas related to the GHC API.  If you feel that something is missing from this page, please add a comment to the comment section below.

---

# Current GHC API design


Most exported API functions that were previously in `IO` are now
in the `Ghc` monad.  Those functions now no longer require a `Session`
parameter.  To start a GHC API session you now use:

```wiki
withGhc :: Maybe FilePath  -- path to GHC library
        -> Maybe [String]  -- ^ Optional list of static flags.
        -> Ghc a           -- ^ The action(s) to perform.
        -> IO a
```


The first parameter can be determined automatically with the ghc-path
package.  The second is a set of "static" command line flags, for
example, profiling options.  Having those part of the run function for
the monad avoids complicated usage rules (e.g. before parseStaticFlags
had to be called before `newSession`).

`load` and `setTarget` work like before.  `checkModule`
has been split up into:

```wiki
parseModule :: ModuleName -> Ghc ParsedModule
typecheck :: ParsedModule -> Ghc TypecheckedModule
desugarModule :: TypecheckedModule -> Ghc DesugaredModule
loadModule :: TypecheckedMod m => m -> Ghc ()
```

`TypecheckedMod` has two instances `TypecheckedModule`,
`DesugaredModule`.  A `DesugaredModule` desugared module is a
very similar to a `CoreModule`, but the latter contains less
information, while the former also contains all the information from
the parser and typechecker.  Users should therefore be careful about
memory leaks by and discard old intermediate results as soon as
possible.


Compile errors are thrown inside the `Ghc` monad, and can be
caught with `onCompileError`.  `IO` errors can also be caught
using `ghcCatch`.  Warnings are logged automatically and can be
retrieved using `getWarnings` or discarded using
`clearWarnings`.  Error messages are communicated in the
exceptions thrown.

## Current Work


I am currently evaluating what information can be added to the output
of the various phases and how hard it would be to modify or create
intermediate types (e.g., typecheck a "hand-crafted" module).


Additionally, logging of errors and warnings is somewhat spread-out,
so I try to find the places where that happens and incorporate that
into the `Ghc` monad.

---

# Project Overview


Simon Marlow reported that the last time he tried to work on the GHC API, it turned into a huge refactoring task.  This could mean that it may take a while until bigger changes are visible and it is very likely that programs that currently use the GHC API will break.  Hopefully, though, it will lead to a more usable GHC API and maybe to a more hackable code base in general.  


I plan to work with a use case and try to make it as clear and simple as possible by adapting the GHC API.  The use case will be a program that generates HTML pages for Haskell programs which will give access to more and more semantic information.  E.g. it will start out with linking references of names to their definitions (possibly to import statement or link to other module), then give typing information, and later give types for sub-expressions.  A lot of this is already possible, but some may not.  


This use case will hopefully reflect many features an IDE or code analysis tools need.  Once this is working, we can move on to performing various transformations on the given code, or only a selected part of it.  This will should help figure out what information needs to be available to work with exposed, separate compilation phases.


There are also a few non-functional requirements:

- Using the API should be fairly self-explainatory and safe.  I.e., where necessary inputs are checked for invariants and there should be no implicit dependencies.  More concretely:

  - If several phases use the same AST, the AST will contain a type parameter which corresponds to the phases that have been performed with it.  Hence, if a function requires input of type `AST Phase3` then it is clear that the phases with types `AST Phase2 -> AST Phase3` and `AST Phase1 -> AST Phase2`  must be performed first.
  - GHC uses some evil hacks to simulate global variables but has some implicit assumptions when those are actually accessible.  If you call a function too early, ie., before a certain variable is initialised, GHC will die with a very unhelpful error message.  I hope to make as many of those dependencies explicit and encode those dependencies via the API (mostly via types).

## Trac Tickets Related to the GHC API

- [\#1467](https://gitlab.haskell.org//ghc/ghc/issues/1467) - GHC API: expose separate compilation stages
- [\#1886](https://gitlab.haskell.org//ghc/ghc/issues/1886) - GHC API should preserve and provide access to comments
- [\#654](https://gitlab.haskell.org//ghc/ghc/issues/654) - Cabalization of the GHC library.

### Possibly Related

- [\#2159](https://gitlab.haskell.org//ghc/ghc/issues/2159) - Use a more efficient representation than `[DynFlag]`
- [\#1631](https://gitlab.haskell.org//ghc/ghc/issues/1631) - Make the External Package Table contain `ModDetails` not `ModIface`

## Related Documents and Discussions

- [ The GSoC proposal](http://code.google.com/soc/2008/haskell/appinfo.html?csaid=4189AF2C8AE5E25A)
- Related GSoC project [ Dynamically Loaded Plugins for the Glasgow Haskell Compiler](http://code.google.com/soc/2008/haskell/appinfo.html?csaid=69C2ABD1E013EE0C)
- Haskell-cafe question: [ How to get the typechecked AST?](http://www.haskell.org/pipermail/haskell-cafe/2008-May/042616.html)
- [ Porting HaRe to the GHC API](http://www.cs.kent.ac.uk/pubs/2005/2266/) Technical report describing some difficulties with the current API.
- [ GHC as a Library](http://www.haskell.org/haskellwiki/GHC/As_a_library), the Haskell Wiki page
- [ GHC Commentary on the GHC API](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/API) (may be outdated)
- [ hint, an attempt to provide a simplified and stable subset of the GHC API](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hint-0.2.1)

## Various Ideas, Comments, Questions

- **Interface Stability**

  - Is there a way to reduce version-skew for clients of the GHC API (currently, there is no stability guaranteed at all, so if you don't want to live with lots of \#ifdefs and breakage, you keep delaying your fantastic GHC API-base projects "until the dust settles") (Claus Reinke)
  - Would it be possible to separate the monolithic GHC API into two parts, one providing a simplified and stable subset/wrapper of commonly used functionality (as in Hint, hs-plugins, GHCi), the other providing all the rest, with no stability guarantees? (Claus Reinke)
- **Ast Traversals/Queries**, see: [GhcApiAstTraversals](ghc-api-ast-traversals)

  - Is it possible to use standalone deriving to get a **generic programming framework over the ASTs** without blowing up GHC's code for its own use (deriving Data, etc.)? (Claus Reinke)
  - David Waern mentions [ deriving \`Data.Traversable\`](http://www.haskell.org/pipermail/haskell-cafe/2008-May/042961.html) for GHC's AST
- **GHC library directory in GHC API clients**

  - the need to hardcode the libdir is very fragile and troublesome (cf. the [ Haddock version during build](http://www.haskell.org/pipermail/cvs-libraries/2008-June/008942.html) thread on `cvs-ghc` for just one example). would it be possible to integrate the path for the compiling GHC as a default, so that one only needs to specify an explicit path if the default doesn't work (compiling GHC moved/unavailable)? (Claus Reinke) 
  - this has been addressed by the new [ ghc-paths](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/ghc-paths) package
- **binary incompatibility of GHC versions**

  - this also affects GHC Api clients, see [ the Haddock 2 in GHC build issues](http://www.haskell.org/pipermail/cvs-ghc/2008-July/043568.html) for an infamous example
- From `compiler/main/GHC.hs`:

  ```wiki
  -- NOTE:
  --   - things that aren't in the output of the typechecker right now:
  --     - the export list
  --     - the imports
  --     - type signatures
  --     - type/data/newtype declarations
  --     - class declarations
  --     - instances
  --   - extra things in the typechecker's output:
  --     - default methods are turned into top-level decls.
  --     - dictionary bindings
  ```
- is there a way to make all the useful functionality of GHCi more easily available from the GHC API? ie, refactoring GHCi so that both it and other GHC API clients can use the same collection of useful functionality? (Claus Reinke)
- dynamic loading of Haskell code, ala hs-plugins, but without
  the version/platform issues (GHCi has to be able to do this
  anyway, but it would be nice to have the ugly bits hidden,
  such as `unsafeCast#`, or whatever it was). that might require
  a standard for typeReps, if I recall correctly.. (Claus Reinke)
- since the refactoring ideas below mention error handling: it appears that some GHC Api functions output error messages directly, without providing a means to handle/capture them in callers. I ran into one such instance a while ago ([\#1463](https://gitlab.haskell.org//ghc/ghc/issues/1463), comments 8, 10, 11)
- ... more comments here ...

## Refactoring Ideas


There follow some notes about desirable refactorings, mainly around [compiler/main/HscMain.lhs](/trac/ghc/browser/ghc/compiler/main/HscMain.lhs).  These will be important when looking at how to modify the GHC API to expose the individual compilation stages.  At the moment, the compilation stages are all hidden behind the `HscMain` interface, which in turn is hidden behind the `DriverPipeline` module, which is used by the code in `GHC`.  In order to untangle things, we need to make some changes.  Not all of these are essential, and some of them might not even end up being good ideas at all; this is just a list of things we (Simon M & Simon PJ) noticed while doing a code walkthrough.

- We should separate the action of reading the old interface from checking its usages.  Currently the two
  are mixed up in `checkOldIface`.  (in the new story with fingerprints instead of versions, we also want
  to discard the old interface as soon as we decide to recompile, because it isn't necessary for calculating
  the new versions now).

- Perhaps `HsModule` should indicate whether the module is an `hs-boot` module or not.  That would reduce the
  number of arguments to `tcRnModule` by one.

- It would be nicer to return error messages from each phase directly rather than invoking the `log_action` callback
  in `DynFlags`.

- What is currently called `RenamedStuff` should be `HsModule Name`.  Hence, the `tcg_rn_` files in `TcGblEnv`
  can be merged into a single `tc_rn_module`.

- instead of passing a `Bool` to `tcRnModule` to ask for the renamed syntax, use a flag in `DynFlags`?

- `mi_globals` is in the wrong place: it is not part of the interface.  The reason it is where it is is because
  we need to keep it when a module is considered for compilation but not recompiled; when we generate the
  `ModDetails` from the `ModIface`.  ToDo: find a better place to put it.

## Change Log

- introduced the `Ghc` monad which is (currently) defined as `Session -> IO (Either GhcError a)` where Session is mutabe.  This improves the following:

  - API functions no longer need to pass around sessions explicitly.  The single-threaded usage of a session is enforced.
  - More error information than a simple `Nothing`.
  - Errors can be handled in one place rather than case matching after each API call
  - ToDo: evaluate role of `log_action`
- Functions that previously worked on a `Session` are now in `Ghc`.  `newSession` is now `withGhc` which is the exported run function for the `Ghc` monad.
- `parseStaticFlags` has been removed, the functionality is now integrated with `withGhc`.  Before users had to be really careful in what order to call functions.
- ATM, most things that import `GHC.hs` are broken due to the monadification.

## Open Issues

- Which operations should automatically call `clearWarnings`?
- Get Haddock to work.
- Error handling strategy.
- Remove `compileToCore` (it says it's there for backwards compatibility only)
- What's the deal with `SuccessFlag`?  Shouldn't that be `Either SomeError ()`?
- Comment in HscTypes:

  ```wiki
  -- If the module does come from the home package, why do we look in the PIT as well?
  -- (a) In OneShot mode, even home-package modules accumulate in the PIT
  -- (b) Even in Batch (--make) mode, there is *one* case where a home-package
  --     module is in the PIT, namely GHC.Prim when compiling the base package.
  -- We could eliminate (b) if we wanted, by making GHC.Prim belong to a package
  -- of its own, but it doesn't seem worth the bother.
  ```

  We now have a separate `ghc-prim` package.  Should we eliminate (b) then?
