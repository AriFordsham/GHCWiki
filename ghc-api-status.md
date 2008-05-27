# GHC API Improvement Status


This Wiki page shall serve as a central place to collect all issues and ideas related to the GHC API.  If you feel that something is missing from this page, please add a comment to the comment section below.


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

## Various Ideas, Comments, Questions

- **Interface Stability** - Is there a way to reduce version-skew for clients of the GHC
  API (currently, there is no stability guaranteed at all, so if you
  don't want to live with lots of \#ifdefs and breakage, you keep
  delaying your fantastic GHC API-base projects "until the dust
  settles") (Claus Reinke)
- Is it possible to use standalone deriving to get a generic
  programming framework over the ASTs without blowing
  up GHC's code for its own use (deriving Data, etc.)? (Claus Reinke)
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
- dynamic loading of Haskell code, ala hs-plugins, but without
  the version/platform issues (GHCi has to be able to do this
  anyway, but it would be nice to have the ugly bits hidden,
  such as `unsafeCast#`, or whatever it was). that might require
  a standard for typeReps, if I recall correctly.. (Claus Reinke)
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
