# New Plugins work


Max originally did the work on [GHC plugins](plugins) in his GSoC 2008 hacking sprint. It involved the implementation of [annotations](plugins/annotations) as well as a dynamic loading aspect to GHC. While the annotations work was included into GHC HEAD, the loading infrastructure was not. This document describes the current work (as of 2011) to get it integrated into GHC HEAD so you can write core plugins, and future extensions to the interface, primarily writing C-- passes.


This page explains what the plug-in mechanism does, how to use it, and a little about the implementation.  For discussion, and the current state of play, see the ticket: [\#3843](https://gitlab.haskell.org//ghc/ghc/issues/3843). If you're interested in writing plugins for GHC, **please comment and give feedback, we want to do it right**!


1/17/11: I (Austin Seipp) am working on getting the patch cleaned up a little more and tidying it up before it gets integrated. Still need testsuite patches.


NB. Ridiculously incomplete writing/documentation.

## Current overview


Get GHC from its HEAD repository, and apply this patch:

[ http://hackage.haskell.org/trac/ghc/raw-attachment/ticket/3843/ghc_plugins_support_2010_11_19.2.dpatch](http://hackage.haskell.org/trac/ghc/raw-attachment/ticket/3843/ghc_plugins_support_2010_11_19.2.dpatch)


Then build GHC like normal.


Now GHC understands the `-fplugin` and `-fplugin-arg` options. You essentially install plugins for GHC by `cabal install`ing them, and then calling GHC in the form of:

```wiki
$ ghc -fplugin=Some.Plugin.Module -fplugin-arg=Some.Plugin.Module:no-fizzbuzz a.hs
```

`Some.Plugin.Module` should export a symbol named 'plugin' - see the following repository for an example that does Common Subexpression Elimination:

[ https://github.com/thoughtpolice/cse-ghc-plugin](https://github.com/thoughtpolice/cse-ghc-plugin)

### Basic overview of the plugins API for Core


Modules can be loaded by GHC as compiler plugins by exposing a declaration called 'plugin' of type 'GHCPlugins.Plugin', which is an ADT containing a function that installs a pass into the Core pipeline.

```wiki
module Some.Plugin.Module (plugin) where
import GHCPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
}


-- type CommandLineOption = String

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options passes = do
  ...


```


We can think of `CoreToDo` as being a type synonym for `(Core -> Core)` - that is, an installation function inserts a pass into the list of core passes by just inserting itself into the list and returning it. For example, the CSE pass actually couples a simplification pass, followed by CSE into the front of the compilation pipeline:

```wiki
module CSE.Plugin where

...

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options todos = do
    -- You should probably run this with -fno-cse !
    return $ CoreDoPasses [defaultGentleSimplToDo, cse_pass] : todos

cse_pass = CoreDoPluginPass "Plugged-in common sub-expression" (BindsToBindsPluginPass cseProgram)

cseProgram :: [CoreBind] -> CoreM [CoreBind]
cseProgram binds = do
  ...
```


More specifically, a `CoreToDo` describes some sort of particular pass over a Core program that can be invoked as many times as you like. For reference, `defaultGentlSimplToDo` is constructed using `CoreDoSimplify`. We use `CoreDoPasses` to just make it easy to run multiple `CoreToDo`'s together (it would be equivalent to just use `cse_pass` and preceed it with `defaultGentleSimplToDo` in the pipeline directly.) In this case, `cse_pass` is constructed using `CoreDoPluginsPass`, which takes a name and a `PluginPass` which looks like the following:

```wiki
data PluginPass = BindsToBindsPluginPass ([CoreBind] -> CoreM [CoreBind]) -- ^ Simple pass just mutating the Core bindings
                | ModGutsToBindsPluginPass (ModGuts -> CoreM [CoreBind])  -- ^ Pass that has access to the information from a 'ModGuts'
                                                                          -- from which to generate it's bindings
                | ModGutsToModGutsPluginPass (ModGuts -> CoreM ModGuts)   -- ^ Pass that can change everything about the module being compiled.
                                                                          -- Do not change any field other than 'HscTypes.mg_binds' unless you
                                                                          -- know what you're doing! Plugins using this are unlikely to be stable
                                                                          -- between GHC versions
```


Most people will be using the first case - that is, writing a `BindsToBindsPluginPass` that just manipulates every individual Core binding.

### Reflections on the current API for Core passes


Scala's compiler has a plugin API described by \[1\], with examples at \[2\]. Scala is a bit of a different beast, but the compiler fully supports compilation plugins in the same manner we would like GHC to - more to the point, we want to make sure we have a **good API for specifying when plugins are used and executed**. This is a part of the API that is currently rather simplistic and ad-hoc: we just modify the entire list of compiler passes and return a new one for the optimizer to run. GHC constantly implements new optimizations and tweaks old ones, so we want to make sure that authors of plugins have a good means of conveying when their work should occur. When the work happens is just as important as how it happens.


Of course, GHC is changing all the time - authors of plugins should be ready to deal with differences and changes in versions (e.g. Core can change, don't doubt it for a second,) but if we will be providing a public API for writing plugins, we need to make sure it's sensible and usable for the future to come.


Part of the purpose of the plugins work (in my vision) is to help lower the barrier to working with and on GHC, as well as piggyback off the work it's done. So we want to make sure plugins are done right, and look at what others have done right (and wrong.)

TODO expand on this bit and scala's compiler plugin design, primarily its `runsBefore` and `runsAfter` constraints, the old design Max had, and, by extension, on a better method of controlling when plugin passes are run, instead of just relying on a weak (and, I personally think somewhat fragile) installation function.

# The Future

## Plugins for Cmm


Aside from manipulating the core language, we would also like to manipulate the C-- representation GHC generates for modules too.


The [new code generator](commentary/compiler/new-code-gen) (more precisely, the optimizing conversion from STG to Cmm part of GHC) based on hoopl for dataflow analysis is going to be merged into HEAD Real Soon Now. It would be best perhaps to leave this part of the interface alone until it is merged - clients of the interface could then use hoopl to write dataflow passes for Cmm.

- New code generator is based on several phases:

  - Conversion from STG to Cmm
  - Basic optimisations (basic block elim., liveness analysis, etc)
  - CPS Conversion
  - More basic optimisations on CPS form
  - Conversion to old Cmm representation, then passed to backend code generator.

- We need some sort of interface to describe how to insert it into the pipeline - is the Core approach best here, that is, an installation function that just inserts itself into a list?

- Add new interface to `Plugin` next to `installCoreToDos` i.e. `installCmmPass`, that installs a pass of type `CmmGraph -> CmmGraph` into the optimization pipeline somehow?

### Rough API possibilities


NB. This section is based mostly on the upcoming NCG mega-patch, which re-engineers the C-- implementation to use Hoopl. If you want to follow along, you'll need the patch, which can be seen and downloaded here:

[ http://www.haskell.org/pipermail/cvs-ghc/2011-January/059015.html](http://www.haskell.org/pipermail/cvs-ghc/2011-January/059015.html)

TODO fixme

#### The Hoopl story - analysis and transformation composition

[ Hoopl](http://hackage.haskell.org/package/hoopl) is the main workhorse behind the new GHC code generator - it is a sophisticated, higher order and highly polymorphic library for writing optimization and dataflow passes over imperative code graphs. It is based on the work of Lerner, Grove and Chambers \[3\], and one of the nice properties of the design is that it makes it easy to compose independent dataflow analysis to create a super-analysis that is more powerful than either analysis on its own, without the tedium of having to manually write such a super-analysis.


The question here is: how can users specify in what ways their plugin pass composes with other passes? This is one of the very tantalizing things about hoopl: independent writers of optimization passes can piggyback off each other and combine separate passes to create a much more powerful optimization pass. Part of the question for the API is - how should/could plugin writers compose their own transformation with the transformations offered by another plugin, or by GHC itself?

TODO fixme. check and explain current Hoopl combinators for deep/shallow passes (deepFwdRw and deepBwdRw,) as well as composing them (thenFwdRw and thenBwdRw) and how they should fit into this part of the API

# References

- \[1\] "Scala Compiler Phase and Plug-In Initialization for Scala 2.8" (PDF) - [ http://www.scala-lang.org/sites/default/files/sids/nielsen/Thu,%202009-05-28,%2008:13/compiler-phases-sid.pdf](http://www.scala-lang.org/sites/default/files/sids/nielsen/Thu,%202009-05-28,%2008:13/compiler-phases-sid.pdf)
- \[2\] "Writing Scala Compiler Plugins" [ http://www.scala-lang.org/node/140](http://www.scala-lang.org/node/140)
- \[3\] "Composing dataflow analysis and transformation" (PDF) - [ http://cseweb.ucsd.edu/\~lerner/UW-CSE-01-11-01.pdf](http://cseweb.ucsd.edu/~lerner/UW-CSE-01-11-01.pdf)