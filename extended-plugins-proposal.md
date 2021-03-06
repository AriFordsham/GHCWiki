
The proposal is the same as [Edsko's Frontend Plugin Proposal](https://ghc.haskell.org/trac/ghc/wiki/FrontendPluginsProposal) with minor changes.

- Instead of implementing it as a different kind of plugin this solution uses the pre-existing [plugin system](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins). Since typechecker plugins have been introduced to the plugin architecture, it seems a better solution to extend the pre-existing support than to create a new kind of plugin.
- It extend this to cover interface loading as well.
- It changes the loading of plugins to be loaded at an earlier stage of the compilation. This is for efficiency reasons. If a plugin is used multiple times, it is better to load it only once. This is in synch with the "Unifying source and core plugins" part of the proposal.


Extended interface for Plugins:


```
data Plugin = Plugin {
    installCoreToDos :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    -- ^ Modify the Core pipeline that will be used for compilation.
    -- This is called as the Core pipeline is built for every module
    -- being compiled, and plugins get the opportunity to modify the
    -- pipeline in a nondeterministic order.
  , tcPlugin :: [CommandLineOption] -> Maybe TcPlugin
    -- ^ An optional typechecker plugin, which may modify the
    -- behaviour of the constraint solver.
  , parsedResultAction :: [CommandLineOption] -> ModSummary -> HsParsedModule 
                            -> Hsc HsParsedModule
    -- ^ Modify the module when it is parsed. This is called by
    -- HscMain when the parsing is successful.
  , renamedResultAction :: Maybe ([CommandLineOption] -> ModSummary 
                                    -> RenamedSource -> Hsc ())
    -- ^ Installs a read-only pass that receives the renamed syntax tree as an
    -- argument when type checking is successful.
  , typeCheckResultAction :: [CommandLineOption] -> ModSummary -> TcGblEnv 
                               -> Hsc TcGblEnv
    -- ^ Modify the module when it is type checked. This is called by
    -- HscMain when the type checking is successful.
  , spliceRunAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
    -- ^ Modify the TH splice or quasiqoute before it is run.
  , interfaceLoadAction :: forall lcl . [CommandLineOption] -> ModIface 
                                          -> IfM lcl ModIface
    -- ^ Modify an interface that have been loaded. This is called by 
    -- LoadIface when an interface is successfully loaded. Not applied to
    -- the loading of the plugin interface.
  }
```

## Motivation


Let's say we want to visualize the function dependency graph of bindings in a complex project, like GHC itself. [SourceGraph](http://hackage.haskell.org/package/SourceGraph) can already do this kind of analysis, but it requires an input of a Cabal file or a module name to do it. In case a cabal file is given, it parses it with the Cabal API to get the modules to run the analysis on. However to be able to use the tool conveniently on a large project such as GHC, one needs to take in consideration how Cabal or GHC is called during the build procedure. Additionally, although using the Cabal API to parse cabal files is convenient, but translating the data in the cabal file to instructions how GHC API should be used is not trivial.



The ideal solution would be to **extract the information about the source code** of a big Haskell project **during the normal build procedure**, since in this case we don't have to figure out how to compile its parts. What can enable development tools to work with complex projects in their own build environment is the inversion of control. Instead of the tool trying to figure out how to invoke the compiler to parse and typecheck the modules, it would be much better if it would be invoked by the compiler with the kind of program representation it needs. Plugin arguments will not alter the compiler mode or the compilation 


## Existing methods



[Compiler Plugins](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins) would be the best to use, but currently they are only able to access the core reprsentation and install type checker plugins, which are not good for tools that want to analyze/manipulate the code at the source level.



[Compiler hooks](https://ghc.haskell.org/trac/ghc/wiki/Ghc/Hooks) could be used, but they cannot be controlled by simply configuring GHC at runtime. In order to install new hooks, the tool developer would have to create an alternative compiler executable and users would have to replace their GHC executable with the altered one. This means that it is unlikely that multiple tools could be used together.



[Frontend plugins](https://downloads.haskell.org/~ghc/master/users-guide/extending_ghc.html#frontend-plugins) are not applicable, because their usage changes the major mode of the compiler. So if the tool developer wants to go on with the compilation procedure, he must replicate what GHC would do if the frontend plugin was not used. Furthermore, it can't be inserted into a normal build environment, since using the --frontend flag clashes with other mode flags like --make or --interactive. If the build environment already uses these flags, the frontend flag cannot be simply added. Some walkarounds can be used (like creating a wrapper for GHC), but they suffer from the same problems as using compiler hooks. Also check [ https://ghc.haskell.org/trac/ghc/ticket/14018](https://ghc.haskell.org/trac/ghc/ticket/14018)


