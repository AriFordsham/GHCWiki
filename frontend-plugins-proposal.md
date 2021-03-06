## Overview


The current support for plugins in ghc is limited to the Core representation;
it is not currently possible to write plugins that operate on the typed (or
untyped AST), before it gets converted to Core. Support for "source plugins"
(as opposed to "core plugins") is useful also for applications that use the GHC
API as a library, as it makes it possible to hook into ghc's make system. The
attached patch, against HEAD (July 22, 2013), focusses on the latter. It would
probably be a good idea to make the treatment of core and source plugins more
uniform; some ideas on that below.


The patch introduces a concept of a "source plugin" defined as 

```wiki
data HscPlugin = HscPlugin {
    runHscPlugin :: forall m. MonadIO m => DynFlags
                                        -> TcGblEnv
                                        -> m TcGblEnv

  , runHscQQ     :: forall m. MonadIO m => Env TcGblEnv TcLclEnv
                                        -> HsQuasiQuote Name
                                        -> m (HsQuasiQuote Name)
  }
```

`runHscPlugin` is the main function; it gets passed the `TcGblEnv`, which
includes a reference to the typed AST (and also the renamed AST, see below).
`runHscQQ` gets called whenever the typechecker/renamer expands a quasi-quote,
because no evidence of QQs is left in the AST; plugins that need to see the
quasi-quote in original form can implement runHscQQ. This is implemented in a
separate module (compiler/main/HscPlugin.hs and
compiler/main/HscPlugin.hs-boot) to avoid circular dependencies between
modules.


We then extend `DynFlags` to include  

```wiki
sourcePlugins :: [HscPlugin],
```


alongside the existing entries for source plugins 

```wiki
pluginModNames        :: [ModuleName],
pluginModNameOpts     :: [(ModuleName,String)],
```

## Calling the plugin


We make three modifications. First, we modify `genericHscFrontend` in `compiler/main/HscMain.hs` to call the plugin:

```wiki
genericHscFrontend :: ModSummary -> Hsc TcGblEnv
genericHscFrontend mod_summary
    | ExtCoreFile <- ms_hsc_src mod_summary =
        panic "GHC does not currently support reading External Core files"
    | otherwise = do
        dynFlags   <- getDynFlags
        tc_result  <- hscFileFrontEnd mod_summary
        runHscPlugin (mconcat (sourcePlugins dynFlags)) dynFlags tc_result
```


Second, we modify `runQuasiQuote` in `compiler/typecheck/TcSplice.hs` to call `runHscQQ`:

```wiki
          -- Notify any source plugins about the QQ
        ; env <- getEnv 
        ; HsQuasiQuote quoter' q_span quote <- 
            runHscQQ (mconcat . sourcePlugins . hsc_dflags . env_top $ env) 
                     env 
                     (HsQuasiQuote quoter' q_span quote)
```


Finally, we make sure that the typechecker leaves the renamed AST (as opposed to the typechecked AST) available to the plugin by passing `True` instead of `False` to `tcRnModule'` in `hscFileFrontend`in `HscMain.hs`:

```wiki
hscFileFrontEnd :: ModSummary -> Hsc TcGblEnv
hscFileFrontEnd mod_summary = do
    hpm <- hscParse' mod_summary
    hsc_env <- getHscEnv
    tcg_env <- tcRnModule' hsc_env mod_summary True hpm
    return tcg_env
```


This is important because the typechecked AST misses information (see [http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html](http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html)). The better solution here would probably be to make this configurable at runtime (an additional flag in `DynFlags`).

## Unifying source and core plugins


The current support for core plugins is very much targeted towards "external"
plugins (as opposed to "hooks" for code using the GHC API), as is evident from
the types above: it makes a module name, not a closure. This makes it awkward
to use core plugins as a hook mechanism.


Conversely, the attached patch \*only\* makes source plugins available as a hook,
and makes no effort to introduce the concept of source plugins available on the
command line. 


It would be useful to unify both designs, replacing the `pluginModNames` and
`pluginModNameOpts` fields with something like

```wiki
sourcePlugins :: [CorePlugin],
```


where core plugin might be something like

```wiki
data CorePlugin = CorePlugin {
    runCorePlugin :: [CoreToDo] -> CoreM [CoreToDo]
  }
```


The resolution of module names to these closures, as well as passing in the
command line arguments, could be done at an earlier stage.


Command line support for source plugins could then be added in a similar
fashion.

## Patches


Attached are patches against 7.4.2 and 7.7. Note that the patch against 7.7 invalidates the check implemented in #7780; you have to temporarily disable it:

```wiki
diff --git a/utils/dll-split/Main.hs b/utils/dll-split/Main.hs
index 336b6d9..c92ff8d 100644
--- a/utils/dll-split/Main.hs
+++ b/utils/dll-split/Main.hs
@@ -41,7 +41,8 @@ doit depfile startModule expectedReachableMods
           putStrLn "Please fix it, or building DLLs on Widnows may break (#7780)"
           tellSet "Redundant modules" redundant
           tellSet "Extra modules"     extra
-          exitFailure
+          putStrLn "WARNING: Continuing regardless!"
+          -- exitFailure
 
 newtype Module = Module String
     deriving (Eq, Ord)
```


(Not completely sure what is the ramifications of that are, but I think it means that the Windows DLL will be broken -- so that will need to be addressed before the patch can be accepted).
