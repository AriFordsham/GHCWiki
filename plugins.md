# GHC Plugins

## Intent


We would like to support dynamically-linked Core-to-Core plug-ins, so that people can add passes simply by writing a Core-to-Core function, and dynamically linking it to GHC. This would need to be supported by an extensible mechanism like attributes in mainstream OO languages, so that programmers can add declarative information to the source program that guides the transformation pass. Likewise the pass might want to construct information that is accessible later. This mechanism could obviously be used for optimisations, but also for program verifiers, and perhaps also for domain-specific code generation (the pass generates a GPU file, say, replacing the Core code with a foreign call to the GPU program). 

## Implementation Speculation

**Here be dragons! **. This is just the result of my very preliminary thinking about how we might implement this in GHC. I am by no means an expert and so this section is certainly neither authoritative or correct! - Max Bolingbroke, March 08

### User Interface


The user would write code something like the following:

```wiki

module Main where

{-# PLUGIN nVidiaGPU NVidiaGPUSettings { useTextureMemoryMb = 256 } #-}

{-# PLUGIN nVidiaGPU doSomethingExpensive NVidiaGPUFunctionSettings { maxStackDepth = 1024 } #-}
doSomethingExpensive :: Int -> ImpressiveResult
doSomethingExpensive = ....

main = print (map doSomethingExpensive [1..10])

```


That is, they can include two types of pragmas. Ambient configuration ones:

```wiki
{-# PLUGIN plugin_name some_data_structure #-}
```


And binder-specific ones:

```wiki
{-# PLUGIN plugin_name binder some_data_structure #-}
```


An alternative would be to allow some of the GHC command line arguments to filter through to the plugin:

```wiki
ghc --make Main.hs ... -pgpu-texture-memory-mb=256 ...
```

### Plugin Writer Interface


The plugin writer would provide:

- Loadable library that contains their plugin code: this would be placed in a magic GHC subdirectory, referenced somehow in package.conf or something. It would probably be best if we had plugins distributed as source and cabal-built / installed via the usual package mechanism so that they can deal with many architectures / GHC versions.
- *Optionally*, a separate library / component of their plugin library which contains the definitions for the data types they are going to want to use in their pragmas, such as NVidiaGPUSettings.


We should provide at least the following hooks to the plugin library:

- Initial configuration: this is when it gets fed the modules ambient PLUGIN pragma information
- Ability to install extra core passes (their library would be fed the core syntax tree either as [ExternalCore](external-core) or as GHCs core data type (via the GHC-as-a-library stuff))
- Ability to add notify GHC about extra things to link in (e.g. GPU code compilation artifacts). This does not necessarily play well with separate compilation: would have to do something like add info to .hi so the eventual linking stage can find out about any extra artifacts that might be required
- Any more you can think of....

### GHC Stuff


The compiler needs to bring the plugin writer and users contributions together. A possible game plan might be:

1. Suck in Haskell source as normal, but also parsing PLUGIN pragmas
1. Load up any plugins we see referenced by PLUGIN pragmas, discarding those we don't know about (on the basis that PRAGMAS are meant to be extra information that isn't essential to compile the program, though we might want to at least warn if we can't find one!). This could happen via [ hs-plugins](http://www.cse.unsw.edu.au/~dons/hs-plugins/).
1. Give the plugins a chance to add some "implicit" imports so that the renamer can see the pragma data types when going through the pragmas. Again this is motivated by the idea that programs that use plugins should still compile if they are absent.
1. Rename, type check as normal, though TcBinds.mkPragFun will need to be extended to splat binder-specific PLUGIN pragmas on those binders
1. Desugarer will have to do something to make the pragma data structure syntax trees into an actual in-memory value. Essentially, we want to evaluate the expression in a context where it just has access to the constructors of the plugins imported data type library. We could reuse some of the GHCi apparatus to do this, see e.g. HscMain.compileExpr, HscMain.hscStmt and Linker.linkExpr. We then wrap this value up into an existential type that has a Dynamic instance and put it into the pragma on the Core (so the typechecker will have to make sure this part of the pragma has such an instance).
1. Give the plugin a chance to install core passes into the pipeline. For each one (assuming we use external core..):

  1. Convert the input into external core. Note this loses information we try and construct in the core2core pipeline like demands and strictness, so this probably limits plugins to early or late in the pipeline where this doesn't matter, or else we will just have to reconstruct the info after every plugin pass..
  1. Feed the external core to the dynamically linked pass function. To ensure binary compatibility we will have to lift external-core out to a library that the stage2 compiler and the plugin both depend on. 
  1. The plugin will encounter CoreSyn Notes that include information about various binders: it can use Dynamic to try and cast the existentially typed value this contains to an appropriate type from the accompanying pragma data type library
  1. Convert the returned core back into GHC core and give that to the next core2core stage.
1. Codegen etc as normal
1. Add to the output file links to anything the plugin specified. Somehow record the extra library files to eventually link in, e.g. via a .hi file


As you can see there are a lot of design choices we might make. Feel free to add your own thoughts on this feature to the page.
