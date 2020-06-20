# Wired-in and known-key things


There are three categories of entities that GHC "knows about"; that is, information about them is baked into GHC's source code.

- [Wired-in things](commentary/compiler/wired-in#wired-in-things) --- GHC knows everything about these
- [Known-key things](commentary/compiler/wired-in#known-key-things) --- GHC knows the *name*, including the `Unique`, but not the definition
- [Orig RdrName  things](commentary/compiler/wired-in#orig-rdrname-things) --- GHC knows which module it's defined in

## Wired-in things


A **Wired-in thing** is fully known to GHC.  Most of these are `TyCon`s such as `Bool`. It is very convenient to simply be able to refer to `boolTyCon :: TyCon` without having to look it up in an environment.  


All [primitive types](commentary/compiler/type-type#classifying-types) are wired-in things, and have wired-in `Name`s.  The primitive types (and their `Names`) are all defined in [compiler/GHC/Builtin/Types/Prim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Types/Prim.hs).


The non-primitive wired-in type constructors are defined in [compiler/GHC/Builtin/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Types.hs).  There are a handful of wired-in `Id`s in [compiler/GHC/Types/Id/Make.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Types/Id/Make.hs). There are no wired-in classes (they are too complicated). 


All the non-primitive wired-in things are *also* defined in GHC's libraries, because even though GHC knows about them we still need to generate code for them. For example, `Bool` is a wired-in type constructor, but it is still defined in `GHC.Base` because we need the info table etc for the data constructors.  Arbitrarily bad things will happen if the wired-in definition in [compiler/GHC/Builtin/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Types.hs) differs from that in the library module.



All wired-in things have a `WiredIn` `Name` (see [Names](commentary/compiler/name-type)), which in turn contains the thing.  See [a case study of Bool implementation](commentary/compiler/case-studies/bool) for more details.


## Known-key things


A **known-key thing** has a fixed, pre-allocated `Unique` or **key**.  They should really be called "known-Name" things, because the baked-in knowledge is:

- Its defining `Module`
- Its `OccName`
- Its `Unique`


Almost all known-key names are defined in [compiler/GHC/Builtin/Names.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Names.hs); for example: `GHC.Builtin.Names.eqClassName :: Name`.


The point about known-key things is that GHC knows its *name*, but not its *definition*.  The definition must still be read from an interface file as usual. The known key just allows an efficient lookup in the environment.

## Initialisation


When reading an interface file, GHC might come across "GHC.Base.Eq", which is the name of the `Eq` class.  How does it match up this occurrence in the interface file with `eqClassName` defined in `GHC.Builtin.Names`?  Because the global name cache maintained by the renamer is initialised with all the known-key names.  This is done by the (hard-to-find) function `Main.newHscEnv`:

```haskell
newHscEnv :: DynFlags -> IO HscEnv
newHscEnv dflags
  = do { ...
         nc_var <- newIORef (initNameCache us knownKeyNames)
	 ...
	 return (HscEnv { ... hsc_NC = nc_var, ... }) }

knownKeyNames :: [Name]
knownKeyNames = map getName wiredInThings ++ basicKnownKeyNames ++ templateHaskellNames
```


Notice that the initialisation embraces both the wired-in and ("basic") known-key names.


## `Orig` `RdrName` things



An **Orig RdrName thing** has a top-level definition of a `RdrName`, using the `Orig` constructor.  Here, the baked-in information is:

- Its defining `Module`
- Its `OccName`


Again, almost all of these are in [compiler/GHC/Builtin/Names.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Names.hs).
Example: `GHC.Builtin.Names.not_RDR :: RdrName`.
