# GHC API Hooks


This document describes a proposal for a **hooks** interface to customise certain phases of GHC's source transformation facilities.  A hook is simply a callback that is called at certain points in the program.

## The Problem


The GHC API can be used for many different purposes, but some of these purposes require deviating behaviour from what GHC would normally do.  For example, if we want to use GHC as a front-end to a Haskell compiler, we may want to replace the code generator with a custom backend (e.g., a byte code format or JavaScript code).  Similarly, some tools want to inject information into the compiler (e.g., during quasi-quoting).

## Proposed Solution


One option is to split GHC's stages into lots of small function calls and allow the user to wire these stages together as needed.  Unfortunately, this is very difficult to do and wouldn't necessarily be very flexible since there are a number of expected invariants not encoded in the types.


Instead we identify "interesting" places inside the compiler and allow users of the GHC API to specify a call-back that gets invoked when execution reaches that place.  For example, instead of calling `typecheckRename` function directly, we first look up whether there is a hook specified for and if so call that function instead.  The hook may choose to perform its own renaming and type checking passes (unlikely) or call the GHC's `typecheckRename` function and inspect its output and generate additional files on disk.

## Example

```wiki

  withGhc libdir $ do
    dflags0 <- getSessionDynFlags
    let dflags1 = dflags0{ hooks = insertHook LocateLibHook myLocateLib
                                 . insertHook LinkDynLibHook myLinkDynLibHook
                                 $ hooks dflags0 }
    setSessionDynFlags dflags1

myLocateLib :: DynFlags -> Bool -> [FilePath] -> String -> IO LibrarySpec
myLocateLib 

myLinkDynLibHook :: DynFlags -> [FilePath] -> [PackageId] -> IO ()
myLinkDynLibHook dflags paths ids = do 
```


The two functions will be called whenever GHC needs to locate or link a dynamically loaded library.

## The Hook datatype


Each hook has a potentially different type from all the other hooks. Additionally, we need to be able to communicate hooks to all the locations where they may be invoked. This is achieved by storing the list of hooks in the `DynFlags`.  This, however, means that hooks cannot be defined as an ADT, as that would lead to huge cyclic imports (the data types used by the hooks will depend on `DynFlags`, but the `DynFlags` will depend on the hook data type.  Instead we `Hooks` is an untyped key-value store.  The keys are single constructor types and the `Hooks` map is indexed by their `TypeRep`.  We recover the hook type via a type family:

```wiki
--- Implementation Sketch -----------------------
data Hook = forall a. Hook TypeRep a

type Hooks = Map TypeRep Hook

type family HookType a :: *

insertHook :: forall a. Typeable a => a -> HookType a -> Hooks -> Hooks
insertHook key value hooks =
  let hook = Hook (typeOf key) value in
  Map.insert (typeOf key) hook hooks

lookupHook :: forall a. Typeable a => Hooks -> Maybe (HookType a)
lookupHook hooks =
  let key = typeOf (undefined :: a) in
  case Map.lookup key of
     Nothing -> Nothing
     Just (Hook _ h) -> Just (unsafeCoerce h :: HookType a)  -- the tricky bit
```

```wiki
-- Defining a hook type:
data LocateLibHook = LocateLibHook deriving Typeable
type instance HookType LocateLibHook = DynFlags -> Bool -> [FilePath] -> String -> IO LibrarySpec
```

## TODO List all currently available hooks