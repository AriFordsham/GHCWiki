## Status


I've pushed Option 2 to HEAD. Since then, I've realized the troubles with laziness are more delicate than I thought, so I came up with Options 5 and 6.

- I think Option 3 is the most robust, but I don't know how to pull it off.

- Option 6 sounds best after that, but it was a late idea and I'm not sure how robust the underlying mechanism is.

- Option 5 avoids the laziness issues, but we'll have to ensure it doesn't adversely affect performance too much — `FastString` has some hot spots.

## Background

### `CoreMonad.reinitializeGlobals`


For unfortunate reasons I don't fully understand ([\#5355](https://gitlab.haskell.org//ghc/ghc/issues/5355), [\#5292](https://gitlab.haskell.org//ghc/ghc/issues/5292)), the host compiler and the set of all plugins each have distinct copies of global variables (unless the host compiler dynamically loads libHSghc, eg on Windows; see the next section).  All plugins share the same copy, so there are at most two copies. The current workaround is `CoreMonad.reinitializeGlobals`, which every plugin is supposed to call at the beginning of its `install` routine.  This function overwrites the plugin's global variables with the corresponding values of the host compiler's. It requires a little plumbing to make this work but not much, and the plugin author sees only `reinitializeGlobals`.


The long-term plan is to eventually totally avoid having two separate images of the ghc library and then redefine `reinitializeGlobals = return ()`.


So the plugin author is instructed to write this:

```wiki
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals
  …
```


This mechanism is currently used for some `StaticFlag`s, some `Linker` state, and some `DynFlags`. I just recently added (partial) support for the `FastString` table.

### `DYNAMIC_GHC_PROGRAMS`


If the ghc executable itself dynamically links against libHSghc, then the entire `reinitializeGlobals` mechanism is unnecessary! In that case, both the host compiler and its plugins link against the dynamic libHSghc, which contains the sole set of mutable global variables.


As of commit b7126674 (\~mid-March 2013), the ghc executable dynamically loads libHSghc by default. This snippet from `mk/config.mk` shows the default behavior as of [163de25813d12764aa5ded1666af7c06fee0d67e](/trac/ghc/changeset/163de25813d12764aa5ded1666af7c06fee0d67e/ghc) (\~July 2013).

```wiki
# Use the dynamic way when building programs in the GHC tree. In
# particular, this means that GHCi will use DLLs rather than loading
# object files directly.
ifeq "$(TargetOS_CPP)" "mingw32"                     # <---- this means Windows
# This doesn't work on Windows yet
DYNAMIC_GHC_PROGRAMS = NO
else ifeq "$(TargetOS_CPP)" "freebsd"
# FreeBSD cannot do proper resolution for $ORIGIN (due to a bug in
# rtld(1)), so disable it by default (see #7819).
DYNAMIC_GHC_PROGRAMS = NO
else ifeq "$(PlatformSupportsSharedLibs)" "NO"
DYNAMIC_GHC_PROGRAMS = NO
else
DYNAMIC_GHC_PROGRAMS = YES
endif
```


NB also that the `*-llvm` presets in `build.mk` set `DYNAMIC_GHC_PROGRAMS = NO` as of [163de25813d12764aa5ded1666af7c06fee0d67e](/trac/ghc/changeset/163de25813d12764aa5ded1666af7c06fee0d67e/ghc).

### `FastString.string_table`


All the `FastString`s created during compilation are memoized in a hash table. For speedy comparison, each string is associated with a unique, which is allocated linearly whenever a `FastString` is created that has no corresponding entry in the hash table. This involves two pieces of global state, which are held in the same global variable.

```wiki
data FastStringTable =
  FastStringTable
     {-# UNPACK #-} !Int
     (MutableArray# RealWorld [FastString])
 
{-# NOINLINE string_table #-}
string_table :: IORef FastStringTable
```

## The Problem with `FastString.string_table`


During its use, the `FastString` table increments the `!Int` argument. `reinitializeGlobals` alone is incapable of supporting this appropriately; it was designed to only copy global variables' values from the host compiler to the plugin, never in the opposite direction. Those original global variables were global for convenience of access, not for the need to be mutable. The `FastString` table breaks the mold.


It's straight-forward to have the two images share the array, but it is difficult to keep the two images' values of `Int` in synch.  The danger is that the two images could allocate the same unique for distinct `FastString`s — that'd break a major invariant.

### Option 1: The General Solution


Ideally, we would extend the `reinitializeGlobals` mechanism to additionally support information flow from the plugin back to the host compiler. However, it's a high priority that this whole mechanism remain as transparent as possible: we'd really like to just rip it out in the future without breaking any code.


Unfortunately, the current interface — just calling `reinitializeGlobals` at the top of `install` — is insufficient for this reverse information flow. We'd have to change the interface to something like:

```wiki
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = workaroundGlobals $ do
  …
```


This function would do three things:

>
> 1) call `reinitializeGlobals` ASAP, just like now

>
> 2) call `reverseReinitializeGlobals` at the end of the `install` routine; this new function would stash the values of the plugin's globals in a new, otherwise unused Writer output of `CoreM`.

>
> 3) similarly wrap every `PluginPass` contained in the result of `install`


We'd also have to add some logic around the host compiler's calls to the plugin in order to copy the stashed values back into the host compiler's globals.


(This suffers from the same laziness issues as Option 2 below, so check that out too.)


Overall, this seems like overkill. I performed a bunch of greps in search of global variables in the code base:

```wiki
# find possible top-level declarations of an IORef, MVar, some sort of pointer, global
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *IORef' {} /dev/null \;
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *MVar' {} /dev/null \;
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *[^ ]*Ptr' {} /dev/null \;
$ find .. -type f -exec grep -nHw -e global {} /dev/null \;
```


(also for `unsafe[^ ]*IO`, `inlinePerformIO`, and `unsafeInterleaveM`)


Legitimate hits:

- these three use the GLOBAL_VAR macro and were already supported by reinitializeGlobals: `StaticFlags`, `DynFlags`, `Linker`

- my focus: `FastString.string_table`

- I don't know what these are for: `Panic.interruptTargetThread`, `InteractiveEval.noBreakStablePtr`


Of all these global variables, I think only `string_table` needs information flow from the plugin back to the compiler.

### Option 2: The Lighterweight Workaround


For `FastString.string_table`, I think we can avoid changing the `reinitializeGlobals` interface. In this case, we can recover the appropriate Int value by scanning the hash table every time we return from the plugin: the Int is just a cache of the table's size (…right?).


This wouldn't affect the plugin API, but it would still require the extra logic around the compiler's calls to plugins.


Unfortunately, since the `FastString` interface uses unsafePerformIO, the two images' `FastString` tables may get out of synch when the evaluation of one of those thunks mutates one of the tables but not the other. Option 1 has this same problem. I'm going to call any thunk that allocates a `FastString` when its forced a "problem thunk". Since we only synchronize the two images' tables when transitioning between the compiler and a plugin, evaluation of a problem thunk by the image that did not create it is problematic.


So the rule would be: *Do not let the compiler force any of a plugin's thunks that allocate `FastString`s, and vice versa.* (Same for Option 1.)

### Option 3: The Full Low-Level Hack


Can we do some dirty low-level pointer copying so that the plugin's `FastString.string_table` CAF is a `IND_STATIC` pointing to the compiler's `FastString.string_table` CAF?


This would be the least invasive approach wrt the GHC source code by far and it would have no rules for the plugin author to worry about.

### Option 4: Do Nothing


For `FastString.string_table`, the lack of reverse information flow probably doesn't matter. I suspect the most common case just involves looking up `FastString`s in the table, not actually creating new ones.


Even if the plugin does create `FastString`s, it looks like the only thing that is downstream from the core2core pipeline and also is (indirectly) sensitive to a `FastString`'s unique is GHCI. Everything else downstream of the plugins just uses the unique associated with Names — `OccString`s are pretty much ignored after the renamer.


So the rule would be: *if your plugin might allocate new `FastString`s, be warned that GHCI might not work quite right*.

### Option 5: Circular `IORef`s


This is idea is predicated on the fact that there are at most two libHSghc images in memory.


We could change `FastStringTable` to the following.

```wiki
data Maybe_FST = Nothing_FST | Just_FST !(IORef FastStringTable)

data FastStringTable =
  FastStringTable
     {-# UNPACK #-} !Int
     (MutableArray# RealWorld [FastString])
     {-# UNPACK #-} !Maybe_FST
```


Semantics: The new field points at the other image's `string_table`; `reinitializeGlobals` would setup this circularity. When updating the `Int` field of one, we would also update the `Int` field of the other, via the new field. We wouldn't need to duplicate any work for the array, since that pointer is shared directly after `reinitializeGlobals`.


Performance: I'm hoping pointer tagging, unpacking (will this sum type be unpacked?), and branch prediction will ameliorate the extra instructions, especially when there is no plugin.  Moreover, this extra work would only happen when allocating new `FastStrings`, which is relatively infrequent.


This Option does not have any laziness issues. Thunks that end up adding `FastString`s to the table, when forced, always begin by reading the `string_table``IORef`.  Thus, they will see the `Just_FST` if they're forced after `reinitializeGlobals`, regardless of when those thunks were created.  In other words, thunks only cache the `IORef`, not its contents. Since each image's `IORef`'s contents now includes a reference to the other image's `IORef`, the thunks will mutate both tables in synch.

### Option 6: Use `UniqSupply` in `FastString`


Instead of allocating `FastString`s' uniques linearly, let's use a `UniqSupply`. Then we'd just need to split the supply in order to prevent the danger of two `FastString`s getting the same unique. Whatever routine does the splitting just needs to be called the first time libHSghc is loaded dynamically and never again.
