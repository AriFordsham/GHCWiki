## Background

### CoreMonad.reinitializeGlobals


For unfortunate reasons I don't fully understand ([\#5355](https://gitlab.haskell.org//ghc/ghc/issues/5355), [\#5292](https://gitlab.haskell.org//ghc/ghc/issues/5292)), the host compiler and plugins have distinct copies of global variables. The current workaround is `CoreMonad.reinitializeGlobals`, which every plugin is supposed to call at the beginning of its `install` routine. This function overwrites the plugin's global variables with the corresponding values of the host compiler's. It requires a little plumbing to pull make this work but not much, and the plugin author sees only `reinitializeGlobals`.


The long-term plan is to eventually totally avoid having two separate images of the ghc library and then redefine `reinitializeGlobals = return ()`.


So the plugin author is instructed to write this:

```wiki
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals
  …
```


This mechanism is currently used for some StaticFlags, some Linker state, and some DynFlags. I just recently added (partial) support for the FastString table.

### `FastString.string_table`


All the FastStrings created during compilation are memoized in a hash table. For speedy comparison, each string is associated with a unique, which is allocated linearly whenever a FastString is created that has no corresponding entry in the hash table. This involves two pieces of global state, which are held in the same global variable.

```wiki
data FastStringTable =
  FastStringTable
     {-# UNPACK #-} !Int
     (MutableArray# RealWorld [FastString])
 
{-# NOINLINE string_table #-}
string_table :: IORef FastStringTable
```

## The Problem with `FastString.string_table`


During its use, the FastString table increments the `!Int` argument. `reinitializeGlobals` alone is incapable of supporting this appropriately; it was designed to only copy global variables' values from the host compiler to the plugin, never in the opposite direction.

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
> 2) call `reverseReinitializeGlobals` at the end of the `install` routine; this new function would stash the values of the plugin's globals in a new, otherwise unused Writer output of !CoreM.

>
> 3) similarly wrap every PluginPass contained in the result of `install`


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

- these three use the GLOBAL_VAR macro and were already supported by reinitializeGlobals: StaticFlags, DynFlags, Linker

- my focus: FastString.string_table

- I don't know what these are for: Panic.interruptTargetThread, InteractiveEval.noBreakStablePtr


Of all these global variables, I think only string_table needs information flow from the plugin back to the compiler.

### Option 2: The Lighterweight Workaround


For `FastString.string_table`, I think we can avoid changing the `reinitializeGlobals` interface. In this case, we can recover the appropriate Int value by scanning the hash table every time we return from the plugin: the Int is just a cache of the table's size (…right?).


This wouldn't affect the plugin API, but it would still require the extra logic around the compiler's calls to plugins.


Unfortunately, the FastString interface uses unsafePerformIO, so unless the plugin forces all of its FastStrings before returning to the compiler, this wouldn't fix the issue. (Option 1 has this same problem.)


So the rule would be: *if your plugin might allocate new FastStrings, be sure to force them before returning to the compiler*. (Same for Option 1.)

### Option 3: The Full Low-Level Hack


Can we do some dirty low-level pointer copying so that the plugin's `FastString.string_table` CAF is a `IND_STATIC` pointing to the compiler's `FastString.string_table` CAF?


This would be the least invasive approach wrt the GHC source code by far and it would have no rules for the plugin author to worry about.

### Option 4: Do Nothing


For `FastString.string_table`, the lack of reverse information flow probably doesn't matter. I suspect the most common case just involves looking up FastStrings in the table, not actually creating new ones.


Even if the plugin does create FastStrings, it looks like the only thing that is downstream from the core2core pipeline and also is (indirectly) sensitive to a FastString's unique is GHCI. Everything else downstream of the plugins just uses the unique associated with Names — OccStrings are pretty much ignored after the renamer.


So the rule would be: *if your plugin might allocate new FastStrings, be warned that GHCI might not work quite right*.
