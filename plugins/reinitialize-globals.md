## Status


After a few unfortunately public iterations, I've pushed Solution 2 to HEAD. It's a lightweight solution, re-uses an existing mechanism, has a small footprint in the GHC source code, and is totally transparent to the plugin author.

## Background

### global variables used in GHC


I performed a bunch of greps in search of global variables in the code base:

```wiki
# find possible top-level declarations of an IORef, MVar, some sort of pointer, global
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *IORef' {} /dev/null \;
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *MVar' {} /dev/null \;
$ find .. -type f -exec grep -nHE -e '^[^ ].*:: *[^ ]*Ptr' {} /dev/null \;
$ find .. -type f -exec grep -nHw -e global {} /dev/null \;
```


(also for `unsafe[^ ]*IO`, `inlinePerformIO`, and `unsafeInterleaveM`)


Manually combing the results, I found these legitimate hits:

- these three modules use the GLOBAL_VAR macro and were already supported by reinitializeGlobals: `StaticFlags`, `DynFlags`, `Linker`

- my focus: `FastString.string_table`

- I don't know what these are for: `Panic.interruptTargetThread`, `InteractiveEval.noBreakStablePtr`

### `CoreMonad.reinitializeGlobals`


When the host compiler is a statically linked executable, the host compiler and the set of all plugins each have distinct copies of global variables.  Unless someone really goes out of their way, all plugins will share the same copy, so there are at most two copies: one for the plugins and one for the host compiler.


The current workaround is `CoreMonad.reinitializeGlobals`, which every plugin is supposed to call at the beginning of its `install` routine.  This function overwrites the plugin's global variables with the corresponding values of the host compiler's. It requires a little plumbing to make this work but not much, and the plugin author sees only `reinitializeGlobals`. The long-term plan is to eventually totally avoid having two separate images of the ghc library and then redefine `reinitializeGlobals = return ()`.


So the plugin author is instructed to write this:

```wiki
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals
  …
```


This mechanism is currently used for global variables in the `StaticFlags`, `Linker`, and `DynFlags` modules.


This mechanism is insufficient for the `FastString.string_table` global variable because `reinitializeGlobals` only supports copying the host compiler's global variables into the plugin's global variables *exactly once* (per loaded plugin) at the beginning of the Core simplifier. This seems sufficient for the globals already supported by `reinitializeGlobals`, but `FastString.string_table` needs more, since the plugin may mutate the string table and those mutations should be propagated back into the compiler's instance of the variable.


Furthermore, since the `FastString` interface uses `unsafePerformIO`, the two images' `FastString.string_table`s may get out of synch when the evaluation of a thunk mutates one of the tables but not the other. I'm going to call any thunk that allocates a `FastString` when its forced a "problem thunk".  Since we can only easily synchronize the two images' tables when control is passed between the compiler and a plugin, evaluation of a problem thunk by an image that did not create that thunk is problematic.


Conclusion: `reinitializeGlobals` is insufficient for `FastString.string_table` and any other global variable that would require non-trivial synchronization between the compiler and the plugins.

### `FastString.string_table`


I'd like to let plugins correctly use this variable, since that would let them invoke some parts of the front-end (eg resolving `RdrName`s).


All the `FastString`s created during compilation are memoized in a hash table. For speedy comparison, each string is associated with a unique, which is allocated linearly whenever a `FastString` is created that has no corresponding entry in the hash table. This involves two pieces of global state, which are held in the same global variable.

```wiki
data FastStringTable =
  FastStringTable
     {-# UNPACK #-} !Int
     (MutableArray# RealWorld [FastString])
 
{-# NOINLINE string_table #-}
string_table :: IORef FastStringTable
```


During its use, the `FastString` table increments the `!Int` argument. `reinitializeGlobals` alone is incapable of supporting this appropriately; it was designed to only copy global variables' values from the host compiler to the plugin, never in the opposite direction. Those original global variables were global for convenience of access, not for the need to be mutable. The `FastString` table breaks the mold.


It's straight-forward to have the two images share the array, but it is difficult to keep the two images' values of `Int` in synch.  The danger is that the two images could allocate the same unique for distinct `FastString`s — that'd break a major invariant.

## Solutions

### Solution 1: the `Globals.c` mechanism


Simon Marlow said:

>
> "I haven't been following this in detail, but I think you're encountering the same problem we had with various top-level IORefs in the base package.  The solution we have there is grotesque but it works.  Take a look at libraries/base/GHC/Conc/Signal.hs, search for getOrSetGHCConcSignalSignalHandlerStore.  There is some corresponding RTS gunk to help with this in rts/Globals.c."


This workaround keeps a table of `StgStablePtr`s in the RTS for a fixed set of symbols (that's managed by `rts/Globals.c`). That table is accessed via C functions named with the scheme `getOrSet<key>`. So we add one such function there (and in `includes/rts/Globals.h`: `getOrSetLibHSghcFastStringTable`.


The mechanism is invoked thusly:

```wiki
{-# NOINLINE string_table #-}
string_table :: IORef FastStringTable
string_table =
 unsafePerformIO $ do
   tab <- IO $ \s1# -> case newArray# hASH_TBL_SIZE_UNBOXED [] s1# of
                           (# s2#, arr# #) ->
                               (# s2#, FastStringTable 0 arr# #)
   ref <- newIORef tab
   sharedCAF ref getOrSetLibHSghcFastStringTable

foreign import ccall unsafe "getOrSetLibHSghcFastStringTable"
  getOrSetLibHSghcFastStringTable :: Ptr a -> IO (Ptr a)
```


Thus there ever exists only one such CAF per process, regardless of how many copies of libHSghc are loaded, since they all share the first such CAF forced. This is arbitrated by the process's sole image of the RTS.

**Concerns**


My only concern is that I imagine we would like to keep the set of pointers maintained by Globals.c to a minimum?

### Solution 2: use a dynamically linked compiler


Ian Lynagh asks "Why not just us a dynamically linked compiler?"


If the ghc executable itself dynamically links against libHSghc, then the entire `reinitializeGlobals` mechanism is unnecessary! In that case, both the host compiler and its plugins link against the same dynamic instance of libHSghc, which contains the sole set of mutable global variables.


The `DYNAMIC_GHC_PROGRAMS` variable in the GHC build system determines this.  As of commit b7126674 (\~mid-March 2013), the ghc executable is dynamically linked by default (except on Windows).  This snippet from `mk/config.mk` shows the default behavior as of [163de25813d12764aa5ded1666af7c06fee0d67e](/trac/ghc/changeset/163de25813d12764aa5ded1666af7c06fee0d67e/ghc) (\~July 2013).

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

**Concerns**


The rule would be: if you want to use a plugin (that uses any of the compiler's global variables), you must use a dynamically-linked compiler.


The repercussions of this rule are not totally apparent to me.  Plugins themselves are already dynamically loaded, so the platform certainly already supports dynamic libraries.  So I think the only burden on the plugin user is having to ensure that their GHC is dynamically linked.


Is GHC HQ planning on distributing dynamically linked compilers when possible? What about the Haskell Platform?


If `reinitializeGlobals` is no longer needed, what about the `Globals.c` mechanism? How likely is it that an image statically linked against the base library will end up dynamically loading the base library? In other words, besides GHC and GHCI, will extensible Haskell programs compiled by GHC ever contain a statically linked-in base library?
