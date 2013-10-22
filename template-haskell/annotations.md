# Problem statement


Currently [wiki:/Annotations](annotations) are only usable from the GHC API.  Our goal is to add support for [TemplateHaskell](template-haskell) too.  Both creation of annotations, reification of annotations and gathering module annotations transitively from our whole import tree should be added.

# Motivation


As a main use-case we will consider the [ http://hackage.haskell.org/package/hflags](http://hackage.haskell.org/package/hflags) library.


The goal of the library is to:

- provide easy to use command line flags,
- that are pure from the programmers point of view (they don't change during one program run),
- that can be defined anywhere, even in a module,
- and the used flags are automatically gathered in the main program for parsing and `--help` generation.


Example:

- `A.hs`:

  ```
  {-# LANGUAGE TemplateHaskell #-}moduleA(verbose)whereimportHFlagsdefineFlag"verbose"True"Whether debug output should be printed."verbose::Boolverbose= flags_verbose
  ```
- `ImportExample.hs`

  ```
  {-# LANGUAGE TemplateHaskell #-}importHFlagsimportControl.Monad(when)importqualifiedAmain=do_<-$initHFlags "Importing example v0.1"
            when A.verbose $ putStrLn "foobar"
  ```


Let's try it out:

```wiki
errge@curry:/tmp/y $ runhaskell ImportExample.hs  --help
Importing example v0.1

  -h  --help, --usage, --version  Display help and version information.
      --undefok                   Whether to fail on unrecognized command line options.
      --verbose[=BOOL]            Whether debug output should be printed. (default: true, from module: A)
errge@curry:/tmp/y $ runhaskell ImportExample.hs  --verbose=True
foobar
errge@curry:/tmp/y $ runhaskell ImportExample.hs  --verbose=False
errge@curry:/tmp/y $
```


What's important to note here is that in the main program we didn't have to specify the list of modules that has to be searched for command line flags, the template haskell `$initHFlags` function automatically found them all.  Even if they are not imported directly, but only indirectly by our main program.  A motivating example for that:

- the TCP module has a connect method that accepts a command-line `tcp_connect_timeout` flag (so the user can change that to 5 seconds from the usual 10 hours :)),
- the HTTP module of course depends on TCP,
- the WGet module depends on HTTP,
- the main program uses WGet to download something from the internet.


In this case HFlags automatically makes it so that the `tcp_connect_timeout` flag is show in `--help` of the main program and can be changed by the user to any value she sees fit.  This is achieved via template haskell, but in exchange the programmer doesn't have to explicitly pass around any kind of values or applicative stuff for every imported module that uses command line flags.


Of course, this whole approach can be debated and maybe we should instead explicitly pass parameters to functions; but let's leave that debate to the getopt authors and focus on TH on this page.

---

# Current implementation with typeclassses


How is this implemented in HFlags currently?  By using typeclasses.


The FlagData ([ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L129](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L129)) datatype contains all the information we need to know about a flag.  Then for every flag we create a new fake datatype that implements the Flag class ([ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L149](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L149)).  In `initHFlags` we simply call template haskell reify on the Flag class.  This gives us our "fake" instances and their `getFlagData` method can be used to query the needed flag data for `--help` generation, parsing, etc.  This can be seen in at [ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L397](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L397).


This is ugly: we are abusing the reification of types and instances to send messages to ourselves between modules.  There should be an explicit way to do that.  This is requested in [\#7867](https://gitlab.haskell.org//ghc/ghc/issues/7867).

## Aside: with the current GHC, this implementation is not just ugly, but broken


Unfortunately, the current idea is not really working all that nice, because of [\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426).


Haskell98 and Haskell prime says that all the instances should be visible that are under the current module in the dependency tree, but this is not the case currently in GHC when using one-shot compilation (`ghc -c`, not `ghc --make`).  This is a optimization, so we can save on the reading of all the module dependencies transitively.  GHC tries to cleverly figure out where to find so called orphan instances.


Template haskell is a corner-case, where this orphan logic is not clever enough and therefore reify doesn't see some of the instances that are under the current module in the dependency tree.  Even more so, if the class instance is in a separate package (and not marked orphan, as is the case in HFlags), then it's not seen either in one-shot or in batch mode.  Therefore HFlags can't gather all the flags in `$initHFlags`.  There is a fix to this as a patch in [\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426), but that needs more discussion.


An easier way is to implement [\#1480](https://gitlab.haskell.org//ghc/ghc/issues/1480), module reification.  If we can get the import list of every module, then HFlags can walk the tree of imports itself and gather all the flags.  The nice in this is that the compiler only needs very basic and simple support, and then the logic of traversal can be implemented in HFlags, not in the compiler.solutions, or object to both.****

---

# Design proposal


The proposal is to make it possible to generate annotations from template haskell (when defining a flag) and read them all back via template haskell (in `$initHFlags`).  These module level annotations (in HFlags case) will then contain the info that is needed for flag parsing and `--help` generation.


Specifically, we propose to add the following new function to the `Quasi` class:

```wiki
class Quasi where 
  ...please fill in...
```


These functions behave as follows:


... Fill in...

## Example


Here is (a sketch of) how we can use these new facilities to implement `defineFlag` and `$initHFlags` in the above example.


...fill in...

---

# Implementation status, options, questions

## Already done: a bugfix, and annotation generation and reification


The already merged [\#3725](https://gitlab.haskell.org//ghc/ghc/issues/3725) and [\#8340](https://gitlab.haskell.org//ghc/ghc/issues/8340) makes it possible to generate annotations from TH.  We support all three kind of annotations: annotations on types, values and whole modules.


Annotation reification is implemented and merged in [\#8397](https://gitlab.haskell.org//ghc/ghc/issues/8397).

## Patch ready, nice to have: typed annotation reification

[\#8460](https://gitlab.haskell.org//ghc/ghc/issues/8460) provides a very small addition that makes it possible to use annotation reification together with the new typed [TemplateHaskell](template-haskell).


This patch doesn't need to get merged urgently, it's just nice to have.

## Waiting for review, hopefully to still go into 7.8.1: module reification, [\#1480](https://gitlab.haskell.org//ghc/ghc/issues/1480)


The only feature that is still not in GHC and needed for HFlags is a way to walk the module dependency tree of the currently being compiled module from TH.  This is made possible by [\#1480](https://gitlab.haskell.org//ghc/ghc/issues/1480), that just adds minimal module reification (import list).  Once we have that, HFlags can just ask for the imports and than for the imports of the imports, etc. to walk the tree itself.  There is no need to do this from the compiler, as that would obscure the fact that this can be a slow and wasteful operation.

**If you have any opinion about this minimal module reification, please comment on and review [\#1480](https://gitlab.haskell.org//ghc/ghc/issues/1480).**

# Other related tickets

- [\#8398](https://gitlab.haskell.org//ghc/ghc/issues/8398)