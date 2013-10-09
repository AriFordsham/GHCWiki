# Problem statement


Currently annotations are only usable from the GHC API.  Our goal is to add support for [TemplateHaskell](template-haskell) too.  Both creation of annotations, reification of annotations and gathering module annotations transitively from our whole import tree should be added.

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

# Current implementation with typeclassses


How is this implemented in HFlags currently?  By using typeclasses.


The FlagData ([ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L129](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L129)) datatype contains all the information we need to know about a flag.  Then for every flag we create a new fake datatype that implements the Flag class ([ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L149](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L149)).  In `initHFlags` we simply call template haskell reify on the Flag class.  This gives us our "fake" instances and their `getFlagData` method can be used to query the needed flag data for `--help` generation, parsing, etc.  This can be seen in at [ https://github.com/errge/hflags/blob/v0.4/HFlags.hs\#L397](https://github.com/errge/hflags/blob/v0.4/HFlags.hs#L397).


This is ugly: we are abusing the reification of types and instances to send messages to ourselves between modules.  There should be an explicit way to do that.  This is requested in [\#7867](https://gitlab.haskell.org//ghc/ghc/issues/7867).


The proposal is to make it possible to generate annotations from template haskell (when defining a flag) and read them all back via template haskell (in `$initHFlags`).  These module level annotations (in HFlags case) will then contain the info that is needed for flag parsing and `--help` generation.

## Aside: with the current GHC, this implementation is not just ugly, but broken


Unfortunately, the current idea is not really working all that nice, because of [\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426).


Haskell98 and Haskell prime says that all the instances should be visible that are under the current module in the dependency tree, but this is not the case currently in GHC when using one-shot compilation (`ghc -c`, not `ghc --make`).  This is a optimization, so we can save on the reading of all the module dependencies transitively.  GHC tries to cleverly figure out where to find so called orphan instances.


Template haskell is a corner-case, where this orphan logic is not clever enough and therefore reify doesn't see some of the instances that are under the current module in the dependency tree.  Even more so, if the class instance is in a separate package (and not marked orphan, as is the case in HFlags), then it's not seen either in one-shot or in batch mode.  Therefore HFlags can't gather all the flags in `$initHFlags`.  I propose to fix this by loading all the imported interfaces transitively when reifying classes or type families.  **If you have any comments or questions regarding this, please comment on [\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426).**


An alternative approach would be to mark all the modules that define command line flags as orphan modules.  Then they are automatically read up with the current one-shot and batch compiler and seen in reification.  To do this it has to be possible to mark modules as orphans, as proposed by [\#8337](https://gitlab.haskell.org//ghc/ghc/issues/8337) or agree on a new orphan rule: e.g. every module that contains module level annotations are orphan.  If we agree on the latter, then the patch in [\#8337](https://gitlab.haskell.org//ghc/ghc/issues/8337) has to be sightly modified to work like that.  **Please comment here or on [\#8337](https://gitlab.haskell.org//ghc/ghc/issues/8337) if you prefer either of the two solutions, or object to both.**

# Implementation status, options, questions

## Already done: a bugfix, and annotation generation


The already merged [\#3725](https://gitlab.haskell.org//ghc/ghc/issues/3725) and [\#8340](https://gitlab.haskell.org//ghc/ghc/issues/8340) makes it possible to generate annotations from TH.  We support all three kind of annotations: annotations on types, values and whole modules.

## In the works, but seems easy: annotation reification

[\#8397](https://gitlab.haskell.org//ghc/ghc/issues/8397) contains the code for reification of annotations.  The patch is quite straightforward and quite separated from all of the other TODOs, it just adds a new Q monad statement for handling the reification request and the necessary handling of this Q monad statement in `TcSplice`.  **I consider this patch ready to review, so please comment.**

## Controversial: [\#8398](https://gitlab.haskell.org//ghc/ghc/issues/8398), module list reification


The [\#8397](https://gitlab.haskell.org//ghc/ghc/issues/8397) from the previous paragraph is already useful on its own. Unfortunately, for HFlags purposes it's not enough: it only make it possible to get annotations for modules, values or types when you already know your target.  But in the case of `$initHFlags` we want to get all the flags that were defined somewhere below us in our import tree.  To do this, [\#8398](https://gitlab.haskell.org//ghc/ghc/issues/8398) implements module listing.  Once we get back the module list, we can use the returned names to get the module annotations for all the modules and extract the flag data.


The current patch is a bit controversial, it simply uses the currently loaded list of modules as a substitute for "module list of our dependencies".  This approximation is heavily used currently everywhere in GHC, but it can cause problems, as showed in [\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426).  SPJ already stated ([ http://ghc.haskell.org/trac/ghc/ticket/7867\#comment:12](http://ghc.haskell.org/trac/ghc/ticket/7867#comment:12)) that he doesn't agree to this approximation and we should work on something that is well defined.  I agree.


I propose to change this patch to properly construct the list of dependent modules and return that.  We would return `(pkgid, moduleid)` pairs for every module that is imported by us or imported by our imports transitively.

**Here for sure we need design discussion, so please think about this and share your thoughts.**  This is not to say that design discussion is not welcome for the other patches, of course it is!
