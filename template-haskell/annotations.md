# Problem statement


Currently annotations are only usable from the GHC API.  Our goal is to add support for [TemplateHaskell](template-haskell) too.  Both creation of annotations, reification of annotations and gathering module annotations transitively from our whole import tree should be added.

# Motivation


As a main use-case we will consider the [ http://hackage.haskell.org/package/hflags](http://hackage.haskell.org/package/hflags) library.


The goal of the library is to:

- provide easy to use command line flags,
- that are pure from the programmers point of view (they don't change while running the program once anyways),
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


What's important to note here is that in the main program we didn't have to specify the list of modules that has to be searched for command line flags, the template haskell `$initHFlags` function automagically found them all.  Even if they're not directly, only indirectly imported by our main program.  A motivating example for that:

- the TCP module has a connect method that accepts a command-line tcp_connect_timeout flag (so the user can change that to 5 seconds from the usual 10 hours :)),
- the HTTP module of course depends on TCP,
- the WGet module depends on HTTP,
- the main program uses WGet to download something from the internet.


In this case HFlags automatically makes it so that the tcp_connect_timeout flag is show in --help of the main program and can be changed by the user to any value she sees fits.  This is achieved via [TemplateHaskell](template-haskell), but in exchange the programmer doesn't have to explicitly pass around any kind of values or applicative stuff for every imported module that uses command line flags.


Of course, this whole approach can be debated and maybe we should instead explicitly pass parameters to functions, but let's leave that debate to the getopt authors and focus on TH on this page.

# Current implementation with typeclassses


How the magic works inside HFlags currently?  By using typeclasses.


The FlagData ([ https://github.com/errge/hflags/blob/master/HFlags.hs\#L129](https://github.com/errge/hflags/blob/master/HFlags.hs#L129)) datatype contains all the information we need to know about a flag.  Then for every flag we create a new fake datatype that implements the Flag class ([ https://github.com/errge/hflags/blob/master/HFlags.hs\#L149](https://github.com/errge/hflags/blob/master/HFlags.hs#L149)).  In `initHFlags` we simply call [TemplateHaskell](template-haskell) reify on the Flag class.  This gives us our "fake" instances and their `getFlagData` method can be used to query the needed flag data for `--help` generation, parsing, etc.  This can be seen in at [ https://github.com/errge/hflags/blob/master/HFlags.hs\#L397](https://github.com/errge/hflags/blob/master/HFlags.hs#L397).
