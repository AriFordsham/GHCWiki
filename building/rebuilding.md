# How do I rebuild GHC after updating or changing it?


To pull new changes, use the [sync-all script](building/sync-all#pulling-new-patches).


After pulling changes, the following sequence should be enough to update your build:

```wiki
$ perl boot
$ ./configure
$ make
```


Don't forget to add any necessary flags to `./configure` if you need them.

## Things that might go wrong

- If you add or remove modules from GHC, make sure you edit `ghc.cabal.in`; see [Commentary/SourceTree](commentary/source-tree)

- When files move around in the tree, this procedure will leave old files lying around.  The build system typically won't know how to clean up the old files.  Usually they won't cause a problem, but in the event that they do it's easy enough to wipe your build tree and start afresh, especially if you use [a separate build tree](building/using#source-trees-and-build-trees).
