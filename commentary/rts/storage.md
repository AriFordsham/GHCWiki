# GHC Commentary: The Storage Manager


GHC's storage manager is designed to be quite flexible: there are a large number of tunable parameters in the garbage collector, and partly the reason for this was because we wanted to experiment with tweaking these settings in the context of Haskell.

[](/trac/ghc/attachment/wiki/Commentary/Rts/Storage/sm-top.png)

## The Block Allocator

## The Garbage Collector

### Copying Collection

### Compacting Collection