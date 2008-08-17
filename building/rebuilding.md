# How do I rebuild GHC after updating or changing it?


Note that after pulling new changes, the safest way to continue working is to clean the build tree and start from scratch.


In fact, it's best to clean the tree *before* pulling, because the new code may not know how to clean the old build tree:

```wiki
  $ make distclean
```


Next you need to pull any new patches:

```wiki
  $ sh darcs-all pull
```


But there may have been new subrepos added to the build, so you need to grab them:

```wiki
  $ sh darcs-all get
```


Now build the tree as normal:

```wiki
  $ sh boot
  $ ./configure
  $ make
```


You may be able to shortcut this procedure if you know what you're doing, or you could just take a risk and if anything goes wrong fall back to the above procedure.
