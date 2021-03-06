# Idiom: standard targets (all, clean, etc.)


We want an `all` target that builds everything, but we also want a way to build individual components (say, everything in `rts/`).  This is achieved by having a separate "all" target for each directory, named `all_`*directory*.  For example in `rts/ghc.mk` we might have this:

```wiki
all : all_rts
.PHONY all_rts
all_rts : ...dependencies...
```


When the top level **make** includes all these `ghc.mk` files, it will see that target `all` depends on `all_rts, all_ghc, ...etc...`; so `make all` will make all of these.  But the individual targets are still available.  In particular, you can say

- `make all_rts` (anywhere) to build everything in the RTS directory
- `make all` (anywhere) to build everything
- `make`, with no explicit target, makes the default target in the current directory's stub `Makefile`, which in turn makes the target `all_`*dir*, where *dir* is the current directory.


Other standard targets such as `clean`, `install`, and so on use the same technique.  There are pre-canned macros to define your "all" and "clean" targets, take a look in `rules/all-target.mk` and `rules/clean-target.mk`.


These targets also work for the libraries, eg `make all_libraries/ghc-prim_dist-install`.  This target is parsed as make `all` in `libraries/ghc-prim` for the `dist-install` build. Some libraries also have a `dist-boot` build.


In order for `make` to be able to find your new directory and apply the rules to it you need to add it to the `BUILD_DIRS` list in the top-level `ghc.mk`:

```wiki
BUILD_DIRS += rts
```


Without this entry calling **make** will not allow it to find a `target` for the new entry.
