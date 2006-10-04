# Interface files


An **interface file** supports separate compilation by recording the information gained by compiling `M.hs` in its interface file `M.hi`.  Morally speaking, the interface file `M.hi` is part of the object file `M.o`; it's like a super symbol-table for `M.o`.


Interface files are kept in binary, GHC-specific format.  The format of these files changes with each GHC release, but not with patch-level releases.  The contents of the interface file is, however, completely independent of the back end you are using (`-fviaC`, `-fasm`, `-fcmm` etc).


Although interface files are kept in binary format, you can print them in human-readable form using the command:

```wiki
  ghc --show-iface M.hi
```


This textual format is not particularly designed for machine parsing.  Doing so might be possible, but if you want to read GHC interface files you are almost certainly better off using the [GHC API](commentary/compiler/api) to do so.


Here are some of the things stored in an interface file `M.hi`

- A list of what `M` exports.
- The types of exported functions, definition of exported types, and so on.
- Version information, used to drive the smart recompilation checker.
- The strictness, arity, and unfolding of exported functions.  This is crucial for cross-module optimisation; but it is only included when you compile with `-O`.


The contents of an interface file is the result of serialising the **`IfaceSyn`** family of data types.  The data types are in [compiler/iface/IfaceSyn](/trac/ghc/browser/ghc/compiler/iface/IfaceSyn) and [compiler/iface/IfaceType](/trac/ghc/browser/ghc/compiler/iface/IfaceType); the binary serialisation code is in [compiler/iface/BinIface](/trac/ghc/browser/ghc/compiler/iface/BinIface).
