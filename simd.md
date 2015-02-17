# SIMD instructions in GHC


The goal of the SIMD project is to allow GHC and Haskell libraries to take advantage of SIMD vector instructions. Please see more information on

- [design](simd/design)
- [implementation status](simd/implementation/status)

## Building the SIMD branch


SIMD support currently requires a version of ghc built from the simd branch. Vectors primops only work when compiling with `-fllvm`. You will also need patched versions of the `dph` and `vector` libraries. Here are the steps to get the SIMD branch up and running.

1. Set up a ghc tree as you normally would. See [Building/GettingTheSources](building/getting-the-sources). Be sure to checkout out the `simd` branch, e.g.,

```wiki
$ git clone -b simd http://git.haskell.org/ghc.git
```

1. Replace `libraries/dph` with the simd branch checked out from `git@github.com:mainland/dph.git`.

1. Replace `libraries/vector` with the simd branch checked out from `git@github.com:mainland/vector.git`.

1. Use the `fingerprint.py` script to set your repository state to match the simd branch. The fingerint file is located in the github repository `mainland/ghc-simd-test`[ here](https://raw.github.com/mainland/ghc-simd-tests/master/simd.fp).

```wiki
$ ./utils/fingerprint/fingerprint.py restore -f simd.fp
```

1. Configure and build as you would normally (see Building), but choose the `perf-llvm``BuildFlavour` in your `mk/build.mk` file.

# Obsolete sub-topics

- An [implementation plan](simd/implementation/plan).
- Manuel's notes on [implementing SIMD support in GHC using LLVM](simd/implementation/llvm).
- An [old implementation](simd/implementation/old) of the very beginnings of SIMD support in GHC.
- An example of vector operations in [LLVM IL](simd/llvm-example).
