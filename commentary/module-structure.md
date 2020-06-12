# Module Structure of GHC

This page summarises the overall structure of the GHC [compiler](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler) (exclude the runtime system).


## Compiler modules

Here is a block diagram of its top-level structure:

![ghc_modules.svg](uploads/db1b2a8fa23301b897a3e6f16dcefbab/ghc_modules.svg)

The boxes are the GHC.XXX top-level module prefixes. For example, the box `Hs` stands for a module `GHC.Hs` and a collection of sub-module (e.g. `GHC.Hs.Pat`, `GHC.Hs.Expr` etc).

The arrows indicate allowed dependencies between groups of modules.

You could browse the GHC API on [the web](https://ghc.gitlab.haskell.org/ghc/doc/) (follow the GHC API link).


## Historical note

The GHC module hierarchy was refined by Sylvain Henry. See [Make GHC codebase more modular](Make-GHC-codebase-more-modular) and [issue #13009](https://gitlab.haskell.org/ghc/ghc/issues/13009).

### Tracking old file names and module names

When you want to know the change history of modules and file names, see [Proposed renaming](https://gitlab.haskell.org/ghc/ghc/issues/13009#proposed-renaming) or [ghc-api-compat](https://github.com/hsyl20/ghc-api-compat/blob/master/ghc-api-compat.cabal).

You can also track it with git command like this:

```
$ git log --oneline --name-status --diff-filter=RAD
```
