
Currently many build systems use `ghc`'s `--make` to manage builds.  However, there are reasons to believe that GHC's single-shot mode is to be preferred over `--make`:

- GHC's parallel compilation support (`-j`) scales relatively poorly to high core counts without runtime system tuning
- Build managers may be in a position to do fine-grained recompilation checking beyond what GHC does.


Consequently, while GHC's single-shot does require more work in the form of repeated interface file loads (something which could be optimised in the future), single-shot mode may result in improved overall compilation performance in real-world settings.


However, it is currently challenging for external build managers to provide effective recompilation avoidance when using single-shot mode as GHC offers no way for external tools to gain knowledge of dependencies needed by GHC during compilation. This proposal is the result of discussions between Duncan Coutts, Herbert Valerio Riedel, and Ben Gamari aiming to resolve this issue.

## Pre-compilation dependencies


The first flag, which we call `-precompilation-deps <file>`, is a new compiler mode allowing the user to query GHC for the set of dependencies GHC would need to build a given module(s). For instance, the build tool might call

```wiki
$ ghc -precompilation-deps /tmp/pre-deps -I/an/include/path -i/a/module/path -package a-package Module1 Module2 
```


And GHC would write the result of its dependency analysis `/tmp/pre-deps` in a structured format (presumably JSON). This analysis would compute for each module

- the module pragmas of that module; this includes,

  - language pragmas (which build tools like `cabal-install` might want to metadata consistency check)
  - `{-# OPTIONS_GHC #-}` pragmas which may,  add dependencies on plugins (with `-plugin`) or preprocessors (with `-pgmf`)
  - module-level `{-# DEPRECATED #-}` pragmas
- the module's direct `import`s. These may be of two varieties:

  - source files (e.g. `.hs`, `.lhs`, or `.hs-boot` files). In this case the result will include the path where the source file was found.
  - compiled modules from an external package. In this case the result will include the package ID where the module was found (and possibly the module name?)

  In both cases GHC will include a list of file paths where GHC looked for the import before finding it. This list can be efficiently represented as a globbing pattern.


This information is sufficient for a build tool to build a dependency graph to plan its build and later update that graph and build plan after changes.


To consider the concrete case of `cabal-install`: the tool would start a fresh build by first invoking `ghc -precompilation-deps` on all modules in the package to be built (in a single GHC invocation). On subsequent rebuilds the tool would first construct a list of files that have changed since the last build and call `ghc -precompilation-deps` on that set. It would then filter the resulting dependencies to those that have changed, and again call `ghc -precompilation-deps`. It would continue in this way until a fixed-point is reached at which point it will know the full set of modules which must be rebuilt.

## Post-compilation dependencies


The second flag, which we call `-fpostcompilation-deps <file>`, allows the build manager to gain knowledge about the files which GHC looked at during a build. For instance, to build `Module1` the build tool might call

```wiki
$ ghc -c -fpostcompilation-deps /tmp/post-deps -I/an/include/path -i/a/module/path -package a-package Module1
```


And GHC would write a list of the new dependencies (those not reported by `-precompilation-deps`) needed during the compilation of `Module1` to `/tmp/pre-deps` in a structured format (similar to the format used by `-precompilation-deps` above). This include,

- Dependencies added in Template Haskell splices by `addDependentFile` (see [DependencyTracking](dependency-tracking))
- `#include`'d paths
- Header files needed by `foreign import`s
- Plugins


This serves two purposes:

- Allows the build manager to gain knowledge of dependencies which can only be discovered during compilation (e.g. due to `addDependentFile`)
- Allows the `-precompilation-deps` mode to remain cheap as it only needs to scan the module header


The build system can then persist this dependency information and use it to inform future recompilation decisions.

## Limitations


It may still be possible for API users and plugins to add dependencies which are not tracked by this scheme.

## Prior art

- [ Phab:D3898](https://phabricator.haskell.org/D3898) aimed to resolve a similar issue.

## Further Additions

### Dependency pragma


It might make sense to add a `{-# DEPENDS fileA fileB ... #-}` module pragma, which allows a source file to explicitly declare a dynamic dependency (e.g. due to `addDependentFile`).
