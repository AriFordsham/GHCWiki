**WORK IN PROGRESS**


THIS IS NOT READY FOR PUBLIC BIKESHEDDING YET

# Redesigned GHC Warnings

## Current Situation (GHC 7.10)


GHC currently uses a somewhat unsatisfying warning CLI:


The following flags resemble more or less GCC's CLI

```wiki
   -W     enable normal warnings
   -w     disable all warnings
   -Wall  enable almost all warnings (details in )
   -Werror make warnings fatal
   -Wwarn make warnings non-fatal
```


While the following (ab)use the `-f` flag namespace (rather than the the `-W` namespace like GCC):

```wiki
 -fwarn-deprecated-flags
        warn about uses of commandline flags that are deprecated
 -fwarn-duplicate-constraints
        warn when a constraint appears duplicated in a type signature
 -fwarn-duplicate-exports
        warn when an entity is exported multiple times
 -fwarn-hi-shadowing
        warn when a .hi file in the current directory shadows a library
 -fwarn-identities
        warn about uses of Prelude numeric conversions that are probably the identity (and hence could be omitted)
 -fwarn-implicit-prelude
        warn when the Prelude is implicitly imported
 -fwarn-incomplete-patterns
        warn when a pattern match could fail
 -fwarn-incomplete-uni-patterns
        warn when a pattern match in a lambda expression or pattern binding could fail
 -fwarn-incomplete-record-updates
        warn when a record update could fail
 -fwarn-lazy-unlifted-bindings
        (deprecated) warn when a pattern binding looks lazy but must be strict
 -fwarn-missing-fields
        warn when fields of a record are uninitialised
 -fwarn-missing-import-lists
        warn when an import declaration does not explicitly list all the names brought into scope
 -fwarn-missing-methods
        warn when class methods are undefined
 -fwarn-missing-signatures
        warn about top-level functions without signatures
 -fwarn-missing-local-sigs
        warn about polymorphic local bindings without signatures
 -fwarn-monomorphism-restriction
        warn when the Monomorphism Restriction is applied
 -fwarn-name-shadowing
        warn when names are shadowed
 -fwarn-orphans, -fwarn-auto-orphans
        warn when the module contains orphan instance declarations or rewrite rules
 -fwarn-overlapping-patterns
        warn about overlapping patterns
 -fwarn-tabs
        warn if there are tabs in the source file
 -fwarn-type-defaults
        warn when defaulting happens
 -fwarn-unrecognised-pragmas
        warn about uses of pragmas that GHC doesn't recognise
 -fwarn-unused-binds
        warn about bindings that are unused
 -fwarn-unused-imports
        warn about unnecessary imports
 -fwarn-unused-matches
        warn about variables in patterns that aren't used
 -fwarn-unused-do-bind
        warn about do bindings that appear to throw away values of types other than ()
 -fwarn-wrong-do-bind
        warn about do bindings that appear to throw away monadic values that you should have bound instead
 -fwarn-unsafe
        warn if the module being compiled is regarded to be unsafe. Should be used to check the safety status of modules when using safe inference.
 -fwarn-safe
        warn if the module being compiled is regarded to be safe. Should be used to check the safety status of modules when using safe inference.
 -fwarn-warnings-deprecations
        warn about uses of functions & types that have warnings or deprecated pragmas
 -fwarn-amp
        warn on definitions conflicting with the Applicative-Monad Proposal (AMP)
 -fwarn-typed-holes
        Enable holes in expressions.
```

## Proposed Change

TODO needs more elaboration & motivation

- Keep the current `-f(no-)warn-$WARNTYPE` flags as hidden flag aliases for
- newly introduced `-W(no-)$WARNTYPE` flags more in line with GCC's conventions, e.g.

  - `-Worphans` instead of `fwarn-orphans`
  - `-Wno-missing-methods` instead of `-fno-warn-missing-methods`
- Introduce variant of `-Werror` (c.f. GCC's `-Werror=*`) which allows to specify the individual warnings to be promoted to errors, e.g.

  - `-Wall -Werror=orphans` would only promote `-Worphans` warnings into errors
  - `-Wall -Wno-error=missing-methods` would promote all warnings *except*`-Wmissing-methods` into errors
- Introduce some warning sets, e.g.

  - `-Wcompat` could refer to all warnings about future compatility GHC *currently* knows about (like e.g. `-Wcompat-amp`, `-Wcompat-mfp`, `-Wcompat-mrp`)
