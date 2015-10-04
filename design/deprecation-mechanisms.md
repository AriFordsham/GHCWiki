
This page serves as a place to collect proposals, tickets, open problems, and ideas regarding mechanisms for deprecating program interfaces.

## Warning on import of `DEPRECATED` definitions


GHC has for a long time allowed users to attach `DEPRECATED` pragmas to definitions, defining a warning message which will be shown to clients when a use-site of the marked definition is encountered.


Sadly, this design doesn't account for instances where a deprecated definition is explicitly imported but not actually \*used\*. [\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119) argues that we should throw a warning when an explicit import of a deprecated definition is found.

TODO Write formal specification.

## Deprecating exports


GHC's existing `DEPRECATED` pragma only covers definitions, not their visibility. [\#4879](https://gitlab.haskell.org//ghc/ghc/issues/4879) argues that GHC should allow `DEPRECATED` pragmas to be attached to names in export lists, meaning that the definition itself isn't deprecated (allowing warning-free uses within the defining module), only its role as an outward-facing interface.

TODO Write specification

## Class method deprecation


As of GHC 7.10 there is no effective way to demote a typeclass method to a normal top-level binding (see [\#10071](https://gitlab.haskell.org//ghc/ghc/issues/10071)). One such case where this would be useful is to remove the `return` method from `Monad`.


A concrete proposal for allowing `DEPRECATED` pragmas to be attached to class methods can be found at [Design/MethodDeprecations](design/method-deprecations).
