
This list overviews the active branches in the main GHC repository.

### `late-dmd` Late Demand Analysis


Nick Frisby. See [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782). It's stowed in a branch become of some nofib slowdowns I'm struggling to understand.

### `late-lam-lift` Late Lambda Lift


Nick Frisby. We lift some lambdas before CorePrep. I'm still determining when to 'not' lift a lambda. My terse notes [here](late-lam-lift).

### `dicts-strict` Strict Dictionaries


Nick Frisby. Make most dictionary arguments strict. It's stowed in a branch until `cardinality` is merged. It should be easier to implement after `cardinality`'s refactoring.

### `type-nats` Solver for Type-Level Naturals


Iavor S. Diatchki.  An experimental solver for discharging constraints involving natural numbers.
