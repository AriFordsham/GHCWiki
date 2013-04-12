
This list overviews the active branches in the main GHC repository.

### `dicts-strict` Strict Dictionaries


Nick Frisby. Make most dictionary arguments strict. It's stowed in a branch until `cardinality` is merged. It should be easier to implement after `cardinality`'s refactoring.

### `late-dmd` Late Demand Analysis


Nick Frisby. See [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782). It's stowed in a branch become of some nofib slowdowns I'm struggling to understand.

### `late-lam-lift` Late Lambda Lift


Nick Frisby. We lift some lambdas before CorePrep. I'm still determining when to 'not' lift a lambda. My terse notes [here](late-lam-lift).

### `type-nats` Solver for Type-Level Naturals


Iavor S. Diatchki.  An experimental solver for discharging constraints involving natural numbers.

## Inactive branches


This is a list of inactive branches, including the date of their last commit.

### `ghc-axioms` First attempt at branched type family instances


Richard Eisenberg / José Pedro Magalhães / Simon PJ. Last commit was Jan 3, 2012. This was an early attempt at implementing branched type family instances. Superseded by `overlapping-tyfams`.

### `overlapping-tyfams` Proper implementation of branched type family instances


Richard Eisenberg. Last commit was Dec 21, 2012. Merged into master on Dec 21, 2012, as commit [8366792eede3c8eb486ff15d8c8e62e9363f1959](/trac/ghc/changeset/8366792eede3c8eb486ff15d8c8e62e9363f1959/ghc). See [NewAxioms](new-axioms).
