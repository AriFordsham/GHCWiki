**Work in progress**

This page outlines the roadmap for [linear types](linear-types).

Issues about linear types are gathered under the ~LinearTypes tag.

#### Stage 0: the first patch

- !852

#### Stage 1: user-facing features

- Syntax:
  - Multiplicity parametric function arrow: #18459
  - Lambdas and patterns: #18460
  - Let binders: #18461
  - Record fields: #18462
- Clean up `FUN`: #18373
- Template Haskell
  - Prevent linear slices: #18465
  - Reify shows inaccurate type: #18378
- Optimisation: promote `case[One]` to `case[Many]` #18471
- Proposal compliance: #15981

#### Stage 2: linear Core

- Support linear FFI functions: #18472

#### Stage 3: performance

- Improve CPR in presence of unrestricted field: #18490
#### Future