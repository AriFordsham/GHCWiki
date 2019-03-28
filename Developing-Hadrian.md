## Cloud Shared Cache Build

Shake, Hadrian's underlying build system, has a cloud shared cache feature. This allows separate builds to share build artifacts stored in a cache. This can ultimately greatly speed up CI builds for example. In order to use this feature and still produce correct results, Hadrian must have accurate dependencies.

### Terms

* **Direct Dependency**: All files read by a rule are direct dependencies of that rule
* **Direct Output**: All files created by a rule that are the target of the rule or are direct dependencies of other rules.
* **Vital Output**: a subset of direct outputs that are the target of the current rule or are vital inputs of any other rule.
* **Vital Input**: a subset of direct dependencies that excludes files whose existence have no affect on the vital output (i.e. cache files of external tools).
* **Shallow Dependency**: A subset of the direct dependencies such that: *no change in the shallow dependencies -> no change in the vial output*. This set is not unique (the direct dependencies are trivially a set of shallow dependencies), but we try to define this as a minimal (or close to minimal) set. With respect to a rule for a haskell object file X.o, the shallow dependencies are the source file X.hs and interface files Y.hi (or Y.hi-boot) for all modules Y imported by X (see [makefile-dependencies](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#makefile-dependencies)). These are the dependencies of X.o as reported by `ghc -M X.hs`. Note that this is a sufficient set as ghc guarantees that the shallow .hi dependencies will change if the deep .hi dependencies change.
* **Deep Dependency**: The direct dependencies minus the shallow dependencies. With respect to a rule for a haskell object file X.o, the deep dependencies are all hi/hi-boot files required by ghc to build X.o excluding direct dependencies. This is a subset of modules transitively imported by X. These dependencies are NOT reported by `ghc -M X.hs`

Properties:
* Shallow Dependency ∪ Deep Dependency = Direct Dependency
* Shallow Dependency ⊆ Vital Input ⊆ Direct Dependency
* Vital Output ⊆ Direct Output

Consider this scenario:

* A imports B and B imports C.
* B exports a function using some types defined in C.
* `ghc -M A.hs` reports that A.o depends on: A.hs, B.hi.
* `ghc -c A.hs` produces A.o and accesses A.hs, B.hi, and C.hi.

In this case the rule to build A.o via `ghc -c A.hs` has:

* Direct dependencies: A.hs, B.hi, C.hi
* Direct Outputs: A.o, A.hi  (Note the -c option also produces .hi files)
* Vital inputs: A.hs, B.hi, C.hi
* Vital outputs: A.o, A.hi  (Assuming another rule will have A.hi as a vital input)
* Shallow Dependencies: A.hs, B.hi
* Deep Dependencies: C.hi

### Accurate Dependencies

How accurate do dependencies need to be? In any given build rule, dependencies are generally expressed via the `need` function, and outputs are the target file(s) of the rule and any files passed to the `produces` function. Must we `need` all direct inputs and `produces` all direct outputs? Not really, in fact that would often be an onerous task. For a cloud build systems the following invariants must hold:

* need all shallow dependencies.
* `produces` all vital outputs (excluding the rule target).

#### Linting with fsatrace

TODO How does --lint-fsatrace help use?

