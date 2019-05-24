## Cloud Shared Cache Build

Shake, Hadrian's underlying build system, has a cloud shared cache feature. This allows separate builds to share build artifacts stored in a cache. This can ultimately greatly speed up CI builds for example. In order to use this feature and still produce correct results, Hadrian must have accurate dependencies.

### Terms

* **Direct Dependency**: All files read by a rule are direct dependencies of that rule
* **Direct Output**: All files created by a rule that are the target of the rule or are direct dependencies of other rules.
* **Vital Dependency**: a subset of direct dependencies that excludes files whose *existence* have no affect on the vital output (i.e. cache files of external tools).
* **Vital Output**: a subset of direct outputs that are the target of the current rule or are vital dependencies of any rule.
* **Indicating Dependencies**: A subset of the direct dependencies such that "a change in the indicating dependencies implies a *possible* change in vital output" or more accurately "the only way the vital output could possibly change is if the indicating dependencies have 
    ```
    change in the vital output -> change in the indicating dependencies
    ```
    Or equivalently:
    ```
    no change in the indicating dependencies -> no change in the vital output
    ```
    This set is not unique (the direct dependencies are trivially a set of indicating dependencies), but we try to define this as a minimal (or close to minimal) set. With respect to a rule for a haskell object file X.o, the indicating dependencies are the source file X.hs and interface files Y.hi (or Y.hi-boot) for all modules Y imported by X (see [makefile-dependencies](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#makefile-dependencies)). These are the dependencies of X.o as reported by `ghc -M X.hs`. Note that ghc reads some transitively imported module's .hi files, but those files are non-indicating dependencies as ghc guarantees that the indicating .hi dependencies (directly imported modules) will change if the non-indicating .hi dependencies (transitively imported modules) change.
* **Non-indicating Dependencies**: The direct dependencies minus the indicating dependencies. With respect to a rule for a haskell object file X.o, the non-indicating dependencies are all hi/hi-boot files required by ghc to build X.o excluding direct dependencies. This is a subset of modules transitively imported by X. These dependencies are NOT reported by `ghc -M X.hs`

Properties:
* Indicating Dependency ∪ Non-indicating Dependency = Direct Dependency
* Indicating Dependency ⊆ Vital Dependency ⊆ Direct Dependency
* Vital Output ⊆ Direct Output

Consider this scenario:

* A imports B and B imports C.
* B exports a function using some types defined in C.
* `ghc -M A.hs` reports that A.o depends on: A.hs, B.hi.
* `ghc -c A.hs` produces A.o and accesses A.hs, B.hi, and C.hi.

In this case the rule to build A.o via `ghc -c A.hs` has:

* Direct Dependencies: A.hs, B.hi, C.hi
* Direct Outputs: A.o, A.hi  (Note the -c option also produces .hi files)
* Vital Dependencies: A.hs, B.hi, C.hi
* Vital outputs: A.o, A.hi  (Assuming another rule will have A.hi as a vital dependency)
* Indicating Dependencies: A.hs, B.hi
* Non-indicating Dependencies: C.hi

### Accurate Dependencies

How accurate do dependencies need to be? In any given build rule, dependencies are generally expressed via the `need` function, and outputs are the target file(s) of the rule and any files passed to the `produces` function. Must we `need` all direct inputs and `produces` all direct outputs? No! In fact that would often be an onerous task. For a cloud build systems the following invariants must hold:

* All rules need all their indicating dependencies.
* All rules need enough trigger building all vital dependencies.
* All rules `produces` all vital outputs excluding the rule targets(s).

Note not all vital dependencies must be needed. E.g. we often need just a Makefile to trigger a rule that runs `./configure` and generates many vital dependencies as well as the Makefile.

Also note, this implies that All indicating dependencies of all rules must match some rule target (as opposed to being passed to `produces`) or be source files (i.e. exist without needing to be built).

### Linting with fsatrace

Shake provides a nice linting feature with the `--lint-fsatrace` command line option (Hadrian now supports this too). This requires installing [fsatrace](https://github.com/jacereda/fsatrace). This feature will use fsatrace to monitor file system accesses of external commands and issues lint errors accordingly. It's important to note that this is not a catch all solution as it does NOT consider:

* file accesses done by non-external commands (e.g. directly calling `parseMyFile :: IO String` in Hadrian instead of issuing Shake's `cmd` funciton and using the stdout)
* file accesses outside of the build directory and the ghc root directory.

That said, fsatrace linting should capture a significant portion of issues. The linting errors are summarized bellow.

#### Lint: file used but not depended upon

An external command read a file but didn't `need` it. If the file is a indicating dependency, then add a need statement. Else use `trackAllow` to silence the error, or if such files should globally be ignored then add a FilePattern to `shakeLintIgnore` in the `ShakeOptions` (and a comment justifying the decision).

#### Lint: file depended upon after being used

You `need`ed a file only after it was read. Make sure to `need` the file *before* using it.

#### Lint: TODO other errors 