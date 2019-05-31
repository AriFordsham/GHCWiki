# Cloud Shared Cache Build

Shake, Hadrian's underlying build system, has a cloud shared cache feature. This allows separate builds to share build artifacts stored in a cache. This can ultimately greatly speed up CI builds for example. In order to use this feature and still produce correct results, Hadrian must have accurately declare inputs and outputs.

## Terms

* **Direct Input**: All files read by a rule are direct inputs of that rule
* **Direct Output**: All files created by a rule that are the target of the rule or are direct inputs of other rules.
* **Vital Inputs**: a subset of direct inputs that excludes files whose *existence* have no affect on the vital output (i.e. cache files of external tools).
* **Vital Output**: a subset of direct outputs that are the target of the current rule or are vital inputs of any rule.
* **Indicating Inputs**: A set of files such that "a change in the indicating inputs implies a *possible* change in vital outputs" or more accurately "the only way the vital output could possibly change is if the indicating inputs have changed":
    ```
    change(X)  <->  exists x in X such that change x
    change(vital output) -> change(indicating inputs)
    ```
    Or equivalently:
    ```
    not change(indicating inputs) -> not change(vital output)
    ```
    * While it often is, the set need not be a subset or equal to the direct inputs i.e. it may include files outside of the direct inputs!
    * This set is not unique (the direct inputs are trivially a set of indicating inputs), but we try to define this as a minimal (or close to minimal) set. With respect to a rule for a haskell object file X.o, the indicating inputs are the source file X.hs and interface files Y.hi (or Y.hi-boot) for all modules Y imported by X (see [makefile-dependencies](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#makefile-dependencies)). These are the dependencies (i.e. inputs) of X.o as reported by `ghc -M X.hs`. Note that ghc reads some transitively imported module's .hi files, but those files are non-indicating inputs as ghc guarantees that the indicating .hi inputs (directly imported modules) will change if the non-indicating .hi inputs (transitively imported modules) change.
* **Non-indicating Inputs**: The direct inputs minus the indicating inputs. With respect to a rule for a haskell object file X.o, the non-indicating inputs are all hi/hi-boot files required by ghc to build X.o excluding direct inputs. This is a subset of modules transitively imported by X. These inputs are NOT reported by `ghc -M X.hs`

Properties:
* Indicating Inputs ⊆ Vital Inputs ⊆ Direct Inputs
* Vital Output ⊆ Direct Output

Consider this scenario:

* A imports B and B imports C.
* B exports a function using some types defined in C.
* `ghc -M A.hs` reports that A.o depends on i.e. had inputs: A.hs, B.hi.
* `ghc -c A.hs` produces A.o and accesses A.hs, B.hi, and C.hi.

In this case the rule to build A.o via `ghc -c A.hs` has:

* Direct Inputs: A.hs, B.hi, C.hi
* Direct Outputs: A.o, A.hi  (Note the -c option also produces .hi files)
* Vital Inputs: A.hs, B.hi, C.hi
* Vital outputs: A.o, A.hi  (Assuming another rule will have A.hi as a vital input)
* Indicating Inputs: A.hs, B.hi

## Accurate Inputs and Outputs

How accurately must be declare inputs and outputs? In any given build rule, inputs are generally expressed via the `need` function, and outputs are the target file(s) of the rule and any files passed to the `produces` function. Must we `need` all direct inputs and `produces` all direct outputs? No! In fact that would often be an onerous task. For a cloud build systems the following invariants must hold:

* All rules `need` all their indicating inputs.
* All rules `need` enough to trigger building all vital inputs.
* All rules `produces` all vital outputs excluding the rule targets(s).

Note, not all vital inputs must be `need`ed. E.g. we often `need` just a Makefile to trigger a rule that runs `./configure` and generates many vital inputs as well as the Makefile.

Also note, this implies that all indicating inputs of all rules must match some rule target (as opposed to being passed to `produces`) or be source files (i.e. exist without needing to be built).

# Reducing the Indicating Inputs set

`need`ing files generally means more code as you must `need` all indicating inputs. Hence you want to pick a convenient (i.e. minimal) set of indicating inputs. Remember this set is not unique so you're free to pick whatever indicating input set you want, but it is your responsibility to ensure is in fact a valid indicating set and to `need` that set in your Hadrian rule.

## Transitivity

Say you have an indicating set `I` for vital output `O` i.e. `change(O) -> change(I)` and you are wondering if some other (possibly overlapping) set `I'` is also an indicating set for `O`. One way you can be sure is to show that `I'` is an indicating set for `I` i.e.:

```
change(I) -> change(I')
```

By transitivity of `->` we then have `change(O) -> change(I')`. In other words the "has indicating set" relationship is transitive.

## Eliminating Redundant Needs

While writing a rule, you may suspect that a certain call to `need` is not necessary. How can you be sure it is safe to remove? It is safe to remove if the remaining `need`s still cover a valid indicating input set. Say your rule currently `need`s set `I` (`I` is your indicating inputs set) and the file you want to no longer `need` is `f ∈ I` resulting in a new set `I' = I \ {f}`. The question you must ask is "is `I'` a valid indicating inputs set?". The answer: it is safe to no longer `need` f if and only if:

```
change(O) -> change(I \ {f})  OR EQUIVALENTLY
change(O) -> change(I')
```

But this condition may be hard to show. Luckily, there there is a more convenient trick you can do here. If you know that `I'` is an indicating input set for for `{f}` then you know `I'` is an indicating inputs set for `O` and it is safe to remove the `need` for `f`. Formally:

```
f ∈ I  AND
I' = I \ {f}  AND
change(f) -> change(I')  AND
change(O) -> change(I)
  -> ( change(O) -> change(I') ) 
```

In fact this generalizes to removing a whole set of files `F` from `I`:

```
F ⊂ I  AND
I' = I \ F  AND
change(F) -> change(I')  AND
change(O) -> change(I)
  -> ( change(O) -> change(I') )
```

### English Please!

1. Start with a know indicating inputs set (e.g. all direct inputs)
    * e.g. `{ a, b, c, d }` are all the files used by this rule.
2. Find a file or subset of files, `K`, in your set that when changed will always result in some change in an other subset of file(s), `F`, in your set.
    * e.g. if `a` changed then there must have been some change in `b` or `c`. We have `F = { b, c }`
3. Remove F from your set and repeat to you're satisfaction
    * e.g. My indicating inputs set is now `{ a, d }` and I'll stop there. Now I only need to `need` `a` and `d` in my rule and can `trackAllow` `c` and `d`.

### Example

This reasoning is applied in the case of Haskell [.hi dependencies](Haskell-object-files-and-.hi-inputs). We are generating vital output `O = { A.o, A.hi }` and start with indicating inputs `I = { A.hs, B.hi, C.hi }` because linting errors show they are direct inputs. We know that a change in transitive `.hi` files (`F = B.hi, C.hi`) will result in a change in the immediate `.hi` file `k = A.hi`: `change(F) -> change({k})` and by `{k} ⊂ I' = I \ F` we get `change(F) -> change(I')` and by the above `change(O) -> change(I')`  we see that it is safe to remove the `need`s of the transitive `.hi` files.

### Even more general

We can remove the requirement that `F ⊂ I` and make a more general statement:

```
change(I \ I') -> change(I')  AND
change(O) -> change(I)
  -> ( change(O) -> change(I') ) 
```

But I'm not quite sure if this is useful in practice.


# Examples in Hadrian

## Haskell object files and .hi inputs

## Libffi

## Haskell object files with CPP

# Understanding the cost of missing dependencies 

# Linting with fsatrace

Shake provides a nice linting feature with the `--lint-fsatrace` command line option (Hadrian now supports this too). This requires installing [fsatrace](https://github.com/jacereda/fsatrace). This feature will use fsatrace to monitor file system accesses of external commands and issues lint errors accordingly. It's important to note that this is not a catch all solution as it does NOT consider:

* file accesses done by non-external commands (e.g. directly calling `parseMyFile :: IO String` in Hadrian instead of issuing Shake's `cmd` funciton and using the stdout)
* file accesses outside of the build directory and the ghc root directory.

That said, fsatrace linting should capture a significant portion of issues. The linting errors are summarized bellow.

## Lint: file used but not needed

An external command read a file but didn't `need` it. If the file is a indicating input, then add a `need` statement. Else use `trackAllow` to silence the error, or if such files should globally be ignored then add a FilePattern to `shakeLintIgnore` in the `ShakeOptions` (and a comment justifying the decision).

## Lint: file needed after being used

You `need`ed a file only after it was read. Make sure to `need` the file *before* using it.

## Lint: TODO other errors 