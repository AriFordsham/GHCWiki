# Cloud Shared Cache Build

Shake, Hadrian's underlying build system, has a cloud shared cache feature. This allows separate builds to share build artifacts stored in a cache. The original hope was that this would greatly speed up CI builds, though this hope has largelly been [demotivated](demotivation). In order to use this feature and still produce correct results, Hadrian must have accurately declare inputs and outputs as described on this page.

## Demotivation

The ultimate goal is to speed up compile times while maintaining correctness. As it turns out, maintaining correctness is not easy and the benefits may not be as great as anticipated.

### Correctness

As described in the rest of this page, the [requirements](build-system-correctness) for a correct cloud cached build is stricter than a clean build. A read through the [examples](examples-in-hadrian) section may be enough to convince you that correctness is hard to achieve. Even with the help of `--lint-fsatrace` we still only get lint errors about direct inputs rather than indicating inputs, so the presence or absence of fsatrace lint errors is not enough to establish (in)correctness.

### Stage2 Cache Misses

GHC is usually built in 2 stages, the majority of the second stage is build using the ghc binary produced in the first stage. Stage 1 ghc is less feature-full than stage 2 and so takes only 1/4 to 1/3 of the total build time. Hence stage 1 is a dependency for most of stage 2. This means that any change to ghc will result in cache misses for the majority of stage 2 (See #16633). A possible solution is to use `--freeze1`, but the correctness of this is questionable:

* Bugs may appear or be hidden by the frozen stage 1 compiler.
* Stage 2 ghc is linked against base package build by stage 1 ghc. This could cause bugs or build failures if the base package or the .hi format differs between stage 1 and stage 2 ghc (i.e. when using `--freeze1`).
![staged-compilation.svg](uploads/9d7fe6d18a45e54ea7b82ac4d596f718/staged-compilation.svg)

On a positive note, stage 1 still takes a significant portion of time to build, and does not suffer from this cache miss issue.

### Local Builds

When developing, each iteration may take a significant amount of time to build ghc. Can caching help here? When using `--freeze1`, one can expect many cache hits, but in practice we usually do an incremental build instead of a clean build with each iteration. Hence caching is unhelpful: anything that would be a cache hit already exists and will not be rebuilt any way. That said, the requirements for a correct incremental build are close to those of a cloud build, so work done on cloud build correctness is usually also beneficial for incremental build correctness.

What about when we change between branches and/or build flavours often. This will be able to take advantage of the cache (assuming you've done a similar build before), though keep in mind that if you'll often want to avoid `--freeze1` after changing branches/flavours, so you'll run into the same issue as [above](stage2-cache-misses) if you haven't done the exact same build before. 

## Terms

* **Direct Input**: All files read by a rule are direct inputs of that rule
* **Direct Output**: All files created by a rule that are the target of the rule or are direct inputs of other rules.
* **Vital Inputs**: a subset of direct inputs that excludes files whose *existence* have no affect on the vital output (i.e. cache files of external tools).
* **Vital Output**: a subset of direct outputs that are the target of the current rule or are vital inputs of any rule.
* **Indicating Inputs**: A set of files such that "a change in the indicating inputs implies a *possible* change in vital outputs" or more accurately "the only way the vital output could possibly change is if the indicating inputs have changed":
    ```
    change(X)  <->  exists x in X such that x has changed upon a full rebuild.
    change(vital output) -> change(indicating inputs)
    ```
    Or equivalently:
    ```
    not change(indicating inputs) -> not change(vital output)
    ```
    * While it often is, the set need not be a subset or equal to the direct inputs i.e. it may include files outside of the direct inputs!
    * This set is not unique (the direct inputs are trivially a set of indicating inputs), but we try to define this as a minimal (or close to minimal) set.

Properties:
* Vital Inputs ⊆ Direct Inputs
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

## Build System Correctness

There are a varying standards of correctness here:

1. **Clean Build** having run no rules yet (or having deleted all build artifacts i.e. the build directory) running a build will succeed with the correct output files.
2. **Incremental Build** having done a full or partial build, then changing a source file, a further build will succeed with the correct output files.
3. **Cached Build** having done a full or partial build with caching enabled (`--shared` option), then changing a source file and delete all build artifacts (but not the cache), a further build with caching enabled will succeed with the correct output files.

Generally we must to track dependencies correctly in order to achieve correctness. The exact invariants are as follows:

1. **Clean Build**
    * All rules `need` enough to trigger all vital inputs to be produced (this can be via direct or transitively triggered rules).
2. **Incremental Build**
    * Clean build correctness
    * All rules `need`/`needed` a valid indicating inputs set.
3. **Cached Build**
    * Clean and Incremental build correctness
    * All rules `produces` all vital outputs excluding the rule targets(s) OR caching is disabled with `historyDisable`.

Note: for incremental builds it is *not* sufficient for indicating inputs to be transitively needed by other rules, they must be directly `need`/`needed`.

Note: for incremental builds we don't need to `need` all vital inputs, just indicating inputs. The libffi rules are a particularly good example of this.

Note: this implies that all indicating inputs of all rules must either be source files (i.e. exist without needing to be built) or must match some rule target as opposed to being passed to `produces`.

# Reducing the Indicating Inputs set

`need`ing files generally means more code as you must `need` all indicating inputs. Hence you want to pick a convenient (i.e. minimal) set of indicating inputs. Remember this set is not unique so you're free to pick whatever indicating input set you want, but it is your responsibility to ensure is in fact a valid indicating set and to `need` that set in your Hadrian rule.

## Removing Irrelevant Inputs

Some inputs intuitively have no effect on the vial outputs and can always be excluded from your indicating inputs set. Such files are "irrelevant" exactly when it is not a vital input nor transitive vital input.

For example. We used to unnecessarily need all library files of all library dependencies when building .hi and .o files, but actually we only depend on relevant .hi files from our library dependencies, not the whole static/dynamic library file. We can see that the static/dynamic library file is not a vital input, nor a transitively vital input. Hence it was safe to remove the corresponding needs.

## Removing Redundant Inputs

To come up with a valid indicating set for a rule:

1. Start with a known indicating inputs set. All direct inputs is a good starting point. You're free to overestimate: if you're not sure whether a file is a direct dependency, you can safely include it in the set any way.
    * e.g. `{ a, b, c, d }` are all the files used by this rule.
2. Find a file or subset of files, `K`, in your set that when changed will always imply some change in an other subset of file(s), `F`, in your set.
    * e.g. if `a` changed then there must have been some change in `b` or `c`. We have `F = { b, c }`
3. Remove F from your set and repeat to you're satisfaction
    * e.g. My indicating inputs set is now `{ a, d }` and I'll stop there. Now I only need to `need` `a` and `d` in my rule and can `trackAllow` `c` and `d` to silence fsatrace lint errors.

### Example

This reasoning is applied in the case of Haskell [.hi dependencies](Haskell-object-files-and-.hi-inputs). We are generating vital output `O = { A.o, A.hi }` and start with indicating inputs `I = { A.hs, B.hi, C.hi }` because linting errors show they are direct inputs. We know that a change in transitive `.hi` files (`F = B.hi, C.hi`) will result in a change in the immediate `.hi` file `k = A.hi`: `change(F) -> change({k})` and by `{k} ⊂ I' = I \ F` we get `change(F) -> change(I')` and by the above `change(O) -> change(I')`  we see that it is safe to remove the `need`s of the transitive `.hi` files.

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

### Even more general

We can remove the requirement that `F ⊂ I` and make a more general statement:

```
change(I \ I') -> change(I')  AND
change(O) -> change(I)
  -> ( change(O) -> change(I') ) 
```

But I'm not quite sure if this is useful in practice.


# Examples in Hadrian

## Dependency Generation and Header Files

This is a particularly disappointing use case. Imagine we have a haskell module that uses CPP:

```
-- A.hs
{-# LANGUAGE CPP #-}
#include "b.h"
```
```
-- B.h
#include "c.h
```
```
-- C.h
main = putStrLn "Hello CPP"
```

Now when we build `A.hs` we must make sure to need the CPP includes i.e. building `A.hs` depends on `A.hs`, `B.h`, and `C.h`. We ideally want to descover the `.h` dependencies dynamically so that if we e.g. add new `#include` macros, we don't need to update Hadrian. In Hadrian we do this by generating a `.dependencies` file generated using GHC's `-M -include-cpp-deps` option. Naturally, this is done in a rule for the `.dependencies` file. Now the rule to build `A.hs` first `need`s the  `.dependencies` file, then dynamically needs the dependencies listed within (in this case `A.hs`, `B.h`, and `C.h`). This looks something like:

```haskell
".dependencies" %> _ -> do
  need ["A.hs"]
  ghcBuidDeps "A.hs"    -- Runs `ghc -M -include-cpp-deps A.hs ...`
["A.o", "A.hi"] &%> _ -> do
  need [".dependencies"]
  deps <- getDepsFromFile "A.o" ".dependencies"
  need deps
  ghcBuild "A.hs"
```

So what is the problem here? The rule for `["A.o", "A.hi"]` is fine, it tracks the correct dependencies, but what about the `".dependencies"` rule? Running `ghc -M -include-cpp-deps A.hs ...` reads A.hs then traverses all the `#include`ed files, hence the dependencies of `.dependencies` are `A.hs`, `B.h`, and `C.h` (this indeed can't be reduced; any of those files can change the output of `.dependencies`). Well, this is awkward! The dependencies of `.dependencies` are exactly the dependencies that we are trying to discover in the first place! If we now include `D.h` from `C.h` and recompile, `A.hs` will recompile (it depends on `C.h` which has changed, but `.dependencies` will not be recalculated so `["A.o", "A.hi"]` will *not* track the new `D.h` dependency. Now change `D.h` and rebuild, `A.hs` (i.e. the `["A.o", "A.hi"]` rule) will not be rebuilt even though a dependency has changed.

## Haskell object files and .hi inputs

Consider crating a rule for a file X.o that compiles X.hs with ghc. We use `ghc -M` which returns `X.o : X.hs X.hi Y.hi`. The `Y.hi` is there because module `X` imports module `Y`. We conclude that `direct inputs = indicating inputs = { X.hs, X.hi, Y.hi } So we implement the rule like this:

```
"X.o" %> \ _ -> do
    need ["X.hs", "X.hi", "Y.hi"]
    buildWithGhc "X.o"
```

This seems correct, but running the rule with `--lint-fsatrace` complains that the rule used a file `Z.hi` without `need`ing it. Upon further inspection, module `Y` imports module `Z`. It turns out ghc will use a *subset* of transitively imported modules' .hi files. This means the set of direct inputs is larger than what we originally though, it contains transitive .hi files. Unfortunately there doesn't seem to be any feasible way to predict what transitive .hi files ghc will use at compile time. Fortunately we only need to `need` indicating inputs, so we can stay optimistic and try to show that transitive .hi files are not indicating inputs. As described in [reducing the Indicating Inputs set](#reducing-the-indicating-inputs-set), we can remove the transitive .hi files from the indicating inputs set by showing that:

```
  change({transitive .hi files}) -> change({current indicating inputs} \ {transitive .hi files})
= change({ Z.hi })               -> change({ X.hs, X.hi, Y.hi })
```

With some insight from an experienced ghc developer we see that all .hi files contain "a list of the fingerprints of everything it used when it last compiled the file" (from the [user guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html?highlight=fingerprint#the-recompilation-checker)). Making the practical assumption that the fingerprint's hashing function is injective, we know that `change({ transitive .hi files }) -> change({ X.hi }) -> change({ X.hs, X.hi, Y.hi })` and hence it is safe to not `need` transitive .hi files. In we `need` the direct .hi files reported by `ghc -M` and `trackAllow` all other `.hi` files. We apply the same logic for .hi-boot files too.

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

# Relative vs. Absolute Paths

Shake is sensitive to relative vs absolute paths. In general, always use relative paths for files in the source and build tree, and always use absolute paths for system files.