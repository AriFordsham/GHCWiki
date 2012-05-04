*This page discusses the design and potential implementation of "holes" in GHC. The progress is tracked in [\#5910](https://gitlab.haskell.org//ghc/ghc/issues/5910). The development repository is [ here](https://github.com/xnyhps/ghc).*

# Introduction


Informally, a "hole" is something to be filled. A hole in the ground can cause an injury if you stepped into it, but you can also build a house around it without a problem. You can easily measure a hole to determine how much material (e.g. dirt or concrete) is needed to fill it.


The analogy of a hole in the ground can be transferred to a hole in a program. If you run the program and encounter a hole, the runtime will halt (e.g. as if you encountered `undefined`). But you can still compile a program with holes. The compiler reports information about the hole, so that the programmer knows what code should replace the hole.


These are the requirements of the problem that holes solve:

1. Extract information about subterms in a program.
1. Do not interrupt compilation.


The extracted information from a hole can include, among other things:

- The expected type of the hole
- The local bindings (and their types) in the scope of the hole


We first describe related work, including concepts that are similar in other languages as well as other approaches to solving the problem proposed. Then, we discuss the proposal in detail.

# Related Work

## Goals in Agda


One of the features of the Emacs mode for [ Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) is the ability to insert a goal, a placeholder for code that is yet to be written. By inserting a `?` in an expression, the compiler will introduce a goal. After loading the file (which typechecks it), Agda gives an overview of the goals in the file and their types.


For example:

```wiki
test : List Bool
test = Cons ? (Cons ? Nil)
```


gets turned into

```wiki
test : List Bool
test = Cons { }0 (Cons { }1 Nil)
```


with the output

```wiki
?0 : Bool
?1 : Bool
```


As can be seen here, goals are numbered, and the typechecker returns the inferred type for each of these goals.


Goals can make it a lot easier to write code. They allow typechecking to continue although certain parts of code are missing. They also work as a to-do list for incomplete programs.

## Deferring type errors to runtime


The proposal [DeferErrorsToRuntime](defer-errors-to-runtime) implements a flag that turns every type error into a warning with an associated bit of code that is run when the offending ill-typed term is encountered at runtime. At the time of writing, this is implemented in GHC as `-fdefer-type-errors`.


Deferring type errors alone is not a solution that fits the problem description; however, it can be used in conjunction with other things to solve the problem.

## Inserting deliberate type errors


One technique for finding the type of a subterm that has been often seen on the mailing lists is deliberately inserting ill-typed terms, so that the type error reports the expected type of the term.


Here is an example:

```wiki
1 + ()
```


We get an error that there is no instance for `Num ()`.


We can do this is a more refined manner by using `undefined` with a type annotation:

```wiki
test :: [Bool]
test = undefined : ((undefined :: ()) ++ [])
```


This gives the error:

```wiki
test.hs:2:22:
    Couldn't match expected type `[Bool]' with actual type `()'
    In the first argument of `(++)', namely `(undefined :: ())'
    In the second argument of `(:)', namely `((undefined :: ()) ++ [])'
    In the expression: undefined : ((undefined :: ()) ++ [])
Failed, modules loaded: none.
```


The advantage of using `undefined` is that we can remove the type annotation, and the program will probably compile.


The clear problem with deliberate type errors is that the program does not type-check. We cannot use this technique on multiple locations at one time. We also only get type errors and not other information.


With **deliberate errors and deferred type errors**, we do get a program that type-checks. This is actually a reasonable solution; however, it still has two problems:

- Deferring type errors is indiscriminate: you defer both the deliberate and unintended type errors.
- It does not provide useful information other than type errors.

## Implicit parameters


The [implicit parameters](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#implicit-parameters) extension allows a programmer to delay the binding of a variable while still preserving its type.


Here is an example:

```wiki
$ ghci -XImplicitParams
Prelude> :t ?x : (?y ++ [])
?x : (?y ++ []) :: (?x::a, ?y::[a]) => [a]
```


The implicit parameters `?a` and `?b` appear as constraints in the type of the term containing them.


Implicit parameters must be bound, either in a term, e.g. `let ?x = ... in ...`, or in a type signature, e.g. `let f :: (?x::a, Num a) => a; f = 1 + ?x`. If an implicit parameter is not bound, it results in a type error. Of course, we can defer the type errors, but then we have the same problem with indiscriminate deferral.


Implicit parameters do not serve very well for debugging. Due to the binding requirement, they are not suitable for typing things with unknown types. Either you must change type signatures or you must deal with the unbound implicit parameter type errors (or warnings in case the errors are deferred). Lastly, since implicit parameters are meant for usage in programs, it does not seem like they should be used for extracting additional information about the parameter's location. (This is not a technical argument, of course.)

# Proposal


In this section, we discuss the proposed extension to GHC.

## Language extension


Since we are changing the syntax and semantics of Haskell, we feel that this should become a language extension (rather than another kind of compiler flag). For now, we proposed the name `Holes` (as in `-XHoles`), though this could change after discussion.

## Variations


We view a hole as a piece of syntax that is inserted in code as a placeholder until the programmer fills that location with something else. Numerous views on the syntax and semantics of holes have been discussed. Here are a few:

### Term wildcards


This approach mirrors Agda goals. The actual syntax is debatable, but we think `_` is quite nice, since it appears to be illegal as an expression.


Example:

```wiki
test :: [Bool]
test = _ : (_ ++ [])
```


Comments:

- Reports the source location of each hole
- Does not allow two holes to have the same type
- Requires evaluation semantics for hole

### Named term variables


This approach mirrors implicit parameters. Each hole is given a name. Within a module (or even a program/library?), each every hole with the same name has the same type. Again, the actual syntax is debatable, but for now, we use `_?x` for a hole with the (possibly shared) name `x`.


Example:

```wiki
test :: [Bool]
test = _?x : (_?y ++ [])
```


Comments:

- Reports the name and source location of each hole
- Requires a fresh name to distinguish one hole from others
- Requires evaluation semantics for hole

### Term brackets (ranges)


Instead of an actual term, we can use a special form of bracketing to indicate a hole. As above, syntax is debatable, but for now, we use `{_` and `_`} for the brackets of the hole.


Example:

```wiki
test :: [Bool]
test = {_ undefined _} : ({_ undefined ++ [] _})
```


Comments:

- Reports the source location of each hole
- Requires opening and closing brackets
- Allows wildcard term to be treated as syntactic sugar, e.g. `_` desugars into `{_ undefined _`} 
- Does not require evaluation semantics for the brackets


Note that we can extend this with names for each pair of brackets.

### Type wildcards


Instead of term holes, we can use a special type to indicate an unknown type. The type hole would be reported. We use `_` for the syntax here.


Example:

```wiki
test :: [_]
test = (undefined::_) : (undefined ++ [] ::_)
```


Comments:

- Reports the source location of each hole
- Does not allow two holes to be equal
- Does not require evaluation semantics for the holes
- Can be used in type annotations for both variables and large expressions (as in the term brackets)
- Allows partial types to be specified
- (?)May not support reporting local bindings


Note that we can extend this with names for each type hole.

## User's view


For this specification, we use the named term variables variant (though it may also apply to other variants).


When using holes (i.e. `-XHoles` is set), we expect the following:

1. The program should type-check as if every hole `_?h` is replaced with `undefined`. There is an exception to this rule: see [Ambiguous types](holes#ambiguous-types) below.
1. If the program is well-typed (as above), then:

  - The types of all holes should be reported.
  - Reporting the hole types should not cause type-checking (or compiling in general) to stop (in error).
1. (optional) If the program is ill-typed, then:

  - The types of all holes should be reported.

### Ambiguous types


Suppose that we replace every hole with `undefined` and type-check the program without `-XHoles`. Some programs would not type-check due to unsolved class constraints that result in ambiguous types. For example, `show _?h` becomes `show undefined`, and the `Show` constraint is not instantiated, so the program does not type-check.


We think holes can be extremely useful with ambiguous types. We prefer that a program, in which there is a hole with unsolved constraints on the type of the hole, still be considered well-typed, assuming the rest of the program is well-typed. In the above example, we would expect `show _?h` to have the type `String` with the reported hole type `_?h :: Show a => a`.

#### Monomorphism restriction


Some ambiguous types fall under the monomorphism restriction. That is, the following program will not type under Haskell2010 due to the restriction that `f` have a monomorphic type:

```wiki
f = undefined >>= undefined
```


We also expect holes to be very useful in these cases, for example by replacing each `undefined` with a hole:

```wiki
f = _?h >>= _?i
```


Thus, we prefer that this program be considered well-typed with the holes reported to have the types `_?h :: Monad m => m a` and `_?i :: Monad m => a -> m b`.


If the extension `-XNoMonomorphismRestriction` is used, we expect that the typing of the holes will not change.

### Type of a hole


The type of a hole should be the resolved type with minimum constraints. That is, the type of a hole should only have constraints that have not been solved but are either inferred from the context (e.g. `show _?h`) or given in a type annotation/signature (e.g. `_?h :: Show a => a`.
