
This page describes the design and potential implementation of "holes" in GHC. The intention of holes is to support debugging types in Haskell programs. We have observed that programmers often want to find the type of an expression somewhere in the depths of a program. There are ways of doing this now, but none of them are satisfactory.

# Goals in Agda


One of the features of the Emacs mode for [ Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) is the ability to add goals, as a placeholder for code that is yet to be written. By inserting a `?` in an expression, the compiler will introduce a goal. After loading the file (which typechecks it), Agda gives an overview of the goals in the file and their types.


For example:

```wiki
test : List Bool
test = Cons ? (Cons ? Nil)
```


Gets turned into:

```wiki
test : List Bool
test = Cons { }0 (Cons { }1 Nil)
```


With extra output:

```wiki
?0 : Bool
?1 : Bool
```


As can be seen here, goals are numbered, and the typechecker returns the inferred type for each of these goals.


These goals can make it a lot easier to write code. They allow typechecking to continue although certain parts of code are missing and they work as a TODO list.

# How we can almost do this in GHC now


GHC does not support holes in the way Agda does. It is possible to insert `undefined` in an expression to make it typecheck (which Agda doesn't have), but this is not very helpful when writing software. Inserting `undefined` only gives as information that the rest of the program typechecks, but will not help you find what you needed to use in its place. We propose to add an extension to GHC (and notably GHCi) to allow using holes, this page is meant to discuss the exact features and workflow of such an extension.


First, two existing features that can be used as holes.

### `undefined`


As stated before, `undefined` typechecks just like a hole: it has type `forall a.a`, so it can be used anywhere. However, it is not very easy to use in this way: it is impossible to find out what type the compiler found for the hole, and it's impossible to get a list of all the holes used in your source file(s).


A similar example:

```wiki
test :: [Bool]
test = undefined : (undefined ++ [])
```


Will not help finding the types of the `undefined`s at all. One advantage is that the code can successfully run, except if one of the `undefined`s is actually evaluated.


To find the type of the `undefined`, one can give it a type that is certainly wrong, and then check what the compiler says the right type is:

```wiki
test :: [Bool]
test = undefined : ((undefined :: ()) ++ [])
```


Gives:

```wiki
test.hs:2:22:
    Couldn't match expected type `[Bool]' with actual type `()'
    In the first argument of `(++)', namely `(undefined :: ())'
    In the second argument of `(:)', namely `((undefined :: ()) ++ [])'
    In the expression: undefined : ((undefined :: ()) ++ [])
Failed, modules loaded: none.
```


However, using multiple of these holes can cause the compiler to assume the error is in a different place than the hole. It also fails the compilation, causing other errors that might occur to not show up, and doesn't allow the rest of the code to still run. 

### Implicit Parameters


The GHC extension [Implicit Parameters](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#implicit-parameters) comes closer to how holes are expected to work. This extension makes it possible to specify a term with a question mark, denoting a implicit variable, but this could also be seen as a hole.


Same example:

```wiki
test = ?a : (?b ++ [])
```


Inspecting the type of `test` when defined in GHCi now shows the types of the (unbound) implicit parameters:

```wiki
> :t let test = ?a : (?b ++ []) in test :: [Bool]
let test = ?a : (?b ++ []) in test :: [Bool]
  :: (?a::Bool, ?b::[Bool]) => [Bool]
```


However, defining `test` like this in a module gives the following error:

```wiki
test.hs:4:8:
    Unbound implicit parameter (?a::Bool)
      arising from a use of implicit parameter `?a'
    In the first argument of `(:)', namely `?a'
    In the expression: ?a : (?b ++ [])
    In an equation for `test': test = ?a : (?b ++ [])

test.hs:4:14:
    Unbound implicit parameter (?b::[Bool])
      arising from a use of implicit parameter `?b'
    In the first argument of `(++)', namely `?b'
    In the second argument of `(:)', namely `(?b ++ [])'
    In the expression: ?a : (?b ++ [])
Failed, modules loaded: none.
```


This will show you the type, however, it is an error and aborts compilation, so there may be other problems you don't get to see because of it. It also will refuse to load and compile the module, so it's impossible to run the parts of it that are finished.


The reason is that the hole becomes a part of the type signature, as a constraint. So to correctly use it here, the function would have to be written as:

```wiki
test :: (?a::Bool, ?b::[Bool]) => [Bool]
test = ?a : (?b ++ [])
```


This makes it very impractical to use them as holes, as all type signatures have to be updated to let the typechecker continue. Not only in the functions that use the implicit parameter itself, but they propagate upwards, just like class constraints: if another function were to call `test`, it would have the same implicit parameters (and therefore, all of these type signatures would have to be updated when a new hole is added).


Another thing to keep in mind with implicit parameters is that implicit parameters with the same name in different functions are not assumed to be the same (i.e., required to be unifiable), *except* if some function has both implicit parameters in its constraints. Lastly, it's impossible to run code with unbound implicit parameters, even if the parameters are never actually used.

# How this could be implemented in GHC


These two approaches are not ideal, they either don't give enough information, or they hinder using the code.

### Agda-style


The simplest way would be to implement them in the same way as Agda: add a new syntax (we shall use two underscores as an example here, `__`) to denote a hole, and after typechecking, show the user a list of all the types of all the holes in their source files. In what cases "after typechecking" we do this is still subject of discussion. We expect at the very least to show it after (re)loading a module into GHCi or typechecking an expression in GHCi directly (`:t`).


Example:

```wiki
test :: [Bool]
test = __ : (__ ++ [])
```


Theoretical output:

```wiki
> :l test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )
Found a hole at test.hs:2:6-7: Bool
Found a hole at test.hs:2:12-13: [Bool]
> 
```

### Named holes


Implicit parameters have some good features too: they can be named, and so used in multiple places. In the Agda-style, this would require let-binding a hole, which is a lot of effort for something that should be a temporary placeholder. So one idea is to allow giving holes a name, just like implicit parameters.


For example:

```wiki
test :: [Bool]
test = _a : (_b ++ [])

test2 = _c ++ _b
```


Theoretical output:

```wiki
> :l test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )
Found a hole _a: Bool
Found a hole _b: [Bool]
Found a hole _c: [Bool]
> 
```


These could either be made shared within all functions in module, or not shared between functions at all (so not the confusing situation with implicit parameters, which are only shared when required).

### Not holes, ranges


Holes can be useful for finding the type of something that still needs to be written, but a more general way of looking at it is this: it is currently quite easy to typecheck a complete expression, for example with `:t`, in GHCi. However, finding the type of a part of an expression within a module is hard. In a large, complicated function it could be useful to ask the compiler for the types of certain subexpressions. `:t` does not help here, as the function's parameters and where/let/lambda-bound terms are not in scope. It would be useful if it were possible to annotate code to ask the compiler to give you the type found for the annotated expression.


Simple example (let `{_ _`} denote a request for the type here):

```wiki
test :: [Bool]
test = {_ undefined _} : ({_ undefined ++ [] _})
```


Could result in:

```wiki
> :l test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )
Found type of undefined (test.hs:2:11-19): Bool
Found type of undefined ++ [] (test.hs:2:30-38): [Bool]
```


The same effect of holes can then be achieved by using ` {_ undefined _} `. To return to the conciseness of holes, `__` could be syntactic sugar for `{_ undefined _`}. (Note that defining `__ = {_ undefined _`} in Haskell would not do this. The type would be `forall a. a`.)
