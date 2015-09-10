## Custom Type Errors


This page outlines the design for a GHC feature to provide support for user-specified type errors.
The main idea was presented by Lennart Augustsson at the 2015 Haskell Symposium in Vancouver BC. 

### The Problem


When designing EDSLs in Haskell, it is useful to have something like `error` at the type level.
In this way, the EDSL designer may show a type error that is specific to the DSL, rather than the standard GHC type error.


For example, consider a type classes that is not intended to be used with functions, but the EDSL user accidentally used
it at a function type, perhaps because they missed an argument to some function.  Then, instead of getting a the standard
GHC message about a missing instance, it would be nicer to emit a more friendly message specific to the EDSL.


Similarly, the reduction of a type-level function may get stuck due to an error, at which point it would be nice to
report an EDSL specific error, rather than a generic error about an ambiguous type or something.

### A Solution


One way to solve the above problem is by adding a single uninterpreted type-function as follows:

```
typefamilyTypeError(msg ::ErrorMessage):: k
```


Note that `TypeError` is polymorphic in the kind of result it returns, so it may be used in any context.  In this respect it resembles to polymorphic value `error`.  
The intention is that users will never define instances of `TypeError`, so one may think of it as closed type family with no equations.  In other words, `TypeError`
will never evaluate to anything, and it is basically very similar to the polymorphic type-level value `Any`, which is already present in GHC.


The kind `ErrorMessage` is a small DSL for constructing error messages, and may be defined using a promoted datatype:

```
data{-kind-}ErrorMessage=TextSymbol-- Show this text as is| forall t.ShowType t               -- Pretty print a type|ErrorMessage:<>:ErrorMessage-- Put two chunks of error message next to each other|ErrorMessage:$$:ErrorMessage-- Put two chunks of error message above each other
```