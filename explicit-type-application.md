# Syntax for explicit type and kind application


We propose a replacement and generalisation of [lexically-scoped type variables](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#scoped-type-variables) (and pattern signatures) that is
more clear and direct by allowing explicit type (and kind) application.
We propose the concrete syntax `@ tyvar`, like in the following example:

```wiki
case x of
  (C @a y z) -> ....
```


On the right-hand side we would have the type variable `a` in scope for use on 
any type signatures.


Note how the use of the symbol `@` is (in this case) unproblematic; we can
use the fact that constructors always start with an uppercase letter to distinguish
whether the `@` refers to an "as pattern" or to a type application:

```wiki
case x of
  p@(C @a y z) -> ....
```


Unfortunately this is not always the case; see below.


Note that this proposal would not allow pattern matching on specific types:
the only thing that we can match on are type or kind variables. However, it
does allow for specifying what type to apply:

```wiki
id @Int 2
```


The idea is to provide access to the explicit types in the core language
(system [ FC-pro](http://dreixel.net/research/pdf/ghp.pdf))
directly from the source language syntax.

## How many arguments, and their order


When we have multiple variables we can pattern match on as many as we need,
and also use underscores:

```wiki
f (C @_ @b x ) = ...
```


If the user gave a type signature for the binding, it is very easy to see
which type patterns relate to which variables in the signature. In the absence
of a signature, though, there are two possible choices:

- Reject matching on type variables altogether.

- Take the inferred signature, look at the introduced variables syntactically
  from left to right, and use that order. This approach does not require tracking
  which bindings were given type signatures or not.


A problem with taking the inferred signature is that it is tied to
many assumptions, including that of principal types.
\[Dimitrios: Can you expand on this?\]

## Parsing matters

### Ambiguity


Consider a problematic example:

```wiki
f :: Int -> forall a. a
f n @a = ....
```


In this case it is really ambiguous what the pattern means. For these
cases we suggest the following workaround:

```wiki
f :: Int -> forall a. a
(f n) @a = ....
```


This approach should work in general, and hopefully only few programs will
actually need to use it.

### Syntax for promoted datatypes


With `-XPolyKinds` on, we can also match/apply kind arguments. This introduces the
need to disambiguate between a datatype and the promoted kind it introduces.
Consider the example:

```wiki
data X = X

f :: forall (a : k). ....
... = ... f @'Nat @Nat ...
```


Since now it is not clear from the context anymore if we are expecting a kind
or a type (since we want to use `@` both for kind and type application), we need to be
able to disambiguate between datatypes and their corresponding promoted kinds.
At the moment this ambiguity does not arise, so we do not allow prefixing
kinds with `'`, but it seems natural to lift this restriction, and use the
same notation as for promoted data constructors.

## More examples

### Impredicative instantiation


This extension also allows for clear impredicative instantiation. For instance,
the application of the list constructor `(:) @(forall a. a -> a)` means
the constructor of type
`(forall a. a -> a) -> [forall a. a -> a] -> [forall a. a -> a]`.

### Type/kind instantiation in classes


With the new kind-polymorphic `Typeable` class, we can recover the old
kind-specific classes by writing, for example:

```wiki
type Typeable1 = Typeable @(* -> *)
```