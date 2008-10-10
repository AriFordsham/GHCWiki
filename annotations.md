# Annotations

## Motivation


Annotations are useful for both [Plugins](plugins) and for users that would like to introspect more carefully on other, already-compiled modules.

## State Of Play


An annotations system was implemented as part of the Compiler Plugins Summer of Code project (Summer 08). It is currently in the process of being revised before being committed to GHC HEAD. Details of the implementation are below.

## Summer of Code Implementation


Annotations look like this:

```wiki
{-# ANN f 1 #-}
f = ...

{-# ANN g Just ("foo", 1337) #-}
g = ...
```


I.e. you annotate an actual identifier with an expression. We impose no constraints on the form of the expression. In particular, general application is allowed:

```wiki
{-# ANN f id 1 #-}
```


You can introduce compile time compilation by using Template Haskell as usual:

```wiki
{-# ANN f $(id [| 1 |]) #-}
```


We will need another notation to be able to refer to identifiers other than data constructor and function names, something like:

```wiki
{-# ANN type Foo Just 2 #-}
data Foo = Foo
```


We also allow:

```wiki
{-# ANN module "I am an annotation" #-}
```


In your annotation, you may refer to the names of things in the modules being compiled, but not their implementations:

```wiki
data Foo = Foo ...
f = ...
g = ...

-- OK:
{-# ANN f Just ('g, ''Foo) #-}

-- Not OK:
{-# ANN f Just (f, Foo) #-}
```


Notice that this notation does not allow annotating non-top-level names or expressions, and due to its use of an Id -\> Annotation mapping may not survive inlining (this only matters to plugins, not users of annotations on exported stuff). These are annoying limitations but it does fit with our view that the annotations should be exportable and accessible by an Id -\> Annotation mapping by other modules.


Evaluation of the actual value of the annotation is done at the point they are accessed by the compiler.

## Evolved Implementation


The Summer of Code implementation had some drawbacks:

- You don't detect non-terminating or otherwise "wrong" annotations at compile time

- It is difficult to specify an interface that allows adding annotations

## Future Work

- Plugins cannot currently add further annotations during compilation that will be compiled into the result. I.e. any annotations they add are transient and disappear at the end of that particular run of the Core pipeline.

- We might want to add attribute metadata, so users can specify the multiplicity attributes should take, what sorts of things they can be attached to (value, type, module), and perhaps even what types they can be attached to (e.g. "only things of type a -\> Bool for some a"), similar to C\# ([ http://msdn.microsoft.com/en-us/library/tw5zxet9(VS.80).aspx](http://msdn.microsoft.com/en-us/library/tw5zxet9(VS.80).aspx)) or Java.

- We might want to extend annotation syntax so you can attach multiple annotations in a single definition, like so:

```wiki
{-# ANN f, g, x Foo #-}
f = ...
g = ...
x = ...
```

- It might be nice to be able to write annotations in more places:

  - Exports
  - Function parameters
  - Expressions (for plugins, similar to SCCs)
  - Fields of data/newtype declarations
  - Non-top-level identifiers (for plugins: tricky because such names are unstable)

- I believe it would make sense to allow annotations to use the implementations of values in the module being compiled,: after all, I they can use the implementations of values in imported modules. This would filling out the relevant field with an error during compilation (for the benefit of plugins) and linking the annotation fields up to the required values after compilation. Is this a good idea?

- Have retention policies, similar to Java?
- See also the Haskell Prime ticket: [ http://hackage.haskell.org/trac/haskell-prime/ticket/88](http://hackage.haskell.org/trac/haskell-prime/ticket/88)