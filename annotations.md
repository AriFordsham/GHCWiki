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

- It is difficult to specify an API that allows adding annotations to the program being compiled because that API needs both a value (that represents the annotation to plugins that may be later on in the pipeline) and a Core expression (that is inserted into the actual target program). What's more, you need to ensure that you are able to find a Typeable dictionary in the program being compiled that corresponds to the dictionary supplied by the user of the API.

- Modules must refer to all the packages that contain data structures used in their annotations. This may be a particular problem if you wish to annotate values with things from the GHC package.


A better implementation is described in this section.


When compiling user-declared annotations (i.e. those in the actual text of the program being compiled):

1. The type checker finds the Binary and Typeable instance for the annotation expression, *e*
1. The expression *e* is evaluated and serialized by the compiler using the Binary instance to obtain a sequence of bytes *b*
1. The TypeRep for *e* is obtained using the Typeable instance and transformed into a rose tree of strings (i.e. stripping out the Keys that will vary between GHC runs)
1. The rose tree and *b* are both put into the interface file in the section recording annotations for the particular name under consideration


When a plugin adds another annotation:

1. At the time the plugin is compiled the type checker finds a Binary and Typeable instance for the expression being added as an annotation, *e*
1. Compilation proceeds as in the user-declared case


When it comes time to read annotations back out in a GHC API client:

1. At the time the client is compiled the type checker finds a Binary and Typeable instance for the target type of annotations
1. The Typeable instance is turned into a rose tree as before and that tree is compared with those trees stored in the interface file being queried for any matches
1. The byte sequences corresponding to those matches are deserialized according to that Binary instance


The key difference is that in this version **dictionaries are not referenced by the files that store annotations** - instead they are provided at the read site. Crucially, this assumes that the Binary and Typeable instances available at the two different times are coherent - and this is not enforced anywhere (you could end up with incoherence if you have some orphan instances). However, it does enable almost all the problems mentioned above to be overcome.


A disadvantage to this approach is that it does not allow functions to be stored in annotations. Under the SoC system, this was allowed:

```wiki
{-# ANN f (\x -> x) #-}
```


But clearly there is no sensible Binary instance for *a -\> b* in general.


Another disadvantage is that currently there is no standard Binary class in the Haskell libraries. We propose to add such a beast by adding [ Data.Binary](http://code.haskell.org/binary/) as a boot library and supporting automatic instance deriving for it in GHC. The advantages of doing this rather than somehow reusing GHCs inbuilt Binary class are that:

- It is compiler independent

- Much user code already has instances for Data.Binary (it is a popular Hackage package)

- The binary package will be distributed with the Haskell Platform anyway because it is depended on by cabal-install


Disadvantages are that:

- It adds another bytestring dependency to GHC (we already depend on bytestring but there was a suggestion that we would stop doing so)

- GHCs own Binary instances for it's own data types cannot be reused straightforwardly. We contend that there are few cases where doing so would be useful, and if e.g. you want to refer to another thing rather than use a GHC Name you can use a TH Name. It would be difficult to piggyback the GHC Binary instance on Data.Binary because there is nowhere in Data.Binary to squirrel away the UserData GHCs Binary instance needs to do some of its stuff (but maybe you could deserialize to a function of type *UserData -\> Iface\** to achieve the same effect...)

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