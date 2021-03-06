# Annotations

## Motivation


Annotations are useful for both [Plugins](plugins) and for users that would like to introspect more carefully on other, already-compiled modules. The concept is substantially similar to the annotation systems in such popular languages as C\# and Java. Some use cases might be:

- Mark functions for modification by [Plugins](plugins)

- Store extra information relevant to a plugin but which is added by that plugin itself, such as a strictness analysis plugin that adds a demand signature to functions, which can be inspected later on by the same plugin when it comes to compile modules that depend on that one

- Specify extra documentation or comments for functions / types in the source code that could be extracted / nicely formatted by a later program

- Mark certain functions as tests, suitable for execution by a QuickCheck runner


There is ongoing work to make Annotations usable from [TemplateHaskell](template-haskell), not just from Plugins.  This can be found at [wiki:TemplateHaskell/Annotations](template-haskell/annotations).

## State Of Play


An annotations system was implemented as part of the Compiler Plugins Summer of Code project (Summer 08). It has been submitted to the HEAD, and will appear in GHC 6.12 and later.  Documentation is available at [http://downloads.haskell.org/\~ghc/latest/docs/html/users_guide/extending_ghc.html\#source-annotations](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#source-annotations).


The annotation system committed in the end is very simple. Basically, you can write things like this:

```wiki
{-# ANN x e #-}
```


Where x is an identifier and e is an expression with a Data instance (used only for serialization - a bit of a wart). You can also annotate types and modules in the same manner:

```wiki
{-# ANN type T e #-}
{-# ANN module e #-}
```


The expressions e are all evaluated at compile time, and subject to the same restrictions as spliced expressions in Template Haskell.


Annotations are accessible via the GHC API by doing something such as this (untested):

```wiki
import Serialized
import Annotations
import GHC

findAnnsForNamedThing nm = findGlobalAnns deserializeWithData (NamedTarget nm)
```

## Why The Implementation Is The Way It Is

- The interface-file loader (part of the GHC library) can't deserialise because it doesn't have an appropriate deserialiser for every type -- a client of the GHC library might be using one unknown to GHC.  That's why Serialized is a pair (TypeRep, \[Word8\])
- Any particular client, such as [SpecConstr](spec-constr), is interested in annotations of a particular type (let's say just one for now).  That type, T, is known to the client, of course.


So the client can take the AnnEnv:

```wiki
    AnnEnv = NameEnv [Serialized]
```


and convert it to:

```wiki
     MyAnnEnv = NameEnv T
```


That is, find all the Serializeds whose TypeRep matches T, and deserialise them.


The general function is something like:

```wiki
deserialiseAnnEnv :: Typeable t => ([Word8] -> t) -> AnnEnv -> NameEnv t
```


This function could be in main/Annotations.lhs.


## OLD Implementation Notes



**
**



In this old implementation, annotations look like this:

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


A better implementation is described in this section.  The programmer's eye view is largely unchanged: she still declares annotations in the same way.


From the point of view of a client of the GHC API or a plugin, the interface looks like this:

```wiki
-- For normal GHC API users:
getAnnotations :: (Typeable a, Binary a) => Name -> GHCM [a]
   -- Get the annotations for an arbitrary Name

getAnnotations :: (Typeable a, Binary a) => Name -> CoreM [a]
   -- Allows a plugin to get the annotations for any Name
   -- whether defined locally or imported

putAnnotations :: (Typeable a, Binary a) => Name -> a -> CoreM ()
   -- Allows a plugin to add its own annotation 
   -- e.g. the results of an analysis
   -- The Name must be from the module being compiled; the 
   -- annotation is persisted into the interface file.
```


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


A further disadvantage is that you can't now just pull out **all** annotations (of any type) for a name (as you can in the SoC design, using a list of Dynamics). We can't allow this even if we had a DynamicBinary type that encapsulated the Binary dictionary too because those dictionaries really do have to be supplied by the guy doing the pulling!


Yet another disadvantage is that we will be doing some compile time compilation without it being introduced with the $() syntax that has heralded all such computation previously - instead, annotations are wrapped in a sort of "implicit splice". This might cause user confusion.

### Serialization Issues

#### Alternative 1 (Proposed)


Another disadvantage of this new annotations scheme is that currently there is no standard Binary class in the Haskell libraries. We propose to add such a beast by adding [Data.Binary](http://code.haskell.org/binary/) as a boot library and supporting automatic instance deriving for it in GHC. The advantages of doing this rather than somehow reusing GHCs inbuilt Binary class are that:

- It is compiler independent

- Much user code already has instances for Data.Binary (it is a popular Hackage package)

- The binary package will be distributed with the Haskell Platform anyway because it is depended on by cabal-install

- We can provide *deriving (Binary)*


Disadvantages are that:

- It adds another bytestring dependency to GHC (we already depend on bytestring but there was a suggestion that we would stop doing so)

- GHCs own Binary instances for it's own data types cannot be reused straightforwardly. We contend that there are few cases where doing so would be useful, and if e.g. you want to refer to another thing rather than use a GHC Name you can use a TH Name. It would be difficult to piggyback the GHC Binary instance on Data.Binary because there is nowhere in Data.Binary to squirrel away the UserData GHCs Binary instance needs to do some of its stuff (but maybe you could deserialize to a function of type *UserData -\> Iface\** to achieve the same effect...)

- By adding binary as a boot library distributed with GHC it will be impossible to upgrade binary separately from GHC itself without introducing problems. Consider packages P and Q, the binary package distributed with ghc (binary-1) and that installed at a later point by a user (binary-2), and the following web of dependencies:

```wiki
           P
           |\____
          /      Q
        ghc       \
        /       binary-1
    binary-2
```


P depends indirectly on both binary versions. What happens if it tries to use an instance of Binary from binary-1 with ghc, or an instance of Binary from binary-2 with Q? They won't unify, so this is a (rather confusing) type error! Argh! What's more, this really might happen because Binary is exported in the API to access annotations:

```wiki
-- For normal GHC API users:
getAnnotations :: (Typeable a, Binary a) => Name -> GHCM [a]

-- Only for plugins adding their own annotations:
getAnnotations :: (Typeable a, Binary a) => Name -> CoreM [a]
putAnnotations :: (Typeable a, Binary a) => Name -> a -> CoreM ()
```


We have sort of the same problem outlined above even today because bytestring is a boot library. However, since bytestring isn't exported by GHC you don't end up with this sort of weird situation. (Though it might give Cabal as much of a headache as the binary problem outlined above).

#### Alternative 2


The main alternative to using binary as a boot package is to import the whole source tree for binary and bytestring but rename them to ghc-binary and ghc-bytestring respectively. This ensures there are no conflicts with user code, but:

- Might be confusing!

- Means user code might have to make use of the package-qualified import syntax if they intend to reference both ghc-binary and binary from their code

- Means we have to have two identical instance declarations for any user types that want to be used both as normal Binary and GHC Binary

- Means we can't support *deriving Binary*

- Etc etc...

#### Alternative 3

**Update: this doesn't actually work because we need to look up the serialization function for user-defined annotations'''
**


The other alternative we came up with is to change the GHC API to annotations to this:

```wiki
-- For normal GHC API users:
getAnnotations :: (Typeable a) => (ByteString -> a) -> Name -> GHCM [a]

-- Only for plugins adding their own annotations:
getAnnotations :: (Typeable a) => (ByteString -> a) -> Name -> CoreM [a]
putAnnotations :: (Typeable a) => (a -> ByteString) -> Name -> a -> CoreM ()
```


This doesn't require any more packages (we could even remove the ByteString usages here) and it does allow use of multiple different serialization libraries for annotations (should that be desirable). However, this makes the API a bit unfriendlier; and (more important) doesn't enforce that the serializer and deserializer for a particular bit of data should be coherent.  For example, the following could easily happen:

- A plugin uses binary-2.1 to serialise stuff into Foo.hi
- A GHC api client uses binary-3.0 to deserialise it (probably without realising that they are using the "wrong" Binary)


Result: extreme broken-ness at run time (not compile time).

#### Alternative 4


We could use Data to implement serialization, and require that instead of Binary. Advantages are manifold (credit goes to Neil Mitchell for this list):

- You don't get any additional dependencies (other than perhaps SYB, which I hope you are going to add for the GHC API data types anyway).

- You can control the format of the serialisation perfectly, and in a version independent way.

- You can add type structure checksums to stop changed types with the same name being messed around - which should eliminate all heisenbugs from stale .hi files.

- Everyone can write deriving Data/Typeable with automatic deriving.

- Data/Typeable seems a much more natural pair of instances than Typeable/Binary

- Data has been around and solidified for a lot longer time period, and isn't likely to change in incompatible ways.

- You can serialise things into XML etc. without any interface change, should your requirements change.


There are some disadvantages:

- Serialisation will be slower. Not massively slower, but a bit. And hopefully it isn't a bottleneck anyway.

- Datas gunfold won't be defined for abstract data types or those that omit some fields from gfoldl. This actually bites GHC, because the Template Haskell NameFlavour type had an abstract Data instance like that! I've worked around it by making the Data instance expose the internals of NameFlavour

## Future Work

- The syntax is a bit verbose. What about this?

```wiki
{-@ f Just "My Annotation" @-}
f = ...
```

- Also syntax related, we could provide a positional syntax. SPJ suggests that the annotations should be hung on the type signature rather than the definition because the definition must be spread out, whereas the type signature occurs in precisely one place. This might make the above example even nicer:

```wiki
{-@ Just "My Annotation" @-}
f :: ...
```

- **(Only in SoC implementation)** Plugins cannot currently add further annotations during compilation that will be compiled into the result. I.e. any annotations they add are transient and disappear at the end of that particular run of the Core pipeline.


 


- We might want to add attribute metadata, so users can specify the multiplicity attributes should take, what sorts of things they can be attached to (value, type, module), and perhaps even what types they can be attached to (e.g. "only things of type a -\> Bool for some a"), similar to C\# ([http://msdn.microsoft.com/en-us/library/tw5zxet9(VS.80).aspx](http://msdn.microsoft.com/en-us/library/tw5zxet9(VS.80).aspx)) or Java.

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

- **(Only in SoC implementation)** I believe it would make sense to allow annotations to use the implementations of values in the module being compiled,: after all, I they can use the implementations of values in imported modules. This would filling out the relevant field with an error during compilation (for the benefit of plugins) and linking the annotation fields up to the required values after compilation. Is this a good idea?

- Have retention policies, similar to Java?
- See also the Haskell Prime ticket: [http://hackage.haskell.org/trac/haskell-prime/ticket/88](http://hackage.haskell.org/trac/haskell-prime/ticket/88)
