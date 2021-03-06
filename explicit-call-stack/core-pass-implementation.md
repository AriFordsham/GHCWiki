# Explicit Call Stack Implementation


As part of an MSR internship, a prototype implementation of some of the explicit
call stack passing transform has been done within GHC as a new pass.  This page
outlines an example of usage, sketches some implementation details should
someone want to take it further, and details future work / problems that need
addressing.

- [ExplicitCallStack](explicit-call-stack) - read this to get an overview of the idea
- [Blog post giving an overview](http://ghcmutterings.wordpress.com/2008/12/04/explicit-stack-traces/)
- [Darcs repository with paper sources and patches](http://code.haskell.org/explicitCallStackPaper/)
- [Paper describing the work submitted to Haskell Symposium 2009](http://pubs.doc.ic.ac.uk/finding-the-needle/)


 


## Examples


There are several examples in the testsuite patch, and also in the blog post
above.  The gist is:


Imagine you have the following source program:

```wiki
module Main where

import Control.Exception

main :: IO ()
main = do
  baz `seq` return ()

bar str = error str
baz = bar "crash!\n"
```


Compile and run this and the program will predictably fail with an error
message.

```wiki
$ ~/ghc/ghc/ghc/stage2-inplace/ghc.exe --make -o Eek Eek.hs
$ ./Eek.exe 
Eek.exe: crash!

```


To build up a call stack, the program needs a bit of rewriting to the following
form:

```wiki
{-# OPTIONS_GHC -fexplicit-call-stack #-}
module Main where

import GHC.ExplicitCallStack.Annotation
import GHC.ExplicitCallStack.Stack
import Control.Exception

main :: IO ()
main = do
  baz `seq` return ()

{-# ANN bar Debug #-}
bar str = throwStack (\s -> ErrorCall $ str ++ show s)

{-# ANN baz Debug #-}
baz = bar "crash!\n"
```


The changes needed are:

- indicating we want to run the explicit-call-stack pass by telling GHC,
  either via an options pragma or compile time flag.

- importing `GHC.ExplicitCallStack.Annotation` to bring the annotation
  `Debug` into scope.

- importing `GHC.ExplicitCallStack.Stack` to bring `throwStack` into scope

- indicating that `bar` and `baz` should be rewritten to explicitly
  pass the stack by decorating them with a `Debug` annotation

- replacing `error` with the primitive `throwStack` that can access
  the Stack and throw an exception


Compile and run now, and you get:

```wiki
$ ./Eek.exe 
Eek.exe: crash!
in bar, Eek.hs:13,11
in baz, Eek.hs:16,7
in main, Eek.hs:10,3
```


Alternatively, the user can indicate instead that all functions should be
rewritten to pass a stack (`-fexplicit-call-stack-all`), and thus not require
the annotations:

```wiki
{-# OPTIONS_GHC -fexplicit-call-stack-all #-}
module Main where

import GHC.ExplicitCallStack.Stack
import Control.Exception

main :: IO ()
main = do
  baz `seq` return ()

bar str = throwStack (\s -> ErrorCall $ str ++ show s)

baz = bar "crash!\n"
```


Which gives a slightly different output of:

```wiki
$ ./Eek.exe 
Eek.exe: crash!
in bar, Eek.hs:11,11
in baz, Eek.hs:13,7
in main, Eek.hs:9,3
in main, Eek.hs:8,1
```


as `main` has also been rewritten to accept a stack...

## Implementation

### Flags


Several flags have been added in the patch:

- `-fexplicit-call-stack` which actually enables or disables the core
  pass.  It also implies `-fno-method-sharing` which makes the core look a
  little more like you would intuitively expect and thus easier to write the
  transform for.

- `-fexplicit-call-stack-all` which tells the core pass to ignore
  `Debug` annotations and just rewrite every top level binding.  It also
  implies `-fexplicit-call-stack` to enable the core pass, and
  `-fno-method-sharing` as above.

- `-ddump-ecs` which dumps the core after the explicit call stack phase
  has run

- `-fds-simple` which causes the desugarer to massivly simplify how it translates mutually recursive functions with typeclass/type parameter arguments.

### Annotations


The pass uses GHC [Annotations](annotations) for two purposes.  The first is to allow the
user to guide which functions get rewritten for debugging purposes.  The second
is to record the link between the original function, and it's debug-rewritten
form.


The annotations live in the module `GHC.ExplicitCallStack.Annotations`
that have been added to the package `template-haskell`.  (There is a
dependency on TH `Name`s in the annotations hence this package).


This module is tiny:

```wiki
{-# LANGUAGE DeriveDataTypeable #-}
module GHC.ExplicitCallStack.Annotation where

import Language.Haskell.TH
import Data.Data

data Debug = Debug
  deriving (Data, Typeable)
data Debugged = Debugged { fromDebugged :: Name }
  deriving (Data, Typeable)
```


We need `Data` and `Typeable` so the `Debug` and `Debugged`
constructors can be serialized using the default annotation serialization
scheme.

- The `Debug` annotation we have already seen, and is used by the core pass to
  establish which functions should have a stack accepting form generated.

- The `Debugged` annotation is placed onto the core during the explicit call
  stack pass.  The annotation is associated with the original function, and the
  `fromDebugged` field is given the template-haskell `Name` of the version
  of the function that accepts and threads through the call stack.


  


### Source locations


In order to give helpful stack traces, the source locations of program variables
had to be persisted into GHC Core somehow.  The chosen route for this was to add
a new type of Core Note:

```wiki
data Note
  = ...
  | CoreSrcLoc SrcSpan
```


These are then sprinkled into the core by the desugarer see (`dsExpr`.
Basically every variable reference becomes wrapped in a {{{Note (CoreSrcLoc loc)
_}}}.


A utility pass was also written and added to `simplCore/` called
`StripSrrcLocNote` that removes all these notes again.  This is used in e.g.
desugaring `RULES` to make sure the patterns don't get clobbered.


However this is a bit messy.  It could be better to add an explicit phase in the pipeline which goes


HSSyn --desugarer--\> Expr SrcLocAnnotatedVar --passes that safely work with src locs--\> Expr SrcLocAnnotatedVar --strip anns--\> CoreExpr --rest of pipeline--\> ...


Alternatively, HPC works fine on HSSyn, maybe that'd be a better target for this work.

### The transform


The transform itself lives in `callStackCore/SimpleDebugTranslate`.  The
entry point is `stackPassTransform`.  Beacause of the need for annotations
(and hence transitively template haskell), most of the module is conditionally
compiled with an `#ifdef GHCI`.


The pass transforms a module by the following:

- Building up a mapping of variable names in the current module that are to be
  _debugged_ (i.e. to have a stack-accepting variant generated), and
  allocating the names for these debugged variants. `buildLocalDebuggedMap`

- Actually doing the rewriting of all bindings in the module
  (`processBind`)

  - Those bindings that are to have a stack-accpeting variant generated have
    that generated, and their original expression is rewritten to forward to
    the new variant.
  - All other bindings are rewritten so that any references to any functions
    with `Debugged` annotations instead call the `fromDebugged` field.

- Adding all of the new `Debugged` annotations into the current pipeline
  and into the `ModGuts` for persisting into the interface file.
  (`mkAndSetAnnotations`).


The actual rewriting performs a fairly dumb transform:

```wiki
f = e
```


will be rewritten to

```wiki
f = f' (pushStack #currentsrcloc# emptyStack)
f' = \stack -> [[e]]_stack
```


where `[[e]]_stack` replaces any variable `x` with a `Debugged x'` annotation
with `x' (pushStack #currentsrcloc# stack)`.


The rewriting also recognises the special function `throwStack` and replaces
it with roughly `\f -> throw (f stack)`.

### Stacks


The library for the actual runtime callstacks lives in `GHC.ExplicitCallStack.Stack` in
package `base`.  The stack has been designed to abstract away recursion
(since otherwise that would kill performance/memory usage given that iteration
has to use recursion).


The high level view of the stack design is this:


Imagine you have a real call stack, e.g.

```wiki
error in top!
      in a
      in b
      in c
      in c
      in c
      in d
      in b
```


Our abstracted call stack would be built by keeping the first occurance of any
element (from the top of the stack), and replacing any later occurences by a
sentinal value (hereafter `...`).  These sentinal values collapse together
if they would ever be adjacent.


i.e, walking down the above stack from the top, we would build up:

```wiki
                                                                      in top!
                                                            in top!   in a
                                        in top!   in top!   in a      in b
                              in top!   in a      in a      in b      in c
                    in top!   in a      in b      in b      in c      ...
          in top!   in a      in b      in c      in c      ...       in d
in top!   in a      in b      in c      ...       ...       in d      ...
```


                                                         
Alternatively, building up as though we were pusing elements onto the stack one
by one:

```wiki
                                          in top!
                                    in a  in a
                              in b  in b  in b
                  in c  in c  in c  in c  in c
            in c  ...   ...   ...   ...   ...
      in d  in d  in d  in d  in d  in d  in d
in b  in b  in b  in b  in b  ...   ...   ...
```


The stack data structure represents the sentinal elements implicitly by having
two constructors to represent an element on top of another stack; `Then`,
and `RecursionThen` (for the sentinal).  Loosly, our final stack in the
examples above would be:

```wiki
top `Then` a `Then` b `Then` c `RecursionThen` d `RecursionThen` Empty
```


In order to prevent a large number of stacks being allocated and held live, each
stack holds an `MVar` to a `Hashtable` of stack elements to new stacks.
This table is used to memoise push calls.

## Open Problems


There are several open problems and incomplete corners in the implementation


Some are mentioned here, they are elaborated on more fully in the paper, linked above.

### Bootstrapping


It would be useful if many prelude functions (c.f. `error` and a few of it's
cousins) could have `Debugged` variants available, so e.g. `fromJust` or
`head` errors could be detected more easily.


However, In order for the pass to work, it requires at least a stage 2 compiler.
This is because the annotations system uses template haskell in order to
evaluate annotation expressions.  This means that the libraries would need
rebuilding with the stage 2 compiler, which may make things a bit messy.  It may
also induce some horrible dependencies between `base` and
`template-haskell` - (`Prelude` needing to import
`GHC.ExplicitCallStack.Annotations`, etc.

### Records


Partial record selectors are currently written with a specialsed error function.
The pass could try and special case to detect these, or a solving of the
bootstrapping problem above would allow a debugged version of the specialised
error function to be generated.

### Type Classes


Type classes provide many interesting problems; some theoretical, some pragmatic

- I have a section outlining some approaches in the paper

### Source locations in `AbsBinds`

`AbsBinds` currently don't store source locations for the binding group
accurately.  This shows up in a couple of the tests in the testsuite in the
patch.  I havn't had chance to investigate this properly, but it should be
relatively straightforward to fix.

### Late Debugging


There is currently a big open question about how or whether it would be possible
to create debugged versions of functions without recompiling / changing that
module.  Maybe I have a library and I want to pass a call-stack down inside
there.

### Higher order functions


Also discussed in the paper
