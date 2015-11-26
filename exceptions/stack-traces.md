# Plan for providing exception stack-traces


Exception handling in modern Haskell programs revolves around the
`Control.Exception` module provided by `base`. This module provides
mechanism where any type belonging to the `Exception` typeclass can be
thrown as an exception and potentially caught by a handler.


The goal of this work is to augment this mechanism with the ability to
transparently attach to an exception a stack trace locating the point
where it was thrown. This should be done in such a way that backwards
compatibility is preserved while providing a way for exception handlers
who want a backtrace to get one.


In order to see how we might accomplish this it will be helpful to
review how exception handling currently works in GHC Haskell.

## Primitive operations


All (synchronous) exception handling in GHC is built on a few primitive operations,

```
catch#::(State#RealWorld->(#State#RealWorld, a #))->(b ->State#RealWorld->(#State#RealWorld, a #))->State#RealWorld->(#State#RealWorld, a #)raise#:: a -> b

raiseIO#:: a ->State#RealWorld->(#State#RealWorld, b #)
```


Which we can roughly interpret as,

```
catch#::IO a
       ->(b ->IO a)->IO a

raise#:: a -> b

raiseIO#:: a ->IO b
```


Note how, like many primops, none of these care in the slightest about the types you give
them. In fact, it is trivial to realize `unsafePerformIO` equipped with them,
{{{\#!
λ\> IO $ catch\# (raiseIO\# 48) (\\a s1 -\> (\# s1, a::Char \#))
'0'
λ\> IO $ catch\# (raiseIO\# \[1,2,3::Int\]) (\\a s1 -\> (\# s1, a::Int \#))
8646911832613230923
}}}


For this reason, `base` implements a far safer interface on top of this mechanism.

## `Control.Excecption`

`Control.Exception` provides a type-guided mechanism for exception
handling,

```
class(Typeable e,Show e)=>Exception e where...-- | A value of any type in @Exception@ can be raised as an exceptionthrow::Exception e => e -> a

-- | Run a potentially failing computation, catching and handling-- exceptions of a particular type @e@.catch::Exception e
      =>IO a         -- ^ potentially failing computation->(e ->IO a)-- ^ a handler->IO a
```


The `Exception` typeclass itself demands very little of its members,
requiring only a `Show` instance (merely so we have something useful to
show the user) and a runtime type representation (i.e. belonging to the
`Typeable` class).


To make this polymorphic handling scheme work we need to have some way
to preserve information about the type of the exception that was thrown,
lest the handler has no idea how to interpret the closure it is handed.
For this, we introduce a type allowing us to box the exception value
itself along with an `Exception` dictionary,

```
dataSomeException=SomeException(forall e.Exception e => e)
```


Since `Exception` implies `Typeable` this gives us the ability to
identify the type of the exception contained within. We can now safely
define `throw` without having to worry about being unable to reconstruct
the type of the thrown exception later,

```
class(Typeable e,Show e)=>Exception e

throw::Exception e => e -> a
throw e =let e' =SomeException e
    in raise# e'
```


Now when we catch an exception, we can use the fact that we know the
value thrown was of type `SomeException`. This provides us the ability to
determine whether a given exception is appropriate for a given handler
using `Typeable`'s `cast` function,

```
-- | From 'Data.Typeable'cast::(Typeable a,Typeable b)=> a ->Maybe b

catchException::Exception e =>IO a ->(e ->IO a)->IO a
catchException(IO io) handler =IO$ catch# io handler'
  where
    handler' ::SomeException->IO a
    handler' se@(SomeException e)=case cast e ofJust e' -> unIO (handler e')Nothing-> raiseIO# se
```


Note how in the event that `cast` returns `Nothing` we rethrow the
exception. This will be an important consideration on our treatment of
stack traces.

## The plan for stack traces


Having examined the current scheme, we can now discuss how to add persist
stack trace information through this entire process. It should be said
that this stack trace support must remain optional: not only does stack
trace support pose a tricky engineering challenge for second tier
platforms, but backtrace collection itself can often represent a
significant cost which users may seek to avoid.

### Feeding a stack trace to the handler


For the moment ignore the question of \*how\* we actually collect a stack
trace; instead we'll consider how we can get a backtrace safely to the
handler once it has been collected.


We begin by modifying `SomeException` to add an optional `StackTrace`
field, using pattern synonyms to preserve the existing interface,

```
-- | Some notion of a stack tracedataStackTracedataSomeException=SomeExceptionWithStack!(MaybeStackTrace)(forall e.Exception e => e)-- | Preserve compatibility for existing userspatternSomeException e <-SomeExceptionWithStack_ e whereSomeException e =SomeExceptionWithStackNothing e

```


We can now easily introduce a variant of catch which can provide this
optional stack trace to the handler,

```
catchExceptionWithStack::Exception e
                        =>IO a
                        ->(MaybeStackTrace-> e ->IO a)->IO a
catchExceptionWithStack(IO io) handler =IO$ catch# io handler'
  where
    handler' ::SomeException->IO a
    handler' se@(SomeExceptionWithStack stack e)=case cast e ofJust e' -> unIO (handler stack e')Nothing-> raiseIO# se
```


Note how both `catchException` and `catchExceptionWithStack` both handle
rethrowing correctly; since they are rethrowing the same
`SomeException`, the stack trace will be preserved even as the exception
bubbles up through the handler stack.

### Producing a stack trace


Now we come to the question of how one actually produces a stack trace.
Despite the perceptions of many users, Haskell actually has a wealth of
potential stack trace sources,

- The cost-center stack may be available if the program was built with
  profiling enabled

- The (relatively new) implicit-parameter-based stacktrace facility may
  be able to offer a bit of local context

- [DWARF-based stack](/trac/ghc/Dwarf/80Status) unwinding may be available depending upon the
  platform and availability of debugging information

### Minimizing unnecessary stack collection


Sadly, all of these mechanisms come at some cost. For instance, 
GHC's newly-integrated `libdw`-based stack unwinder appears to require
on the order of one microsecond per unwound frame (although this can probably be
improved by implementing a fast-path for traversing the STG stack). For
this reason, we ideally want to minimize unnecessary stack collections:
if none of the handlers on the stack want a backtrace when an exception
is thrown, we should avoid collecting one at all.


To accomplish this, we introduce a piece of per-Haskell-thread state which
records the location of the outer-most stack-frame wanting a backtrace
(if any). This pointer would be set by the RTS on the first call to
`catchExceptionWithStack` in a thread's lifetime. Conversely it would be
cleared when this catch frame is popped from the stack.


When the thread raises an exception, `throw` would check this state and
collect a stack trace if a parent frame has reported interest in one.


While this reduces the number of unnecessary stack collections, it does not
eliminate them entirely. If a handler with no interest in a stack frame
handles the exception before it bubbles up to the stack-trace-desiring
handler, the effort spent in collecting the trace will have been wasted.
Unfortunately, this may be a rather common occurrence, as we would
likely want the top-level exception handler to provide a stack trace if
possible.

### Alternatives


A more sophisticated (and complicated) scheme for reducing the cost of
stack collection would be to utilize memory management trickery to
preserve the stack immutably when an exception is thrown. However, given
the complexity this involves and the cost of setting up memory
mappings, this is unlikely to be worth exploring.


Moreover, given that exceptions are relatively rare in Haskell code,
it's unclear whether any of the effort described in the above two
sections is worthwhile. Perhaps we just accept a high exception cost and
enable/disable stacktrace collection explicitly on a per-thread or
even per-process basis.
