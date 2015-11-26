# Plan for stack traces on exceptions


Exception handling in modern Haskell programs revolves around the
`Control.Exception` module provided by `base`. This module provides
mechanism where any type belonging to the `Exception` typeclass can be
thrown as an exception and potentially caught by a handler.


The goal of this work is to augment this mechanism with the ability to
transparently attach a stack trace to these exceptions. This should be
done in such a way that backwards compatibility is preserved while
providing a way for exception handlers who want a backtrace to get one.


In order to see how we might accomplish this, let's peer into the
current mechanism.

## Primitive operations


All exception handling in GHC is built on a few primitive operations,

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
For this, we introduce a type including an `Exception` dictionary,

```
dataSomeException=SomeException(forall e.Exception e => e)
```


Since `Exception` implies `Typeable` this gives us the ability to
identify the type of the exception contained within. We can now safely
define `throw` without having to worry about being unable to reconstruct
the type of the thrown exception later,

```
class(Typeable e,Show e)=>Exception e where
    toException   :: e ->SomeException
    fromException ::SomeException->Maybe e

throw::Exception e => e -> a
throw e =let e' = toException e ::SomeExceptionin raise# e'
```


Now when we catch an exception, we can use the fact that we know the
value thrown was of type `SomeException`. This provides us the ability to
determine whether a given exception is appropriate for a given handler
using `Typeable`'s `cast` function,

```
-- | From 'Data.Typeable'cast::(Typeable a,Typeable b)=> a ->Maybe b

catchException::Exception e =>IO a ->(e ->IO a)->IO a
catchException(IO io) handler =IO$ catch# io handler'
    where handler' e =case cast e ofJust e' -> unIO (handler e')Nothing-> raiseIO# e
```


Note how in the event that `cast` returns `Nothing` we rethrow the
exception. This will be an important consideration on our treatment of
stack traces.

## The plan for stack traces


We begin by modifying `SomeException` to add an optional `StackTrace`
field, using pattern synonyms to preserve the existing interface,

```
-- | Some notion of a stack tracedataStackTracedataSomeException=SomeExceptionWithStack!(MaybeStackTrace)(forall e.Exception e => e)-- | Preserve compatibility for existing userspatternSomeException e <-SomeExceptionWithStack_ e whereSomeException e =SomeExceptionWithStackNothing e

```


We can now trivially introduce a variant of catch which can provide this 
