# [ValidateMonoLiterals](validate-mono-literals) Extension

## Problem


In the interest of type-safety and bug elimination we want to make our types as specific and semantic as possible, to steal an example from Don Stewart's recent talk, not:

```wiki
f :: Double -> Double -> String -> Double
```


but

```wiki
f :: Rate Libor -> Spot SGD -> Date -> Rate SIBOR
```


Frequently we want to restrict the values that are allowed for these special types, for the sake of a simple running example, let's define:

```wiki
newtype Even = Even Int
```


Clearly we don't ever want allow `Even 1` to be an acceptable construction, so we hide `Even`'s constructor and export a "smart-constructor": 

```wiki
mkEven :: Int -> Maybe Even
```


This succesfully forces users to validate any input is appropriate and helps us preserve important program invariants, there is however a problem. In many scenarios a programmer will have to write constants into their program. For example, someone writing a HTTP or SMTP implementation will likely have to include various `ByteString`'s with specific protocol commands in their source. And now our type safety becomes a hassle.


We now somehow have to handle a failure condition that, logically, should never happen. If the constant we wrote is correct, it should always succeed and if the constant is wrong it can never work. Yet we find ourselves dealing with this uncomfortable `Maybe` that we can't really get rid off.


And now for a small detour about literals.


Some efficient and frequently used data structures, such as `Text` and `ByteString` don't really have a convenient way of being written directly, instead of converted from existing values, like `String` and `[Word8]`. To counter this hassle we have extensions like `OverloadedStrings` and now `OverloadedLists`, allowing plain, easy to write literals to be automatically converted to some other type that does not have an easily typeable form.


Now, one of the common pitfalls of these `OverloadedX` extensions and the `Num` typeclass is that these conversions have to be **total**. If they're partial then literals may end up producing runtime errors, which is obviously undesirable. However, many sensible types do not have total conversions, so they either silently do the "wrong" thing or just end up producing runtime errors.


Examples of "Doing The Wrong Thing" are: `ByteString`'s `IsString` instance silently truncating all values to ASCII, `Word8` resulting in modulo 256 results (i.e. "(256 :: Word8) == 0"). Now GHC recently gained warnings for overflowing literals for built in numeric types, but this doesn't solve any issues for user overloaded literals.

### Summary


So what we would like is the ability to:

1. Enforce invariants on datatypes
1. Have a convenient way to write constants of these datatypes
1. Statically guarantee invariants hold for these constants.

## Currently


In GHC we can actually already get most of the way there. We have Template Haskell which we can use to execute functions of type `a -> Maybe b` at compile time, producing an error in the case of `Nothing` or splicing in the resulting `b` on success. In fact, with Typed Template Haskell, we can even use typeclass to infer the conversion function from the context of the splice. A simple implementation would be something along the lines of:

```wiki
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FooTH where

import Control.Applicative
import Data.Maybe
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype Even = Even Integer

class Validate a where
    validateInteger :: Integer -> Maybe a
    validateInteger _ = Nothing

instance Validate Even where
    validateInteger i
        | even i = Just (Even i)
        | otherwise = Nothing

validate :: forall a . Validate a => Integer -> Q (TExp a)
validate i = case (validateInteger i :: Maybe a) of
    Nothing -> fail "whoops"
    Just _ -> [|| fromJust (validateInteger i) ||]
```


Now in another source files we can write:

```wiki
{-# LANGUAGE TemplateHaskell #-}
module Foo where

import FooTH

x :: Even
x = $$(validate 38)
```


And this compiles fine, yet had we written `$$(validate 39)` we would have gotten a compile error, as expected.


However, this notation is rather noisy and in compound expressions can make the result really unreadable. It would be much nicer to give GHC the ability to automatically "swap out" the various Overloaded literals and automatically replace them with versions that validate their conversion at compile time, thus preserving the easy readability of overloaded literals, but dramatically increasing their safety.


This clearly only makes sense for monomorphic literals (else, how would you decide which validation function to use!), hence the name `ValidateMonoLiterals`

### Summary


To clarify the exact change proposed:


When the [ValidateMonoLiterals](validate-mono-literals) is enabled GHC would replace `fromString`/`fromInteger`/`fromList` expressions originating from literals with a Typed TH splice along the lines of `validate` for all monomorphic cases. Validating polymorphic cases at compile time is not really a sensible thing to do, so those should just remain with the current behaviour.

## Various Bikesheds in Need of Colouring


This proposal has various bikesheds in need of colouring and my research into the feasibility had me run into several errors.


The most minor issue is: One class with all functions (fromList, fromString and fromInteger) or one class for each (mimicing the current split between IsString, IsList and Num). The biggest advantage of splitting them is that only the list version would have to deal with an associated type like the current IsList class, so people who don't care about list literals don't have to specify anything in that case).


A more critical issue is whether the Validate class should require a Lift superclass or somehow try to make use of it. As you can see in the above example code for `validate`, we are splicing the entire `fromJust (validateInteger i)` code into the target program. This means that the conversion happens at runtime, every time the program runs. Ideally we would only splice the result into the compiled program, however this requires a Lift instance which only 13 data types in base have (half of which are tuples). There is a Template Haskell library for deriving Lift instances, but it is unclear why GHC can't just do it for us.
