# Polymorphic Dynamics


Haskell already provides dynamic types with `Data.Dynamic` and `Data.Typeable` but this interface limits functions to being monomorphic.  This page is to collect notes on implementing polymorphic dynamic types.

## Motivation


There are instances when an object that is not fully known at the time of compiling asks to be linked into a running program.  This is currently possible for monomorphic types:

```wiki
data Dynamic = Dynamic TypeRep Obj   -- Obj is like HValue; GHC is inconsistent

toDyn       :: Typeable a => a -> Dynamic
fromDynamic :: Typeable a => Dynamic -> Maybe a
dynApply    :: Dynamic -> Dynamic -> Maybe Dynamic
```


Having the ability to load polymorphic (both ad-hoc and parametric) functions after the program has been executed would provide a more general mechanism.

## Use cases


I have a natural language program that uses type inference to drive a chart parser.  Each word has a type and a semantic function.  The semantic functions are actually Haskell monadic actions that may or may not be polymorphic, depending upon the particular word.  So as to be able to increase the vocabulary while running, the word definitions are looked up in a dictionary and dynamically linked at run-time.  I found cases, such as with the word `and` in which the monadic action **must** be polymorphic.  This extension would solve that problem.

## Interface to user (programmer)


Because of the introduction of polymorphism, all this work would have to be done within a particular type environment, elegantly implemented by using a monad that encapsulates the type environment.


Otherwise, the interface is very similar to that above.

```wiki
data Dynamic = Dynamic Type HValue

toDynamic   :: a -> Dyn Dynamic
fromDynamic :: Dynamic -> Dyn (Maybe a)
dynApply    :: Dynamic -> Dynamic -> Dyn (Maybe Dynamic)

dynamicLoad :: [Dflags] -- ^ compilation flags
            -> [String] -- ^ names to dynamically load
            -> String   -- ^ module
            -> Dyn [Dynamic]
```

## Design


The Dyn monad is a newtype wrapper around the GHC monad.


Currently, using the GHC API, I am able to dynamically load/compile constants and functions, including parametrically polymorphic functions, and view the result of application.


The difficulty lies with ad-hoc polymorphism, because of the requirement of a dictionary for successful linking at run-time.


A function such as `sort :: Ord a => [a] -> [a]` in a module in which it is defined is incompletely specified because the specific type of the argument is not known at the export level.


During type-checking, normally before run-time, a specific argument type will be given and so the compiler can figure which dictionary to use for the class methods.  By running the full type-checker at run time it should be possible to use the unification substitutions to determine which dictionary to use and then to dynamically link the class methods so that the function can be applied.
