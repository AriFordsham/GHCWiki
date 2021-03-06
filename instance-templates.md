
This page documents yet another proposal to make class hierarchies easier to deal with. A good counterpoint is the [IntrinsicSuperclasses](intrinsic-superclasses) proposal, which shares many of the motivations.


## Example


```
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  deriving instance Functor m where
    fmap = lift1

  deriving instance Applicative m where
    pure = return
    (<*>) = ap

instance Monad Maybe where
  return = Just
  Nothing >>= _  = Nothing
  (Just x) >>= f = f x

  deriving (Functor, Applicative)   -- these inherit the constraints and params above
  -- OR
  deriving instance Functor m
  deriving instance Applicative m   -- these specify constraints
```

## Proposal


A class may now include so-called *instance templates*, right in the class declaration. The enclosing class is called the *enclosing class*. An instance template declaration does not actually manufacture an instance -- it just acts as a template which can be activated when an instance of the enclosing class is written.


The rules for instance template declarations are as follows:

- There is no requirement for the instance template's class to have any particular relationship with the deriving class. An instance template will often be for a superclass, but this is up to the programmer.

- Instance templates may have arbitrary constraints.

- At least one of the type arguments in the instance template head must mention at least one of the class parameters.

- The body of the instance template is, syntactically, a normal instance body. `ScopedTypeVariables` controls whether or not the enclosing class's type variables are in scope.

- When type-checking an instance template method instantiation, we assume a dictionary for the enclosing class. That is, the enclosing class is assumed as a constraint on any enclosed instance templates.

- It is allowable to have multiple instance templates for the same class within the same enclosing class, as long as the type arguments are *apart*.

- A `MINIMAL` pragma, which would appear in the enclosing class, *not* within any instance templates, may refer to definitions in the classes instantiated in the instance templates. When checking an enclosing class's instance for completeness, a method from a templated class is said to be defined if the method is defined locally (that is, within a `deriving` clause within the enclosing class instance in question) or if there exists an instance in scope that matches the relevant instance template.


When writing an instance for a class enclosing one or more instance templates, a user can include a `deriving` directive in the instance body. Note that this does not steal syntax. There are two forms of `deriving` directives:

1. The keyword `deriving` followed by a parenthesized, comma-separated list of class names. (The parentheses can be omitted if there is only one class name, just like with `deriving` and `data` declarations.)

  - Each class in this list must have precisely one instance template enclosed in the enclosing class.
  - The instance templates are then instantiated with the type arguments given in the enclosing instance head. 
  - The constraints on these instances are inferred. The constraint is the smallest set of constraints that imply all of the following:

    - The constraints on the instance template.
    - The constraints on the enclosing instance.

1. The keywords `deriving instance` followed by an optional set of constraints and an instance head.

  - The instance head must match one of the instance templates of the enclosing class, when type variables are instantiated to the type arguments supplied to the enclosing instance.
  - The selected instance template is then instantiated with the type arguments given.
  - No constraints are inferred. For the `deriving` directive to type-check, the set of constraints provided after `deriving instance` must imply both the constraints on the instance template and the constraints on the enclosing instance. (**RAE:** Would it be better to automatically include the constraints on the enclosing instance? Could that ever be against the user's wishes?)

## Extension 1: Defaults


If instance templates are used to mitigate problems when refactoring a class hierarchy between releases, we have a small problem: client code must still add a `deriving` directive. We can extend the idea above to get around this problem:

- An instance template can now be written `deriving default instance ...`

- A default instance template *must* be for a superclass of the enclosing class.

- If an instance of the enclosing class is written and there is no instance in scope that matches the default instance template, the instance template is automatically instantiated, as if by `deriving <<class name>>`. (That is, constraints are inferred.) A suppressable warning is also issued, recommending that the user add the `deriving` clause explicitly.

## Extension 2: Derived instance bodies


This extension allows derived instances to override definitions in the instance templates. This is suitable when a user wants most of an instance template's definitions, but not all. This is also useful when the instance template is necessarily incomplete (because one or more functions in the instance template's class can't be written in the template). Here are the rules:

- An instance template declaration is *not* checked for completeness against `MINIMAL` pragmas. (**RAE:** This means that an instance template writer can make an easy, undetected mistake, if the user intends the template to be complete. Should there be a way to signal the desire for completeness? Or, probably better, the desire for incompleteness, effectively suppressing the completeness warning?)

- All derived instances (including those created with a bare `deriving` directive) *are* checked for completeness against `MINIMAL` pragmas.

- Users can put a `where` clause after `deriving instance ... => ...`, and give definitions, as in a normal instance declaration. The only difference between a `deriving instance` declaration and a normal one is that any omitted definitions are inherited from the instance template in the enclosing class.

## Extension 3: Inherited instance templates


In a deep class hierarchy (i.e. with long chains of superclasses), it may be convenient to propagate instance templates from superclass to subclass. The base proposal ignores superclass relationship. (Just because `Applicative` has an instance template for `Functor`, it doesn't mean that `Monad`, a subclass of `Applicative`, also has the instance template.) This extension would allow users to write `deriving <<class names>>` in a *class* declaration, meaning that instance templates for those classes are inherited from superclasses. Here are the rules:

- A class declaration may include a `deriving << class names >>` directive, using syntax similar to other `deriving` directives.

- Each class listed in a `deriving` directive must have precisely one instance template enclosed in precisely one superclass. This instance template is considered to be part of the enclosing class.


For example:


```
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  deriving default instance Functor f where
    fmap f = (pure f <*>)

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  fail :: String -> m a

  deriving default instance Applicative m where
    pure = return
    (<*>) = ap
  deriving default Functor
```


In the last line, we see a combination of extensions 1 and 3, which causes no additional difficulties. (**RAE:** Though, perhaps it suggests swapping the order of the syntax from `deriving default` to `default deriving`.)

## Example of splitting a class


This proposal does not address directly how to split a class into pieces, but the features described here can be used to do so, with the perhaps-unfortunate consequence if requiring new names to be introduced. (Contrast with [IntrinsicSuperclasses](intrinsic-superclasses), which supports splitting directly, but has the perhaps-unfortunate consequence that method definitions in instances do not always belong to the class they appear to be defined for.)



Here is a class we want to split:


```
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
```


And here is how we might split it:


```
class Additive a where
  add :: a -> a -> a
class Multiplicative a where
  mult :: a -> a -> a

class (Additive a, Multiplicative a) => Num a where
  (+) :: a -> a -> a
  (+) = add

  (*) :: a -> a -> a
  (*) = mult

  deriving default instance Additive a where
    add = (+)
  deriving default instance Multiplicative a where
    mult = (*)

  {-# MINIMAL (add | (+)), (mult | (*)) #-}
```


Now, an old `Num` instance would continue to work, and would automatically generate suitable `Additive` and `Multiplicative` instances, albeit with warnings due to default instance generation. (The warnings are because I -- Richard -- believe that all instances should be made explicit, under ideal circumstances.)
