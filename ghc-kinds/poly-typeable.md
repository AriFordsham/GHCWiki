# Kind-polymorphic `Typeable`


The page describes an improved implementation of the `Typeable` class, using polymorphic kinds.  Technically it is straightforward, but it represents a non-backward-compatible change to a widely used library, so we need to make a plan for the transition.


Relevant tickets we could thereby fix: [\#5391](https://gitlab.haskell.org//ghc/ghc/issues/5391), [\#5863](https://gitlab.haskell.org//ghc/ghc/issues/5863).


Open question: what are the corresponding changes to `Data.Data`?  See [\#4896](https://gitlab.haskell.org//ghc/ghc/issues/4896), 

## The current `Typeable` class


The current `Typeable` class is:

```wiki
class Typeable (a :: *) where
  typeOf :: a -> TypeRep
```


Because it is mono-kinded we also have

```wiki
class Typeable1 (f :: *->*) where
  typeOf1 :: f a -> TypeRep
```


and so on up to `Typeable7`.  It's a mess, and we cannot make `Typeable` at all for
type constructors with higher kinds like

```wiki
  Foo :: (* -> *) -> *
```


See [\#5391](https://gitlab.haskell.org//ghc/ghc/issues/5391)

## The new `Typeable` class


Having polymorphic kinds lets us say this:

```wiki
data Proxy t = Proxy

class Typeable t where
  typeRep :: Proxy t -> TypeRep
```


Notice that

- `Typeable` and `Proxy` have polymorphic kinds:

  ```wiki
    Proxy    :: forall k. k -> *
    Typeable :: forall k. k -> Constraint
  ```

- The method is called `typeRep` rather than `typeOf`

- One reason for the name change is that the argument is not a value of the type `t`, but a value of type `(Proxy t)`.  We have to do this because `t` may have any kind, so we can't say 

  ```wiki
    typeOf :: t -> TypeRep
  ```


Now we can give give kind-specific instances:

```wiki
instance Typeable Int where typeRep _ = ...
instance Typeable []  where typeRep _ = ...
instance (Typeable a, Typeable b) => Typeable (a b) where
  typeRep _ = ...
```


A use of `deriving( Typeable )` for a type constructor `T` would always generate

```wiki
instance Typable T where typeRep _ = ....
```


i.e. an instance of `T` itself, not applied to anything.

### Aside


Iavor suggested:

```wiki
class Typeable (a :: k) where
  typeRep :: TTypeRep a

newtype TTypeRep a = TR TypeRep
```


Is this perhaps better?

## A change-over plan

**In GHC 7.8:**

- Rename `Data.Typeable` to `Data.OldTypeable` and deprecate the whole module.

- Define a new library `Data.Typeable` with the new definitions in them.

- Include in `Data.Typeable` old methods for backward compatibility, but deprecate them:

  ```wiki
  typeOf :: forall a. Typeable a => a -> TypeRep
  typeOf _ = typeRep (Proxy :: Proxy a)

  typeOf1 :: forall t (a :: *). Typeable t => t a -> TypeRep
  typeOf1 _ = typeRep (Proxy :: Proxy t)
  ```

- Make `deriving( Typeable )` work with whatever `Typeable` class is in scope.  So what it does will be determined by whether you say `import Data.Typeable` or `import Data.OldTypeable`.

**I think that means that old programs will continue to work in GHC 7.8**, provided

- You did not mention `Typeable1` etc explicitly
- You used `deriving( Typeable )` to write instances.

**In GHC  7.10:**

- Remove `Data.OldTypeable`