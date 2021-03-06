# Infer DatatypeContexts


Haskell prime has decided to [depricate DatatypeContexts](https://ghc.haskell.org/trac/haskell-prime/wiki/NoDatatypeContexts).


Running Example:

```wiki
data (Eq e) => HasEq e = HasEq e
```


In haskell98, 2010 these are not typically usable as you might expect

```wiki
eq :: HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```


Would give a missing context error, so contexts always had to be specified.

```wiki
eq :: (Eq e) => HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```

## GADT alternative


An alternative in terms of GADTs that works as expected has been suggested.

```wiki
data HasEq e where
    HasEq :: (Eq e) => e -> HasEq e

eq :: HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```

### Problems with GADT alternative

- For performance sensitive types this might be problematic since they
  are always stored in a box that could be bottom and that needs to be checked
  at runtime.

- If you have a num container,

  ```wiki
  data IsNum n where
      IsNum :: (Num n) => n -> IsNum n
  ```

  you can not write the type of the following expression without stating the type class

  ```wiki
  IsNum $ fromInteger 1 :: (Num n) => IsNum n
  ```

## Suggested Extensions

### Syntactic sugar for explicit type annotations + Constructor type change


How can you get a value of type `HasEq a`.

1. Using a Constructor (also in a pattern):
  Use the same trick as with GADTs?
  Just make the type signature of the constructor `(Eq a) => a -> HasEq a`
1. Explicit type signature `(undefined :: HasEq a)`:
  For all free variables in `HasEq (...)` add `Eq (...)` to the context where the variable is bound.

>
>
> Listing of all places where explicit types appear.
>
>

- Haskell98

  - `type simpletype = type`
    just a simple type synonym no need to do anything
  - `data [context =>] simpletype = constrs [deriving]`
    check the constructors for types with DatatypeContexts.
    Place all found contexts in `context`. If the context is not empty?
    Change the constructor types to:
    ` Constr :: context => ...`
  - same for newtype
  - `class [scontext =>] tycls tyvar [where cdecls]`
    Nothing to do really.
  - instance \[scontext =\>\] qtycls inst \[where idecls\]
    Make InferDatatypeContext imply FlexibleContexts. Check inst for types with DatatypeContexts.
    Add contex to scontext.
  - default (type1 , ... , typen)
    ignore
  - `gendecl -> vars :: [context =>] type`
    Check type. Add to context
  - `context -> class | ( class1 , ... , classn )`
    Check types in class1 ... Add to context
  - `exp -> exp0 :: [context =>] type`
    Check type. Add to context
- Extensions:

  - Explicit forall RankNTypes, Imporedicative types.
    For all free variables in `HasEq (...)` add `Eq (...)` to the context where the variable is bound
    e.g.

    ```wiki
    Maybe (
        forall b. (
            forall c. HasEq (a,b,c,forall d.d)
            ) -> ()
        ) -> a
    ```

    becomes

    ```wiki
    Eq (a,b',c',d') => Maybe (
        forall b. (Eq (a,b,c'',d'')) => (
            forall c. (Eq (a,b,c,d''')) => HasEq (a,b,c,forall d.d)
            ) -> ()
        ) -> a
    ```
  - Type families: like with type synonyms contexts can be safely ignored here. What about newtype type families?
  - Functional Dependencies: TODO
  - GADTs: TODO


      


1. Functions in modules without this extension.
  Maybe it's possible to add the contexts when the functions are imported. I don't know.

## Uses


## Related



#8026

[http://www.haskell.org/pipermail/haskell-prime/2010-July/003249.html](http://www.haskell.org/pipermail/haskell-prime/2010-July/003249.html)


