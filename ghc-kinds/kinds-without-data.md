# Defining kinds without an associated datatype


When using `-XDataKinds` GHC automatically promotes every datatype to a kind, and its constructors to
types. This forces us to declare a datatype for every kind. However, sometimes we are not interested
in the datatype at all, only on the kind. Consider the following data kind that defines a small
universe for generic programming:

```wiki
data Universe star = Sum  Universe Universe
                   | Prod Universe Universe
                   | K star
```


This universe comes with an associated interpretation:

```wiki
data Interpretation :: Universe * -> * where
  L    :: Interpretation a -> Interpretation (Sum a b)
  R    :: Interpretation b -> Interpretation (Sum a b)
  Prod :: Interpretation a -> Interpretation b -> Interpretation (Prod a b)
  K    :: a -> Interpretation (K a)
```


In this case, having to declare a datatype for `Universe` has two disadvantages:

1. We cannot use kinds (such as `*`) while defining a datatype, so we are forced to make `Universe` a parametrised datatype, and later always instantiate this parameter to `*` (like in the kind of `Interpretation`).

1. We lose constructor name space, because the datatype constructor names will be taken, even though we will never use them. So `Prod` and `K` cannot be used as constructors of `Interpretation` as above, because those are also constructors of `Universe`.

**Solution**: let users define things like

```wiki
data kind Universe = Sum  Universe Universe
                   | Prod Universe Universe
                   | K *
```


By using `data kind`, we tell GHC that we are only interested in the `Universe` kind, and not the datatype.
Consequently, `Sum`, `Prod`, and `K` will be types only, and not constructors.


Also,

```wiki
data type (i :: D) where C :: I ('C Int)
```


defines a datatype `D` which is not promoted to a kind, and its constructors
are not promoted to types. We would then also have ‘type only T = Int -\> Int\`.

*Advantages*: solves (1) and (2)

*Disadvantages*:

1. If, in the future, we make `* :: *`, we will no longer have separation of


types and kinds, so things like `D`/`I` above will become impossible.

1. Requires changing the parser


Currently we are planning to implement the second solution. If we do get `* :: *` other things will break due to name clashes, so that shouldn't prevent us from going ahead now. This ticket to track this request is [\#6024](https://gitlab.haskell.org//ghc/ghc/issues/6024).

## Alternative Solutions


Add

```wiki
data Star
```


in `GHC.Exts` such that the promotion of datatype `Star` is the kind `*`. As a
datatype, `Star` is just an empty datatype.

*Advantages*: very easy, backwards compatible

*Disadvantages*: somewhat verbose, doesn't fix (2)

## Alternative Notations

- Use `data only` instead of `data type`.
- Use `'data` instead of `data kind`, suggested by Gabor Greif.


In both cases, we felt that using `type` and `kind` as the modifiers to the `data` declaration better reflect what's being defined.
