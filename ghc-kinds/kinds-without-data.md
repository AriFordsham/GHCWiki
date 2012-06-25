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

- We lose constructor name space, because the datatype constructor names will be taken, even though
  we will never use them. So `Prod` and `K` cannot be used as constructors of `Interpretation` as above,
  because those are also constructors of `Universe`.

- We cannot use kinds (such as `*`) while defining a datatype, so we are forced to make `Universe` a
  parametrised datatype, and later always instantiate this parameter to `*` (like in the kind of
  `Interpretation`).

**Proposal:** allow defining kinds directly, as in the following example:

```wiki
data kind Universe = Sum  Universe Universe
                   | Prod Universe Universe
                   | K *
```


By using `data kind`, we tell GHC that we are only interested in the `Universe` kind, and not the datatype.
Consequently, `Sum`, `Prod`, and `K` will be types only, and not constructors.

# Notes

- `data kind K ...`

- Allow `*` on `data kind`s? Or maybe `Type`, or `Star`.

- Perhaps also `data type D ...`

- Promote type synonyms by default

- What about `type kind K1 = K2`?

- Even worse: `type type T1 = T2`...
