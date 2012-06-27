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
Consequently, `Sum`, `Prod`, and `K` will be types only, and not constructors. Note however that this would
imply being able to parse kinds (`*`, at the very least) on the right-hand side of data kind declarations.
To avoid this, we propose instead using a kind `Type` (or `Star`), defined in `GHC.Exts`, that acts as a
synonym of `*`.


This ticket to track this request is [\#6024](https://gitlab.haskell.org//ghc/ghc/issues/6024).

# Defining datatypes without an associated kind


By extension, we might want to define a datatype that will never be promoted, even with `-XDataKinds`.
For that we propose the syntax `data type D ...`.

# Kind synonyms (from type synonym promotion)


Currently GHC does not promote type synonyms. We propose to change this, and make GHC promote
type synonyms to kind synonyms by default with `-XDataKinds`. For instance, `type String = [Char]`
should give rise to a kind `String`.

**Question:** are there dangerous interactions with `-XLiberalTypeSynonyms`? E.g. what's the kind
of *type K a = forall b. b -\> a\`?
*


By extension, we might want to have kind synonyms that do not arise from promotion: `type kind K ...`.
And perhaps even type synonyms that never give rise to a promoted kind: `type type T ...`.
