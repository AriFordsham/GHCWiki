# Left-Associative `Semigroup` Operator Alias

## Problem


With the implementation of [ prime:Libraries/Proposals/SemigroupMonoid](https://prime.haskell.org/intertrac/Libraries/Proposals/SemigroupMonoid), `Semigroup` will become a superclass of `Monoid`, and consequently `Semigroup((<>))` will be re-exported alongside `Monoid` from the `Prelude` module.

```
-- reduced/simplified definitionclassSemigroup a where(<>):: a -> a -> a

infixr6<>
```

### Conflicting fixities of `<>` in pretty printing APIs


However, there are a few popular pretty-printing modules which already define a `<>` top-level binding for their respective semigroup/monoid binary operation. The problem now is that those `<>` definitions use a different operator fixity/associativity:

```
-- prettymoduleText.PrettyPrint.Annotated.HughesPJwhereinfixl6<>infixl6<+>infixl5$$,$+$-- prettymoduleText.PrettyPrint.HughesPJwhereinfixl6<>infixl6<+>infixl5$$,$+$
```

```
-- template-haskellmoduleLanguage.Haskell.TH.PprLibwhereinfixl6<>infixl6<+>infixl5$$,$+$
```

```
-- ghcmoduleOutputableinfixl9<>infixl9<+>infixl9$$,$+$-- ghcmodulePrettywhereinfixl6<>infixl6<+>infixl5$$,$+$
```


On the other hand, the popular [ hackage:ansi-wl-pprint](http://hackage.haskell.org/package/ansi-wl-pprint) package does use right-associative operators:

```
moduleText.PrettyPrint.ANSI.Leijenwhereinfixr6<>infixr6<+>
```


Other pretty printers also using a `infixr 6 <>, <+>` definition:

- [ hackage:annotated-wl-pprint](http://hackage.haskell.org/package/annotated-wl-pprint)
- [ hackage:mainland-pretty](http://hackage.haskell.org/package/mainland-pretty)

### Changing `<>`'s associativity in pretty-printing APIs


Changing the fixity of `pretty`'s `<>` would however results in a semantic change for code which relies on the relative fixity between `<+>` and `<>` as was [ pointed out by Duncan back in 2011](https://mail.haskell.org/pipermail/libraries/2011-November/017066.html) already:

>
> So I was preparing to commit this change in base and validating ghc when I discovered a more subtle issue in the pretty package:
>
>
> Consider
>
> ```
> a<> empty <+> b
> ```
>
>
> The fixity of `<>` and `<+>` is critical:
>
> ```
> (a <> empty)<+> b
> ={- empty is unit of <> -}(a         )<+> b
>
>   a <>(empty <+> b)={- empty is unit of <+> -}
>   a <>(          b)
> ```
>
>
> Currently Text.Pretty declares `infixl 5 <>, <+>`. If we change them to be `infixr` then we get the latter  meaning of `a <> empty <+> b`. Existing code relies on the former meaning and produces different output with the latter (e.g. ghc producing error messages like "instancefor" when it should have been "instance for").

### Unsatisfying Situation Seeking a Long-term Solution


Consequently, it's confusing and bad practice to have a soon-to-be-in-Prelude `<>` operator whose meaning depends on which `import`s are currently in scope. Moreover, a module needs to combine pretty-printing monoids and other non-pretty-printing monoids, the conflicting `<>`s operator needs to be disambiguated via module qualification or similiar techniques.


However, there also seems to be a legitimate use-case for a left-associative `<>` operator.

## Proposed Solution


Add a standardised alias for `<>` to the `Data.Semigroup` vocabulary, e.g.

```
moduleData.Semigroupwhereinfixl6><-- | Left-associative alias for (right-associative) 'Semigroup' operation '(<>)'(><)::Semigroup a => a -> a -> a
(><)=(<>)
```

#### Bikesheds for `><`

- `.<>`
- `<~>`