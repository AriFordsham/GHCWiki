## Changes to Template Haskell Library


Throughout this document, it is assumed that the current version of GHC is 7.4.1.


A proposed change will add the following constructors to TH's `Type` datatype...

```wiki
| PromotedListT [Type]    -- for types of the form '[Int, Bool]
| PromotedTupleT [Type]   -- for types of the form '(Int, 'False)
| PromotedConsT           -- ':
```


... and the following constructors to TH's `Kind` datatype:

```wiki
| ConK Name               -- for kinds of the form Bool
| VarK Name               -- k
| ForallK [Name] Kind     -- forall k. ...
| AppK Kind Kind          -- k1 k2
| ListK                   -- []
| TupleK Int              -- (), (,), ...
| ConstraintK             -- Constraint
```


The final `Kind` constructor does not need any special binder construct because all kinds are of sort `BOX`.


TH will also need to support promoted constructors other than lists and tuples, but this is in fact already supported through the use of `ConT`. The namespace of defined types and of promoted types is also already kept distinct. For example, if we have the definition `data Foo = Foo`, the results of ` [t| Foo |] ` and ` [t| 'Foo |] ` are distinct (as in, `==` returns `False`). However, applying `show` to these two results produces the same string.


The one place TH needs to be updated to handle promoted data constructors is in the naming quote syntax. Currently, writing `'Foo` in an expression context within a splice looks `Foo` up in the expression namespace; TH will find a data constructor named `Foo` and return its `Name`. Writing `''Foo` in an expression context within a splice looks `Foo` up in the type namespace; TH will find a type constructor named `Foo` and return its name. There is currently no way to look up a promoted data constructor. The update will include a third form of quote, `'''Foo`, which gets the name of a promoted data constructor `Foo`. Though having three quotes is somewhat regrettable, it dovetails nicely with the fact that `'Foo`, when used in a type context, refers to a promoted data constructor. If we think of the first two quotes as establishing a type context, the third quote flows naturally.

## Alternatives


Here are two alternatives to the above changes. They are orthogonal to each other (i.e. either can be chosen without affecting whether or not the other is chosen).

### Simpler Types


Instead of the new types above, we could have the following:

```wiki
| PromotedTupleT Int     -- '(), '(,), ...
| PromotedNilT           -- '[]
| PromotedConsT          -- ':
```


Client code could create full tuples and lists using a combination of the above constructors with a liberal sprinkling of `AppT`s.

- Pros: Matches syntax of existing `TupleT` and `ListT`. For lists, matches forms available in surface syntax.
- Cons: Believed to be harder to use in practice. The `PromotedTupleT` construct here is not available in surface syntax. This also loses the ability to write succinct lists, while the original format proposed above allows for nil as a 0-element list.


It may be worth noting that `'(,) Int Bool` is *not* a synonym for `'(Int,Bool)`. `'(,) Int Bool` is a parse error.

### Structured Kinds


Instead of the new `ListK` and `TupleK` kinds above, we could have the following:

```wiki
| ListK Kind     -- [k]
| TupleK [Kind]  -- (k1,k2,...)
```

- Pros: Perhaps easier to use. Mirrors surface syntax.
- Cons: Does not match internal GHC representation, including what is printed in error messages and such. Different from the way `Type` works.


It may be worth noting that, in the kind language, `(,) Int Bool` is *not* a synonym for `(Int,Bool)`. `(,) Int Bool` is a parse error.
