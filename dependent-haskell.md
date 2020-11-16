# Adding dependent types to Haskell


This page is to track design and implementation ideas around adding a form of dependent types to Haskell.

***Disclaimer:*** Everything below represents a research proposal. While it is my (RAE's) hope that something resembling this all will actually make it into GHC, no one should read anything too strongly into words like "will happen".

# Surface Language Design

## Quantifiers


As pointed out in the [Hasochism paper](dependent-haskell#), Haskell currently enjoys a confluence of design decisions. One says that compile-time arguments are elided in runtime code. For example, when calling `map :: (a -> b) -> [a] -> [b]`, the type instantiations for `a` and `b` are properly arguments to `map` (and are passed quite explicitly in Core), but these arguments are always elided in surface Haskell. Along similar lines, type arguments in Haskell are always erasable -- that is, instantiations for types are never kept at runtime. While this is generally a Good Thing and powers much of Haskell's efficiency, dependent typing relies on keeping *some* compile-time information around at runtime. Here, it is even more apparent that sometimes, we want to be able to pass in values as dependent arguments, especially if those values can be inspected at runtime.

Haskell currently has three quantifiers: `forall`, `->`, and `=>`, as classified in the following table:

| Quantifier | Dependent? | Visible? | Relevant? | Valid in types of terms? | Valid in types of types? | Extension |
| ------ | ------ | ----- | ----- | ----- | ----- | ----- |
| `forall ... .` | Yes | No (unification) | No | Yes | Yes (but these are relevant) | `-XExplicitForAll` |
| `forall ... ->` | Yes | Yes | Yes | No (proposal [#281](https://github.com/ghc-proposals/ghc-proposals/pull/281)) | Yes | `-XPolyKinds` |
| `->` | No | Yes | Yes | Yes | Yes | N/A |
| `=>` | No | No (solving) | Yes | Yes | Yes (only equality constraints) | N/A |

**Dependent**

*Dependent* means that the quantified thing (henceforth, *quantifiee*) can appear later in the type. This is clearly true for `forall`-quantified things and clearly not true for `->`-quantified things. (That is, if we have `Int -> Bool`, we can't mention the `Int` value after the `->`!)

**Visible**
*Visibility* refers to whether or not the argument must appear at *definitions* and *call sites* in the program text. If something is not visible, the table lists how GHC is to fill in the missing bit at call sites. If something is visible, we must specify how it is parsed, noting that the term- and type-level parsers are different. For example (not implemented, but see https://github.com/ghc-proposals/ghc-proposals/pull/281):

```wiki
-- Invisible ----
f1 :: forall a. a -> a
f1 x = x

g1 x = f1 True

-- Visible ----
f1 :: forall a -> a -> a
f1 a x = x 

g1 x = f1 Bool True
```

Same at the type level (but this *is* implemented):

```wiki
-- Invisible ----
type Proxy1 :: forall k. k -> *
data Proxy1 (a :: k) = P1
f1 :: Proxy1 Int -> Bool

-- Visible ----
type Proxy2 :: forall k -> k -> *
data Proxy2 k (a :: k) = P2
f2 :: Proxy2 Type Int
```

**Relevance**

*Relevance* refers to how the quantifiee can be used in the term classified by the type in question. For terms and their types, a binder is relevant iff it is not erased; that is, it is needed at runtime. In GHC terms, relevant binders are *Id*s and irrelevant ones are *TyVar*s.

For types and their kinds, we can't talk about erasure (since the are all erased!) but the relevance idea works the same, one level up.  Example


```wiki
type family Id (x::k1) (y::k2) :: (k1,k2) where
  Id True  v = (False, v)
  Id False v = (True,  v)
  Id x     v = (x,     v)
```

If `Id`'s kind was `forall k1 k2. k1 -> k2 -> (k1,k2)`, it looks parametric in both `k1` and `k2`.  But it isn't, because it can pattern-match on `k1`.  So `k1` is relevant, but `k2` is irrelevant.

(This is distinct from dependence, which says how the quantifiee can be used in the *type* that follows!) `forall`-quantifiees are not relevant. While they can textually appear in the classified term, they appear only in irrelevant positions -- that is, in type annotations and type signatures. `->`- and `=>`-quantifiees, on the other hand, can be used freely. 

Having explained our terms with the current Haskell, the proposed set of quantifiers for dependent Haskell is described in [Proposal #102](https://github.com/ghc-proposals/ghc-proposals/pull/102).

Declarations given without a type signature will need to perform *relevance inference* to figure out whether quantified variables should be `forall`-bound or `foreach`-bound. Relevance inference simply looks at usage sites; iff a variable is used in a relevant context (scrutinee of a pattern-match, or passed to a function expecting a relevant argument, among others) then it is relevant. It is tempting to perform relevance inference if the nature of a quantifier is omitted in a type signature, but this would make the type signature's meaning depend on the term, which may mean that a variable's type signature is not the authoritative type for the variable.

**Datatypes**

How is the kind of a datatype classified? After some debate, Stephanie and RAE thought that a poly-kinded datatype should be quantified with `foreach`, not `forall`. For example, consider `data Proxy (k :: *) (a :: k) = Proxy`. Is its kind `forall (k :: *). k -> *` or `foreach (k :: *). k -> *`. Let's consider the former type as if it classified a term-level function. That function would have to be a constant function, by parametricity. Yet, we do *not* want `Proxy * Bool` to be the same as `Proxy Nat Zero`. So, we choose the latter classifier.

This choice matters in how datatypes are used in pattern-matching situations. For example, is this possible: `type instance F (Proxy (a :: k)) = k`? The result uses `k` in a relevant context, so it must be that `k` is introduced in a relevant context, and thus that it must be `foreach`-quantified. If we wanted to introduce `forall`-quantification in datatypes, then the use of these variables would be restricted during matching. Is this a good idea? Is this necessary? And, it seems that `foreach` will be default in datatypes but `forall` will be default in type families. This is strange.

## Open design questions

### Parsing/namespace resolution

Parsing is a bit of a nightmare for this new language and will require some compromises.

- The type language and the term languages are more different. There are two proposals on the table to deal with types embedded in terms:

  - **Keep the parsers separate**: Since the parsers are separate and the parser certainly doesn't know anything about types, we need some indication in the code as a signal to the parser to switch to the type parser. Due to the construction of GHC's parser, this indicator would have to come *before* the phrase in question.

    - Option 1: We can use `@` to indicate that we are parsing a type. Take `id :: forall a -> a -> a`. This is just like the normal `id`, but with the type parameter explicit. Because the parser/renamer won't know the type of `id` when parsing its arguments, the first argument will have to manifestly be a type. For example, `id @Bool True`. The `@` indicates to the parser that the following thing is a *type*, not a *term*.
    - Option 2: We can use the keyword `type` to indicate that we are parsing a type.

    Both of these are somewhat painful, in that they require users to keep track of the difference between terms and types, something that really should be done away with in order to have an ergonomic dependently typed language.

  - **Merge the parsers**: It may be possible to merge the term/type parsers. This would make `forall` a proper keyword. `(->)` and `(=>)` are already unusable at the term level. `\` is already unusable at the type level. One possible conflict is that `'` is used in types to indicate namespace and it is used in terms to indicate Template Haskell quoting. In any case, this all seems at least possible to consider. 
    Even if we could write some kind of combined parser, the renamer would have major trouble distinguishing between data constructors and type constructors. 

    - Option 1: Use `'` to write data constructors in types and use `^` to write type constructors in terms. The first of these is already implemented. The second is up for debate. Do these operators work only on individual identifiers? Or, can we say `f ^(...)` to make everything in the `...` be treated like a type?
    - Option 2: Use `'` to mean "switch default" -- it goes in either direction.
    - Option 3: Discourage the use of identifiers that appear in both contexts, providing a module-based mechanism for disambiguation (much like we disambiguate other constructs). See [Proposal #270](https://github.com/ghc-proposals/ghc-proposals/pull/270).

- We will similarly need a syntax for type patterns embedded within term patterns. It would be ideal if the pattern syntax were identical to the expression syntax.

- Regardless of other choices above, simple cases might be able to remain simple. For example, `f Bool` will surely parse as a term. When the renamer can't find the data constructor `Bool`, it could be smart enough to look for a type constructor `Bool` and get on with it.

### Overriding visibility defaults

The `.`/`->` distinction in quantifiers allows programmers to specify the visibility of arguments at use sites. But, sometimes callers will want to override the defaults.

- If a visible, dependent argument is to be elided, we could allow `_` to indicate that GHC should use unification to fill in the argument. (This is similar to the approach in Coq, for example, among other languages.) Does this conflict in any way with typed holes? Perhaps a programmer wants to get an informative error message, not for GHC to plug in a value. See [Proposal #194](https://github.com/ghc-proposals/ghc-proposals/pull/194) for more discussion.

- Visible, non-dependent arguments cannot be inferred via unification, so `_` would not be applicable here, and would retain its current meaning of a typed hole.

- How to override an invisible, dependent type argument? This might be critical if a function call would be otherwise ambiguous. It should be marked with `@` -- simple.

# Type Inference


Figuring out type inference for this language is a bit of a challenge. While the full design won't be laid out here, interesting tidbits will be gathered here for easy retrieval.

### Inferring `foreach` types


Suppose a programmer writes, without a type signature

```wiki
foo @Zero y = y
```


What is `foo`'s type? It could be `foreach (n :: Nat). forall (a :: *). Vec a n -> Vec a Zero`. It could also be `forall (n :: Nat) (a :: *). a -> a`. Neither is more general than the other -- we are in the same GADT type-inference problem as described in the [OutsideIn](http://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/jfp-outsidein.pdf) paper. Thus, we reject such a `foo` that matches on an implicit parameter without a type signature.


But, what about

```wiki
foo Zero y = y
```


We are actually in the same situation here. But, backward compatibility compels us to prefer non-dependent types over dependent ones, inferring `foo :: forall (a :: *). Nat -> a -> a`. (Note that `foo :: forall (a :: *). foreach (n :: Nat) -> Vec a n -> Vec a Zero` is a valid but incomparable type that we could assign.)

When do `foreach`-types get inferred, if ever? Good question.

# Implementation

*Kind equalities* will be part of GHC 8. Merging commit: [67465497](https://github.com/ghc/ghc/commit/6746549772c5cc0ac66c0fce562f297f4d4b80a2). It was developed in [ Eisenberg's nokinds tree](https://github.com/goldfirere/ghc/tree/nokinds).

# Related work

**Readers:** Please add to these lists!


There are several published works very relevant to the design:

- [System FC with Explicit Kind Equality](https://www.cis.upenn.edu/~justhsu/docs/nokinds.pdf). Stephanie Weirich, Justin Hsu, and Richard A. Eisenberg. ICFP 2013.
- [Type Inference, Haskell, and Dependent Types](http://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf). Adam Gundry. PhD Thesis, 2013.
- Eisenberg's thesis: [https://github.com/goldfirere/thesis](https://github.com/goldfirere/thesis)


There are also many works addressing the use of dependent types in Haskell. Here is a selection:

- [Dependently typed programming with singletons](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf). Richard A. Eisenberg and Stephanie Weirich. Haskell Symposium 2012.
- [Hasochism: The Pleasure and Pain of Dependently Typed Haskell](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf). Sam Lindley and Conor McBride. Haskell Symposium 2013.
- [Promoting Functions to Type Families in Haskell](http://www.cis.upenn.edu/~eir/papers/2014/promotion/promotion.pdf). Richard A. Eisenberg and Jan Stolarek. Haskell Symposium 2014.
