# Adding dependent types to Haskell


This page is to track design and implementation ideas around adding a form of dependent types to Haskell. This work will also fix bug [\#7961](https://gitlab.haskell.org//ghc/ghc/issues/7961). Richard Eisenberg (a.k.a. goldfire) is expecting to take on most (all?) of this work.

## Surface Language Design


It is possible to fix [\#7961](https://gitlab.haskell.org//ghc/ghc/issues/7961) without any surface language changes, as that bug addresses only lifting restrictions on promotion. There is a chance that this bugfix will enter HEAD without all of the other features below, but this writeup generally will not consider fixing [\#7961](https://gitlab.haskell.org//ghc/ghc/issues/7961) separate from adding dependent types.

### Quantifiers


As pointed out in the [Hasochism paper](dependent-haskell#), Haskell currently enjoys a confluence of design decisions. One says that compile-time arguments are elided in runtime code. For example, when calling `map :: (a -> b) -> [a] -> [b]`, the type instantiations for `a` and `b` are properly arguments to `map` (and are passed quite explicitly in Core), but these arguments are always elided in surface Haskell. As the levels are mixing, we may want to revisit this. Along similar lines, type arguments in Haskell are always erasable -- that is, instantiations for types are never kept at runtime. While this is generally a Good Thing and powers much of Haskell's efficiency, dependent typing relies on keeping *some* types around at runtime. Here, it is even more apparent that sometimes, we want to be able to pass in values for type arguments, especially if those values can be inspected at runtime.


Haskell currently has three quantifiers: `forall`, `->`, and `=>`, as classified in the following table:

<table><tr><th> Quantifier </th>
<th> Dependent? </th>
<th> Visible? </th>
<th> Required? </th>
<th> Relevant? 
</th></tr>
<tr><th>`forall`</th>
<th> yes </th>
<th> unification </th>
<th> FVs </th>
<th> no 
</th></tr>
<tr><th>`->`</th>
<th> no </th>
<th> yes </th>
<th> yes </th>
<th> yes 
</th></tr>
<tr><th>`=>`</th>
<th> no </th>
<th> solving </th>
<th> yes </th>
<th> yes 
</th></tr></table>

- *Dependent* means that the quantified thing (henceforth, *quantifiee*) can appear later in the type. This is clearly true for `forall`-quantified things and clearly not true for `->`-quantified things. (That is, if we have `Int -> Bool`, we can't mention the `Int` value after the `->`!)
- *Visibility* refers to whether or not the argument must appear at call sites in the program text. If something is not visible, the table lists how GHC is to fill in the missing bit at call sites.
- A *required* quantification is one that must textually appear in the type. Note that Haskell freely infers the type `a -> a` really to mean `forall a. a -> a`, by looking for free variables (abbreviated to FVs, above).
- *Relevance* refers to how the quantifiee can be used in the term that follows. (This is distinct from dependence, which says how the quantifiee can be used in the *type* that follows!) `forall`-quantifiees are not relevant. While they can textually appear in the term that follows, they appear only in irrelevant positions -- that is, in type annotations and type signatures. `->`- and `=>`-quantifiees, on the other hand, can be used freely.


Relevance is something of a squirrely issue. It is (RAE believes) closely related to parametricity, in that if `forall`-quantifiees were relevant, Haskell would lose the parametricity property. Another way to think about this is that parametric arguments are irrelevant and non-parametric arguments are relevant.

## Related work

**Readers:** Please add to these lists!


There are several published works very relevant to the design:

- [ System FC with Explicit Kind Equality](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds.pdf). Stephanie Weirich, Justin Hsu, and Richard A. Eisenberg. ICFP 2013.
- [ Type Inference, Haskell, and Dependent Types](https://personal.cis.strath.ac.uk/adam.gundry/thesis/thesis-2013-07-24.pdf). Adam Gundry. PhD Thesis, 2013.


There are also many works addressing the use of dependent types in Haskell. Here is a selection:

- [ Dependently typed programming with singletons](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf). Richard A. Eisenberg and Stephanie Weirich. Haskell Symposium 2012.
- [ Hasochism: The Pleasure and Pain of Dependently Typed Haskell](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf). Sam Lindley and Conor McBride. Haskell Symposium 2013.
