
This is a proposal for allowing **families of patterns** indexed by expressions.

## Syntax


The syntax from [PatternSynonyms](pattern-synonyms) can be reused for simplicity:

```wiki
pattern Take n xs <- (take n -> xs)
```


here `xs` is a normal variable as in [PatternSynonyms](pattern-synonyms) but `n` is the expression the pattern is indexed by: this can be inferred from it appearing in the [ViewPatterns](view-patterns) expression (`take n`) rather than in the pattern.

`Take 0`, `Take 1`, `Take 2` would be equivalent to the following pattern definitions:

```wiki
pattern Take0 xs <- (take 0 -> xs)
pattern Take1 xs <- (take 1 -> xs)
pattern Take2 xs <- (take 2 -> xs)
...
```

### Grammar


A simple grammar would then be

> `pattern`*conid**varid<sub>1</sub>* .. *varid<sub>n</sub>*`<-` (*expr*`->`*pat*)


where each *varid<sub>i</sub>* must be free in either *expr* or *pat*.

## Dynamic semantics (Desugaring)


More concretely, a pattern definition has the form:

> `pattern`*conid* \[*evarid<sub>i</sub>*, *pvarid<sub>j</sub>*\] `<-` (*expr*`->`*pat*)


where \[*evarid<sub>i</sub>*, *pvarid<sub>j</sub>*\] denotes some interleaving of variables that appear in *expr* or *pat*. When matched against, all *evarid<sub>i</sub>* must be instantiated with expresions *expr<sub>i</sub>*:

> `fn` (*conid* \[*expr<sub>i</sub>*, *pvarid<sub>j</sub>*\]) = `result`


where *pvarid<sub>j</sub>* may appear in `result` as in usual patterns. This then gets translated into the following view pattern:

> `fn` (*expr*\[*evarid<sub>i</sub>* := *expr<sub>i</sub>*\] `->``pat`) = `result`


For the simple example of the pattern family `Take` this (where 3 is *expr<sub>1</sub>* and `xs` is *pval<sub>1</sub>*):

```wiki
    foo (Take 3 xs) = xs ++ xs
```


would get translated to:

```wiki
    foo (take 3 -> xs) = xs ++ xs
```


which is the same as:

```wiki
    foo ys = case take 3 ys of
      xs -> xs ++ xs
```

## Static semantics (Typing)


If *expr* in the view pattern is an expression of type *t* with free variables *evarid<sub>i</sub>* of type *t<sub>i</sub>* then an *expr<sub>i</sub>* used to instantiate the corresponding *evarid<sub>i</sub>* must have a type *u<sub>i</sub>* that unifies with *t<sub>i</sub>*, the final expression *expr* will have type *t*. Otherwise same typing and scoping rules as [ ViewPatterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Semantics).

## Motivating examples

### Sets


Example from [ViewPatternsAlternative](view-patterns-alternative):

```wiki
module Set(Set, empty, insert, delete, has) where

    newtype Set a = S [a]
  
    has :: Eq a => a -> Set a -> Maybe (Set a)
    has x (S xs) | x `elem` xs = Just (S (delete x xs))
                 | otherwise   = Nothing
```


Using patterns indexed by an element of `Set a`:

```wiki
    pattern Has    x set <- (has x        -> Just set)
    pattern HasNot x set <- (has x &&& id -> (Nothing, set))
```


One can write:

```wiki
    delete :: Eq a => a -> Set a -> Set a
    delete x (Has x set) = set
    delete x set         = set

    insert :: Eq a => a -> Set a -> Set a
    insert x (HasNot x (S xs)) = S (x:xs)
    insert x set               = set
```


Compare that to the the [ViewPatternsAlternative](view-patterns-alternative) proposal:

```wiki
    delete :: Eq a => a -> Set a -> Set a
    delete x (r | Just s <- has r) = set
    delete x set                   = set
  
    insert :: Eq a => a -> Set a -> Set a
    insert x (s | Just _ <- has x s) = set
    insert x (S xs)                  = S (x:xs)
```


Using operators `:∈ = Has` and `:∈ = HasNot` one could write:

```wiki
    delete x (x :∈ set)  = set
    insert x (x :∉ S xs) = S (x:xs)
```


if one were so inclined.

### Type checking


From [ Bidirectional Typing Rules: A Tutorial](http://itu.dk/people/drc/tutorials/bidirectional.pdf):

```wiki
    inferType ctx (If t1 t2 t3) = case (inferType ctx t1, inferType ctx t2, inferType ctx t3) of
      (Just BoolT, Just ty2, Just ty3) -> 
        if ty2 = ty3 
        then Just ty2
        else Nothing
      _ -> Nothing
```


could be rewritten using pattern families as:

```wiki
    -- Here ‘Inf’ is a family indexed by ‘ctx’
    pattern Inf ctx ty <- (inferType ctx -> Just ty)

    inferType ctx (If (Inf ctx BoolT) (Inf ctx ty1) (Inf ctx ty2))
       | ty1 == ty2 = Just ty1
    inferType ctx If{} = Nothing
```


allowing the user to pattern match *directly* on the inferable types without manually checking for `Just`s — note the use of the previous argument `ctx` to index later. This could currently be written somewhat awkwardly using view patterns:

```wiki
    inferType ctx (If (inferType ctx -> Just BoolT) (inferType ctx -> Just ty1) (inferType ctx -> Just ty2))
       | ty1 == ty2 = Just ty1
    inferType ctx If{} = Nothing
```


which is longer and clunkier, especially since the user is forced to deal with `Just`s again.


Again one could use operators (`:⇒ = Inf`) in which case it the examples follow notation in type theory more closely:

```wiki
    inferType γ (If (γ :⇒ BoolT) (γ :⇒ τ₁) (γ :⇒ τ₂)) = ...
```

### More advanced examples: Regular expressions


Given a regular expression operator `(~=) :: String -> String -> Maybe [String]` we can define a pattern:

```wiki
    pattern Match x regexp <- ((~= regexp) -> Just x)
```


where `regexp` is indexes the `Match` pattern family:

```wiki
    firstWord (Match words "[a-zA-Z]+") = words
    firstWord _                         = error "No words found"
```


or

```wiki
    vowels (Match vwls "[aeiou]") = length vwls
```


As an operator:

```wiki
    pattern x :~= regexp ((~= regexp) -> Just x)
```

### More advanced examples: Prisms


...
