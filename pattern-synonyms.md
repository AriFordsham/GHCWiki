# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.

## Motivating example


Here is a simple representation of types

```wiki
    data Type = App String [Type]
```


Using this representations the arrow type looks like `App "->" [t1, t2]`.
Here are functions that collect all argument types of nested arrows and recognize the `Int` type:

```wiki
   collectArgs :: Type -> [Type]
   collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
   collectArgs _ = []

   isInt (App "Int" []) = True
   isInt _ = False
```


Matching on `App` directly is both hard to read and error prone to write.


The proposal is to introduce a way to give patterns names:

```wiki
   pattern Arrow t1 t2 = App "->" [t1, t2]
   pattern Int = App "Int" []
```


And now we can write

```wiki
   collectArgs :: Type -> [Type]
   collectArgs (Arrow t1 t2) = t1 : collectArgs t2
   collectArgs _ = []

   isInt Int = True
   isInt _ = False
```

## Pattern-only synonyms


The simplest form of pattern synonyms is the one from the examples above.  The grammar rule is:

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`=`*pat*

`pattern`*varid<sub>1</sub>**consym**varid<sub>2</sub>*`=`*pat*

- Each of the variables on the left hand side must occur exactly once on the right hand side 
- Pattern synonyms are not allowed to be recursive.  Cf. type synonyms.
- The semantics is simply expansion of the synonym.


Pattern synonyms can be exported and imported by prefixing the *conid* with the keyword `pattern`:

```wiki
   module Foo (pattern Arrow) where ...
```


This is required because pattern synonyms are in the namespace of constructors, so it's perfectly valid to have

```wiki
   data P = C
   pattern P = 42
```


You may also give a type signature for a pattern, but as with most other type signatures in Haskell it is optional:

`pattern`*conid*`::`*type*


E.g.

```wiki
   pattern Arrow :: Type -> Type -> Type
   pattern Arrow t1 t2 = App "->" [t1, t2]
```


Together with [ViewPatterns](view-patterns) we can now create patterns that look like regular patterns to match on existing (perhaps abstract) types in new ways:

```wiki
import qualified Data.Sequence as Seq

pattern Empty = (Seq.viewl -> Seq.EmptyL)
pattern x :< xs = (Seq.viewl -> x Seq.:< xs)
pattern xs :> x = (Seq.viewr -> xs Seq.:> x)
```

## Implicitly-bidirectional pattern synonyms


In cases where *pat* is in the intersection of the grammars for patterns and expressions (i.e. is valid both as an expression and a pattern), the pattern synonym is said to be bidirectional, and can be used in expression contexts as well.


For example, the following two are not bidirectional:

```wiki
   pattern ThirdElem x = _:_:x:_
   pattern Snd y = (x, y)
```


since the right-hand side is not a closed expression of {*x*} and {*y*} respectively.


In contrast, the pattern synonyms for *Arrow* and *Int* above are bidirectional, so you can e.g. write:

```wiki
   arrows :: [Type] -> Type -> Type
   arrows = flip $ foldr Arrow
```

## Explicitly-bidirectional pattern synonyms


What if you want to use `Plus1` from the earlier example in an expression?
It's clearly impossible since its expansion is a pattern that has no meaning as an expression.
Nevertheless, if we want to make what looks like a constructor for a type we will often want to use it in both patterns and expressions.
This is the rationale for the most complicated synonyms, the bidirectional ones.  They provide two expansions, one for patterns and one for expressions.

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`=`*pat*`where`*cfunlhs**rhs*


where *cfunlhs* is like *funlhs*, except that the functions symbol is a *conid* instead of a *varid*.


Example:

```wiki
   pattern Plus1 n = n1 | let n = n1-1, n >= 0 where
      Plus1 n = n + 1
```


The first part as is before and describes the expansion of the synonym in patterns. The second part describes the expansion in expressions.

```wiki
   fac 0 = 0
   fac (Plus1 n) = Plus1 n * fac n 
```

## Associated Patterns Synonyms


Just like data types and type synonyms can be part of a class declaration, it would be possible to have pattern synonyms as well.


Example:

```wiki
   class ListLike l where
      pattern Nil :: l a
      pattern Cons :: a -> l a -> a
      isNil :: l a -> Bool
      isNil Nil = True
      isNil (Cons _ _) = False
      append :: l a -> l a -> l a

   instance ListLike [] where
      pattern Nil = []
      pattern Cons x xs = x:xs
      append = (++)

   headOf :: (ListLike l) => l a -> Maybe a
   headOf Nil = Nothing
   headOf (Cons x _) = Just x
```


One could go one step further and leave out the `pattern` keyword to obtain *associated constructors*, which are required to be bidirectional. The capitalized identifier would indicate that a pattern synonym is being defined. For complicated cases one could resort to the `where` syntax (shown above).

## Typed Patterns Synonyms


So far patterns only had _syntactic_ meaning. In comparison Ωmega has _typed_ pattern synonyms, so they become first class values. (I am not suggesting this for Haskell, yet.)
