# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.

## Motivating example


Here is a simple representation of types

```wiki
    data Type = App String [Type]
```


Using this representations the arrow type looks like `App "->" [t1, t2]`.
Here are functions to collect are all argument types of nested arrows and recognizing the `Int` type:

```wiki
   collectArgs :: Type -> [Type]
   collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
   collectArgs _ = []

   isInt (App "Int" []) = True
   isInt _ = False
```


Matching on the arrow type is both hard to read and error prone to write.


The proposal is to introduce a way to give pattern names:

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


Furthermore, the pattern synonym can also be used in expressions, e.g.,

```wiki
arrows :: [Type] -> Type -> Type
arrows = foldr Arrow
```

## Simple pattern synonyms


The simplest form of pattern synonyms is the one from the examples above.  The grammar rule is simply:

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`=`*patexp*


where *patexp* is simply the intersection of the grammars for patterns and expression, i.e., those terms that are valid both as a pattern and as an expressions.
Each of the variables on the left hand side must occur exactly one on the right hand side, and these are the only variables that can be mention on the right hand side.  The semantics is simply given by expansions of the synonym.
Pattern synonyms are not allowed to be recursive.  Cf. type synonyms.


Pattern synonyms can be exported and imported by mentioning the *conid* in the export/import list.  Note that this suffers from the same constructor vs type confusion that already exists in `hiding` list, i.e., given the mention of a *conid* you cannot tell if it refers to a constructor or a type.


You may also give a type signature for a pattern, but as with most other type signatures in Haskell it is optional:

`pattern`*conid*`::`*type*


E.g.

```wiki
   pattern Arrow :: Type -> Type -> Type
   pattern Arrow t1 t2 = App "->" [t1, t2]
```

## Pattern only synonyms


The simple patterns synonyms are restricted to having a right hand side that is also a valid expression.
The pattern only synonyms can have any pattern on the right hand side, but may only be used in patterns.

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`~`*pat*


Note the use of `~` instead of `=` as the equality symbol.  This serves as a syntactic cue that this is a pattern only synonym.
Again, each of the variables on the left hand side must be mentioned exactly once of the right hand side, but the right hand side can mention other variables as well.  These variables will not be bound by using the pattern synonyms.


Examples:

```wiki
   pattern ThirdElem x ~ _:_:x:_
   pattern LazySecond a b ~ (a, ~b)

   third (ThirdElem a) = a
   third _ = error "No third"

   fcn :: (Int, (Int, Int))
   fcn (LazySecond x (y, z)) = if x == 0 then 0 else y+z
```


And their expansions

```wiki
   third (_:_:a:_) = a
   third _ = error "No third"

   fcn :: (Int, (Int, Int))
   fcn (x, ~(y, z)) = if x == 0 then 0 else y+z
```


Together with [ViewPatternsAlternative](view-patterns-alternative) we can now create patterns that look like regular patterns to match on existing (perhaps abstract) types in new ways.

```wiki
   pattern Plus1 n ~ n1 | let n = n1-1, n >= 0

   fac 0 = 0
   fac (Plus1 n) = (n+1) * fac n 
```


Note that the right hand side of `Plus1` binds `n1` and `n`, but since only `n` is mentioned on the left hand side it is the only variable that gets bound when `Plus1` is used.

## Bidirectional pattern synonyms


What if you want to use `Plus1` from the earlier example in an expression.
It's clearly impossible since its expansion is a pattern that has no meaning as an expression.
Nevertheless, if we want to make what looks like constructors for a type we will often want to use them in both patterns and expressions.
This is the rational for the most complicated synonyms, the bidirectional ones.  They provide two expansions, one for patterns and one for expressions.

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`~`*pat*`where`*cfunlhs**rhs*


where *cfunlhs* is like *funlhs*, except that the functions symbol is a *conid* instead of a *varid*.


Example:

```wiki
   pattern Plus1 n ~ n1 | let n = n1-1, n >= 0 where
      Plus1 n = n + 1
```


The first part as is before and describes the expansion of the synonym in patterns, whereas the second describes the expansion in expressions.

```wiki
   fac 0 = 0
   fac (Plus1 n) = Plus1 n * fac n 
```