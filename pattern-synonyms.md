# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.

## Motivating example


Here is a simple representation of types

```wiki
    data Type = App String [Type]
```


Using this representations the arrow type looks like `App "->" [t1, t2]`.
Here is a function to collect are all argument types of nested arrow:

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