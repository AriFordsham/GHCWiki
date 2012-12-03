# Template Haskell changes to support overlapping type family instances


The current form of a type family instance in Template Haskell is this constructor of the `Dec` type:

```wiki
TySynInstD Name [Type] Type
```


Here is the proposed new constructor:

```wiki
TySynInstD Name [TySynEqn]
```


where

```wiki
data TySynEqn = TySynEqn [Type] Type
```


represents one equation with a list of left-hand side patterns and a single right-hand side result. The only alternate design on the table is to use a list of (\[Type\], Type) pairs instead of an extra datatype.
