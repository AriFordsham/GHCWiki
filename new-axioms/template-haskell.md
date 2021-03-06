# Template Haskell changes to support overlapping type family instances


The current form of a type family instance in Template Haskell is this constructor of the `Dec` type:

```wiki
TySynInstD Name [Type] Type
```


Here is the implemented new constructor:

```wiki
TySynInstD Name TySynEqn
```


where

```wiki
data TySynEqn = TySynEqn [Type] Type
```


represents one equation with a list of left-hand side patterns and a single right-hand side result.


We also add a new declaration form to the `Dec` type

```wiki
ClosedTypeFamilyD Name [TyVarBndr] (Maybe Kind) [TySynEqn]
```


with a straightforward meaning.
