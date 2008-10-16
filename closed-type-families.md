# Closed Type Synonym Families


Currently thinking about adding Closed Type Synonym Families to GHC.  Currently WIP.

```wiki
type family Foo a b c where
  Foo..
```


Overlapping is allowed here, as we can take the rewrites in top-bottom order 
