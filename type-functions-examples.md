# Examples of Type Functions


The map example from *Associated Types with Class* in the new form:

```wiki
data family Map k :: * -> *

data instance Map ()     v = MapUnit (Maybe v)
data instance Map (a, b) v = MapPair (Map a (Map b v))
```


We can define operations on indexed maps using a type class whose instances corresponds to the type indexes.  Note that a declaration, such as

```wiki
data instance Map Int Char = Nonsense
```


is not acceptable, as it constraints the second argument of `Map`, which is not a type index, but a parametric argument.
