# Kinds


Kinds are represented as types:

```wiki
type Kind = Type
```


Basic kinds are now
represented using type constructors, e.g. the kind `*` is represented as

```wiki
liftedTypeKind :: Kind
liftedTypeKind = TyConApp liftedTypeKindTyCon []
```


where `liftedTypeKindTyCon` is a built-in `PrimTyCon`.  The arrow type
constructor is used as the arrow kind constructor, e.g. the kind `* -> *` 
is represented internally as

```wiki
FunTy liftedTypeKind liftedTypeKind
```


It's easy to extract the kind of a type, or the sort of a kind:

```wiki
typeKind :: Type -> Kind
```


The "sort" of a kind is always one of the
sorts: `TY` (for kinds that classify normal types) or `CO` (for kinds that
classify coercion evidence).  The coercion kind, `T1 :=: T2`, is
represented by `PredTy (EqPred T1 T2)`.

### Kind subtyping

[](https://docs.google.com/drawings/pub?id=1M5yBP8iAWTgqdI3oG1UNnYihVlipnvvk2vLInAFxtNM&w=359&h=229)


(You can edit this picture [ here](https://docs.google.com/drawings/d/1M5yBP8iAWTgqdI3oG1UNnYihVlipnvvk2vLInAFxtNM/edit?hl=en_GB).)

- "`*`" is the kind of boxed values. Things like `Int` and `Maybe Float` have kind `*`.

- "`#`" is the kind of unboxed values. Things like `Int#` have kind `#`.

- "`(#)`" is the kind of unboxed tuples. Things like `(# Int, Int #)` have kind `(#)`.

- "`ArgKind`" is the kind of things that can appear as arguments to functions.

- "`OpenKind`" is the kind of things that can appear as results of functions.
