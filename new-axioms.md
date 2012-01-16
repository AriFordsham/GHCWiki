# Pattern-matching axioms

## Background


One might imagine that it would be a simple matter to have a type-level function

```wiki
type family Equal a b :: Bool
```


so that `(Equal t1 t2)` was `True` if `t1`=`t2` and `False` otherwise.  But it isn't.  You can do  it for a fixed collection of types thus:

```wiki
type instance Equal a a = True
type instance Equal Int Bool = False
type instance Equal Bool Int = False
```


but this obviously gets stupid as you add more types.  Nor can you write

```wiki
type instance Equal a a = True
type instance Equal a b = False
```


because System FC (rightly) prohibits overlapping family instances.  

## What to do about it


So the deficiency is in System FC, and it seems fundamental.  We've been working on an extension to System FC, with a corresponding source-language extension, that does allow overlapping type families, with care. You would write something like this:

```wiki
type instance where
  Equal a a = True
  Equal a b = False
```


This wiki page is a stub:

- See [ this Github repo](https://github.com/dreixel/New-axioms) for a Latex draft of the design
- Here is a [ cached pdf](https://docs.google.com/open?id=0B1pOVvPp4fVdOTdjZjU0YWYtYTA5Yy00NmFkLTkxMWUtZmI0NmNhZTQwYzVl) of the current state
- We'll use GHC branch `ghc-axioms` for development work.


Status (Jan 12): the groundwork is done, in HEAD; mainly making `CoAxiom` a more fundamental data type.  Not yet started on the details.
