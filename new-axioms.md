# Pattern-matching axioms

## Background


One might imagine that it would be a simple matter to have a type-level function

```wiki
type family Equal a b :: Bool
```


so that `(Equal t1 t2)` was `True` if `t1`=`t2` and `False` otherwise.  But it isn't.  


You can't write

```wiki
type instance Equal a a = True
type instance Equal a b = False
```


because System FC (rightly) prohibits overlapping family instances.  


Expanding this out, you can do it for a fixed collection of types thus:

```wiki
type instance Equal Int Int = True
type instance Equal Bool Bool = True
type instance Equal Int Bool = False
type instance Equal Bool Int = False
```


but this obviously gets stupid as you add more types.  


Furthermore, this is not what you want. Even if we restrict the equality function to booleans

```wiki
type family Equal (a :: Bool) (b :: Bool) :: Bool
```


we can't define instances of Equal so that a constraint like this one

```wiki
Equal a a ~ True
```


is satisfiable---the type instances only reduce if a is known to True or False. GHC doesn't reason by cases.  (Nor should it, \|Any\| also inhabits \|Bool\|. No kinds really are closed.)


The only way to work with this sort of reasoning is to use Overlapping Instances, as suggested in the [ HList paper.](http://homepages.cwi.nl/~ralf/HList/)

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

**[Discussion Page](new-axioms/discussion-page)** added May 2012, for comment/suggestions/requests for clarification/alternative solutions, to explore the design space.

- We'll need some concrete syntax for the discussion, so we'll follow the cached pdf, but note that the syntax there is not final.
