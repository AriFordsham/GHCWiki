# Pattern-matching axioms


This page describes an extension to type families that supports overlap.

- See also the **[Discussion Page](new-axioms/discussion-page)** added May 2012, for comment/suggestions/requests for clarification/alternative solutions, to explore the design space.
- See also the **[Coincident Overlap](new-axioms/coincident-overlap)** page (added August 2012) for a discussion around the usefulness of allowing certain overlaps when the right-hand sides coincide.
- See also the **[Template Haskell](new-axioms/template-haskell)** page (added December 2012) for a proposal for the Template Haskell changes necessary to support this change.
- See also the **[Non-linearity](new-axioms/nonlinearity)** and **[Type Spaces](new-axioms/type-spaces)** pages (added May 2013) for discussion and a proposal around type unsoundness that can be caused by repeated variables on the left-hand side of an instance. The proposal on that page will likely be implemented and will then be copied here.


Status (Jan 12): the groundwork is done, in HEAD; mainly making `CoAxiom` a more fundamental data type.  Not yet started on the details.


Status (Aug 12): A working prototype implementation is in `overlapping-tyfams`.


Status (Dec 12): A working implementation has been pushed to HEAD.

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


A new version of axioms is now implemented. The formal treatment can be found in docs/core-spec/core-spec.pdf.


Here are the changes to source Haskell.

-  A `type instance` declaration can define multiple equations, not just one:

  ```wiki
  type instance where
    Equal a a = True
    Equal a b = False
  ```

- Patterns within a single `type instance` declaration (henceforth "branches") may overlap, and are matched top to bottom.

- A single type family may, as now, have multiple `type instance` declarations:

  ```wiki
  type family F a :: *

  type instance where
    F [Int] = Int
    F [a]   = Bool

  type instance where
    F (Int,b) = Char
    F (a,b)   = [Char]
  ```

- The instances for `F` may not overlap.  That is, there must be no type `t` such that `(F t)` matches more than one instance. This rule explicitly excludes overlaps among group members, even if the right-hand sides coincide (but see the [Coincident Overlap](new-axioms/coincident-overlap) page for discussion).

- The groups do not need to be exhaustive.   If there is no equation that matches, the call is stuck. (This is exactly as at present.)

- An error is issued when a later equation is matched by a former, making the later one inaccessible.

  ```wiki
  type instance where
    F (a,b)   = [Char]
    F (Int,b) = Char
  ```

  Here the second equation can never match.


For closed kinds (and maybe for open ones, but I can't unravel it), it seems possible to write a set of equations that will catch all possible cases but doesn't match the general case. This situation is currently (Dec 2012) undetected, because I (Richard, `eir` at `cis.upenn.edu`) am unconvinced I have a strong enough handle on the details. For example, what about `Any`?

- The equations do not need to share a common pattern:

  ```wiki
  type instance where
    F Int = Char
    F (a,b) = Int
  ```

- When matching a use of a type family against a branched instance, special care must be taken (by GHC) not to accidentally introduce incoherence. Consider the following example:

  ```wiki
  type instance where
    F Int = Bool
    F a   = Char
  ```

  and we try to simplify the type `F b`. The naive implementation would just simplify `F b` to `Char`, but this would be wrong. The problem is that `b` may later be unified with `Int`, meaning `F b` should simplify to `Bool`, not `Char`. So, the correct behavior is not to simplify `F b` at all; it is stuck for now. Note that the second equation above is not useless: we will still simplify, say, `F Double` to `Char`.


More formally, we only match a type against an equation in an instance group when no previous equation can *unify* against the type.

## Limitations


The implementation described above does not address all desired use cases. In particular, it does not work with associated types at all. (Using something like `type where` in a class definition is a parse error.) There's no set reason the approach couldn't be expanded to work with associated types, but it is not done yet. In particular, the FC extension will handle intra-module overlapping associated types without a change. The biggest reason not to add associated types into the mix is that it will be a confusing feature. Overlap among class instances is directed by specificity; overlap among family instances is ordered by the programmer. Users would likely expect the two to coincide, but they don't and can't, as it would not be type safe:


It seems that inter-module overlapping non-coincident associated types are a Bad Idea, but please add comments if you think otherwise and/or need such a feature. Why is it a Bad Idea? Because it would violate type safety: different modules with different visible instances could simplify type family applications to different ground types, perhaps concluding `True ~ False`, and the world would immediately cease to exist.


This last point doesn't apply to overlapping type class instances because type class instance selection compiles to a term-level thing (a dictionary). Using two different dictionaries for the same constraint in different places may be silly, but it won't end the world.
