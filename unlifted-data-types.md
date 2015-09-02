# Unlifted data types


This page describes the unlifted data types, i.e. algebraic data types which live in kind \# rather than kind \*.

## Motivation


Bob Harper [ has written](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/):

>
> Haskell suffers from a paucity of types.  It is not possible in Haskell to define the type of natural numbers, nor the type of lists of natural numbers (or lists of anything else), nor any other inductive type!


The reason, of course, is that whenever you write `data T = T`, you define a type with not one, but two inhabitants: `T` and bottom. In many settings, the laziness that may creep into a type is undesirable. We can see this in many domains in Haskell:

1. The `NFData` type class allows users to force data into normal form, where it has no more thunks inside. NFData is highly desirable for parallel Haskell: thunks can cause problem with parallel computation by shifting computation from one thread to another (for this reason, the `Par` Monad requires `NFData` on its API). And in general, users of Haskell often apply `NFData` in order to eliminate thunks which may be retaining data.

1. In the implementation of mutable variables, it is extremely useful to be able to assume that a pointer you have is to the honest closure that contains the mutable field, not an indirection to that closure. Without this guarantee, code which writes to this mutable variable has to first check if the thunk is fully evaluated before actually performing the memory write. For this reason, the `MutVar#` primitive (which is a GC'd object) lives in kind \#.

1. Haskell already has the ability to specify a field of a constructor as strict, forcing it to be evaluated when it is constructed. Strictness enables such optimizations as unpacking, and other benefits. Why not also make it possible to specify a type as strict as a whole?


Fortunately, we already have the kind \# for unlifted data types. So, the idea is to make it possible to define data types in kind \#.

## Implementation

1. Introduce syntax for the \# kind. Probably something like `Unlifted`, so we don't add anymore really special unary syntax (like star.)

1. Add `unliftedTypeKindTyConName` to the `tc_kind_var_app` to accept it from user

1. Don't require kind of data type to be `*`

1. (???)
